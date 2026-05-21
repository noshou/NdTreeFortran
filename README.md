# NdTreeFortran

A thread-safe N-dimensional tree library in modern Fortran. Provides two concrete tree types  a balanced kd-tree (`KdTree`) and a balanced ball-tree (`BallTree`)  both with radius nearest-neighbour search, id-based lookup, physical node removal, and DBSCAN clustering. The `NdTree` abstract base type holds the shared pool infrastructure and is the scaffold for future tree types (OcTree, QuadTree).

All node counts, indices, and capacities are `integer(int64)` end to end, so a tree can theoretically hold up to `int64` nodes. The `int64` kind is re-exported from the module, so `use NdTreeFortran` brings it into scope for writing literals such as `bufferSize=1000_int64`.

> **v0.7.1** introduces `BallTree` (with selectable distance metric) and widens the entire public integer API to `integer(int64)`.

---

## Quick start

```fortran
use NdTreeFortran
use iso_fortran_env, only: real64

type(KdTree) :: t
real(real64) :: pts(3, 4) = reshape([...], [3, 4])   ! 4 points in 3D

call t%build(pts)
```

Or a ball-tree, optionally choosing the distance metric before building:

```fortran
type(BallTree) :: b

call b%setMetric('manhattan')   ! optional; defaults to 'euclidean'
call b%build(pts)
```

Both types share the same search, getter, and mutation API documented below; the only difference is `BallTree`'s metric is fixed at build time (see [`BallTree`](#balltree)).

---

## Public types

### `NodeId`

A 128-bit composite identifier returned by `getNodeId()` and `getAllNodeIds()`. `node_id` is stable for the lifetime of the node. `pool_idx` is a mutable hint to the node's current pool position; it may go stale after `addNodes` or `rmvNodes`.

```fortran
type, bind(c) :: NodeId
    integer(c_int64_t) :: node_id  = 0   ! stable, globally unique per tree
    integer(c_int64_t) :: pool_idx = 0   ! positional hint; may be stale
end type NodeId
```

Compare by `node_id` only:

```fortran
if (a%node_id == b%node_id) ...
```

### `NdNode`

A single tree node. Not directly constructable by the caller. Accessed via `NdNodePtr%p` after a search. Exposes:

- `getCoords()`  `real(real64)(:)` coordinates
- `getData()`  polymorphic payload (if set at build time)
- `getNodeId()`  `type(NodeId)`

### `NdNodePtr`

Owned pointer to a deep-copied `NdNode`. Freed automatically when it goes out of scope (via `final`). Call `%destroy()` explicitly if you need early release.

```fortran
type(NdNodePtr), allocatable :: res(:)
res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.0_real64)
! res(1)%p points to a heap-allocated copy
call res(1)%destroy()   ! optional; finalizer runs on deallocation anyway
```

### `NdNodeBucket`

Container for the `NdNodePtr` array returned by multi-query searches and DBSCAN. `%nodes` is a zero-length array when the query has no match.

```fortran
type :: NdNodeBucket
    type(NdNodePtr), allocatable :: nodes(:)
end type NdNodeBucket
```

### `NdTree` (abstract base)

The shared pool infrastructure, all search functions, and all getters are defined on this abstract type. `KdTree` and `BallTree` both extend `NdTree`. Users declare `type(KdTree)` or `type(BallTree)`  `NdTree` is the scaffold for future tree types.

### `KdTree`

The concrete kd-tree type. Extends `NdTree` and implements the balanced kd-tree build, rNN search, addNodes, and rmvNodes. Also exposes:

- `getSplitAxis(node)`  `integer(int64)`, the split dimension of a given node
- `printTree([unit])`  prints the tree in pre-order; see format below

### `BallTree`

The concrete ball-tree type. Extends `NdTree` and implements a balanced ball-tree build, rNN search, addNodes, and rmvNodes. Unlike `KdTree`, a `BallTree` carries a single distance metric chosen at build time and used for both construction and every search. Exposes:

- `setMetric(metric)`  sets the metric (`'euclidean'`, `'manhattan'`, `'chebyshev'`); must be called **before** `build` and `error stop`s if the tree is already initialized. Defaults to `'euclidean'` if never called.
- `getMetric()`  `character(len=9)`, the tree's metric, or `'not init'` if not built
- `getBallRadius(node)`  `real(real64)`, the bounding-ball radius of a node (leaves are `0`); `error stop`s on an uninitialized tree
- `printTree([unit])`  prints the tree in pre-order with `[r=<radius>]` prefixes; see format below

Because the metric is fixed per tree, passing a `metric` argument to a `BallTree` search that disagrees with the tree's metric is an error.

---

## Tree lifecycle

### `build(coordsList, dataList, rebuildRatio)`

Builds the tree from `coordsList`, a `real(real64)` array of shape `[dim, N]`. Rejects double-build without an intervening `destroy`. For a `BallTree`, call `setMetric` first if you want a non-default metric.

```fortran
real(real64) :: pts(2, 5) = reshape([...], [2, 5])
call t%build(pts)

! With polymorphic payload
integer :: labels(5) = [10, 20, 30, 40, 50]
call t%build(pts, labels)

! With custom rebuild ratio
call t%build(pts, rebuildRatio=0.5_real64)
```


| Parameter      | Type                | Default | Notes                                 |
| -------------- | ------------------- | ------- | ------------------------------------- |
| `coordsList`   | `real(real64)(:,:)` | n/a     | shape `[dim, N]`                      |
| `dataList`     | `class(*)(:)`       | absent  | polymorphic; length must equal N or 0 |
| `rebuildRatio` | `real(real64)`      | `0.25`  | rebuild threshold; must be in (0, 1)  |


### `destroy()`

Frees all internal state. The tree can be rebuilt after destruction.

### `addNodes(coordsList, dataList)`

Appends N nodes to an initialized tree. When `modifications + N > rebuildRatio * pop`, a full rebalance runs; otherwise nodes are inserted at leaves.

```fortran
real(real64) :: extra(2, 3) = reshape([...], [2, 3])
call t%addNodes(extra)
```

### `rmvNodes(coordsList, radii, ids, epsilon, metric, bufferSize)` -> `integer(int64)`

Physically removes nodes and rebuilds the tree. Returns the number of nodes removed.

Five dispatch branches:


| Arguments                      | Semantics                                                 |
| ------------------------------ | --------------------------------------------------------- |
| `coordsList` only              | removes all within `epsilon` of each query point          |
| `ids` only                     | removes by `NodeId`; O(n x size(ids))                     |
| `coordsList` + `ids`           | paired: removes node at `coords(:,i)` matching `ids(i)`   |
| `coordsList` + `radii`         | removes all within `radii(i)` of `coords(:,i)`            |
| `coordsList` + `radii` + `ids` | spatial search then filters to nodes whose id is in `ids` |


```fortran
type(NodeId) :: target(1)
target(1) = someNode%p%getNodeId()
n = t%rmvNodes(ids=target)

! By coordinate
real(real64) :: q(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
n = t%rmvNodes(coordsList=q)

! By radius
real(real64) :: qs(2, 2) = reshape([0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64], [2, 2])
real(real64) :: rs(2)    = [0.5_real64, 1.0_real64]
n = t%rmvNodes(coordsList=qs, radii=rs)
```


| Parameter    | Type                | Default       | Notes                                                          |
| ------------ | ------------------- | ------------- | -------------------------------------------------------------- |
| `coordsList` | `real(real64)(:,:)` | absent        | required unless `ids` only                                     |
| `radii`      | `real(real64)(:)`   | absent        | paired with `coordsList`; length must match                    |
| `ids`        | `type(NodeId)(:)`   | absent        | unordered set; obtain via `getNodeId()`                        |
| `epsilon`    | `real(real64)`      | `1e-15`       | coord-match tolerance when `radii` is absent                   |
| `metric`     | `character(*)`      | `'euclidean'` | valid metrics: `'euclidean'`, `'manhattan'`, `'chebyshev'`     |
| `bufferSize` | `integer(int64)`    | `1000`        | initial rNN buffer capacity; must be > 0                       |


---

## Search functions

All search functions are read-only and safe to call concurrently from multiple threads.

### `rNN_Centroid(centroid, radius, metric, bufferSize)` -> `NdNodePtr(:)`

Finds all nodes within `radius` of a single coordinate point. The query point need not exist in the tree.

```fortran
type(NdNodePtr), allocatable :: res(:)
res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64)
res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='manhattan')
```

### `rNN_Node(target, radius, bufferSize, metric, excludeTarget)` -> `NdNodePtr(:)`

Finds all nodes within `radius` of an existing tree node. Pass `excludeTarget=.true.` to omit the query node itself.

```fortran
type(NdNodePtr), allocatable :: centre(:), neighbours(:)

centre     = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
neighbours = t%rNN_Node(centre(1), 1.5_real64, excludeTarget=.true.)
```

### `rNN_Coords(coords, metric, epsilon, bufferSize)` -> `NdNodeBucket(:)`

Batch coordinate search. `coords` is `[dim, nQuery]`; returns a parallel array of `NdNodeBucket`.

```fortran
real(real64) :: q(2, 3) = reshape([...], [2, 3])
type(NdNodeBucket), allocatable :: res(:)

res = t%rNN_Coords(q, epsilon=0.5_real64)
! res(i)%nodes contains all matches for query i
```


| Parameter    | Type                | Default       | Notes                                       |
| ------------ | ------------------- | ------------- | ------------------------------------------- |
| `coords`     | `real(real64)(:,:)` | n/a           | shape `[dim, nQuery]`                       |
| `metric`     | `character(*)`      | `'euclidean'` | `'euclidean'`, `'manhattan'`, `'chebyshev'` |
| `epsilon`    | `real(real64)`      | `1e-15`       | search radius; must be >= 0                 |
| `bufferSize` | `integer(int64)`    | `1000`        | initial buffer; must be > 0                 |


### `rNN_Ids(coords, ids, metric, epsilon, bufferSize)` -> `NdNodeBucket(:)`

Like `rNN_Coords`, but `ids(i)` is an additional filter for query `i`. Returns a node only if it is within `epsilon` of `coords(:,i)` and its `node_id` equals `ids(i)%node_id`. `ids` is paired with `coords` column-by-column.

```fortran
type(NodeId) :: target_ids(2)
target_ids(1) = nodeA%p%getNodeId()
target_ids(2) = nodeB%p%getNodeId()
res = t%rNN_Ids(q, target_ids, epsilon=1e-10_real64)
```

### `rNN_Rad(coords, radii, metric, bufferSize)` -> `NdNodeBucket(:)`

Per-query variable-radius search. `radii(i)` is the search radius for `coords(:,i)`.

```fortran
real(real64) :: q(2, 2) = reshape([0.0_real64, 0.0_real64, 5.0_real64, 5.0_real64], [2, 2])
real(real64) :: r(2)    = [1.5_real64, 3.0_real64]

res = t%rNN_Rad(q, r)
```

### `rNN_RadIds(coords, radii, ids, metric, bufferSize)` -> `NdNodeBucket(:)`

Per-query variable-radius search, then filters to nodes whose `node_id` appears anywhere in `ids`. `ids` is an unordered set, not paired with `coords`.

```fortran
type(NodeId) :: wanted(2)
wanted(1) = nodeA%p%getNodeId()
wanted(2) = nodeB%p%getNodeId()
res = t%rNN_RadIds(q, r, wanted)
! res(i)%nodes: nodes within r(i) of q(:,i) that appear in wanted
```

### `linScan(ids)` -> `NdNodePtr(:)`

Looks up nodes by `NodeId`. Uses `pool_idx` as an O(1) hint; falls back to O(n) scan only when the hint is stale (e.g. after `rmvNodes`). Returns a zero-length array when no match exists.

```fortran
type(NodeId)                 :: ids(2)
type(NdNodePtr), allocatable :: res(:)

ids(1) = nodeA%p%getNodeId()
ids(2) = nodeB%p%getNodeId()
res = t%linScan(ids)
```

Prefer coordinate-based search when you know where a node is. Use `linScan` when you have `NodeId` values but no coordinates.

### `DBSCAN(minPts, radius, metric, bufferSize)` -> `NdNodeBucket(:)`

Density-based spatial clustering. Returns `res(1:nClusters+1)`: each `res(i)` for `i <= nClusters` holds one cluster, and `res(nClusters+1)` holds all noise nodes. Returns a zero-length array when the tree is empty.

```fortran
type(NdNodeBucket), allocatable :: res(:)
integer :: nClusters, noiseCount, i

res        = t%DBSCAN(minPts=3_int64, radius=0.5_real64)
nClusters  = size(res) - 1
noiseCount = size(res(size(res))%nodes)

do i = 1, nClusters
    print *, 'cluster', i, 'has', size(res(i)%nodes), 'nodes'
end do
print *, 'noise nodes:', noiseCount
```


| Parameter    | Type           | Default       | Notes                                       |
| ------------ | -------------- | ------------- | ------------------------------------------- |
| `minPts`     | `integer(int64)` | n/a           | minimum neighbourhood size for a core point |
| `radius`     | `real(real64)`   | n/a           | neighbourhood search radius (epsilon)       |
| `metric`     | `character(*)`   | `'euclidean'` | `'euclidean'`, `'manhattan'`, `'chebyshev'` |
| `bufferSize` | `integer(int64)` | `1000`        | initial rNN buffer capacity; must be > 0    |


**Error guards** (`error stop`): uninitialized tree; `minPts < 0`; `radius < 0`; unknown metric; `bufferSize <= 0`.

**Border points:** a point first visited as noise that is later reached as a seed by a core point is correctly reassigned to the cluster.

---

## Printing

### `printTree([unit])`

Prints the tree in pre-order (root, left subtree, right subtree). Each line is indented two spaces per depth level. The per-node prefix is tree-specific: `KdTree` prints `[axis=N]` (the split dimension), `BallTree` prints `[r=<radius>]` (the bounding-ball radius).

```fortran
! 3 points in 2D: (1,2), (3,1), (5,4)
call t%printTree()
```

`KdTree` output:
```
[axis=1] (3.000, 1.000)
  [axis=2] (1.000, 2.000)
  [axis=2] (5.000, 4.000)
```

`BallTree` output (radii depend on geometry):
```
[r=2.828] (3.000, 1.000)
  [r=0.000] (1.000, 2.000)
  [r=0.000] (5.000, 4.000)
```

`unit` defaults to stdout. Pass any open Fortran unit to redirect output.

### `assert(testName, expected)`

Compares `printTree` output against an expected node set, matching as an unordered multiset of coordinate tuples (the per-node prefix and indentation are stripped). Use in tests to verify tree shape independent of tie-breaking order.

```fortran
call t%assert('my_test', [ character(len=32) :: &
    '[axis=1] (3.000, 1.000)',                   &
    '[axis=2] (1.000, 2.000)',                   &
    '[axis=2] (5.000, 4.000)'                    &
])
```

Prints a diagnostic and calls `stop 1` on mismatch.

---

## Getters

### Tree state


| Function                    | Returns          | Notes                                             |
| --------------------------- | ---------------- | ------------------------------------------------- |
| `getPop()`                  | `integer(int64)` | number of live nodes                              |
| `getDim()`                  | `integer(int64)` | coordinate dimension                              |
| `getInitState(isInit)`      | via argument     | `.true.` after `build`, `.false.` after `destroy` |
| `getTreeId()`               | `integer(int64)` | unique id per `build` call                        |
| `getNumMods()`              | `integer(int64)` | insertions since last rebuild; 0 after rebuild    |
| `getRebuildRatio()`         | `real(real64)`   | current rebuild threshold                         |
| `setRebuildRatio(ratio)`    |                 | `ratio` must be in (0, 1)                         |
| `associatedNodePool(assoc)` | via argument     | `.true.` when pool is allocated                   |
| `associatedRoot(assoc)`     | via argument     | `.true.` when root index is nonzero               |


### Bulk node retrieval


| Function          | Returns               | Notes                                                              |
| ----------------- | --------------------- | ------------------------------------------------------------------ |
| `getAllNodes()`   | `NdNodePtr(:)`        | deep-copied array, length == pop; pool_idx pre-stamped             |
| `getAllCoords()`  | `real(real64)(:,:)`   | shape `[dim, pop]`; column i is pool position i                    |
| `getAllNodeIds()` | `type(NodeId)(:)`     | length pop; pool_idx accurate at call time                         |


### Node accessors

On an `NdNode` accessed via `NdNodePtr%p`:


| Method        | Returns           | Notes                                  |
| ------------- | ----------------- | -------------------------------------- |
| `getNodeId()` | `type(NodeId)`    | stable `node_id`; `pool_idx` at search |
| `getCoords()` | `real(real64)(:)` | copy of node coordinates               |
| `getData()`   | `class(*)`        | polymorphic payload; set at build time |


### `getSplitAxis(node)` -> `integer(int64)`

Returns the splitting dimension for a given node in the kd-tree. Called on the tree, not the node:

```fortran
type(NdNodePtr), allocatable :: res(:)
integer(int64)               :: axis

res  = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.0_real64)
axis = t%getSplitAxis(res(1)%p)   ! 1-based dimension index
```

### `isMember(target)` -> `logical`

Returns `.true.` if `target` belongs to this tree instance and has not been removed. Uses `pool_idx` from the node's `NodeId` as an O(1) hint; falls back to a full O(n) pool scan when the hint is stale (e.g. after `rmvNodes` compacted the pool).

```fortran
type(NdNodePtr), allocatable :: res(:)
res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.0_real64)
print *, t%isMember(res(1))   ! .true.
```

---

## Thread safety


| Operation                                                                        | Concurrent-safe?                                                  |
| -------------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| Any `rNN_*`, `linScan`, `DBSCAN`, `getAllNodes`, `getAllCoords`, `getAllNodeIds` | Yes  read-only, no locking                                       |
| `addNodes` from multiple threads on one tree                                     | Yes  serialized via `!$OMP CRITICAL (tree_mutate)`               |
| `rmvNodes` from multiple threads on one tree                                     | Yes  search is read-only; compaction and rebuild are serialized  |
| `rmvNodes` concurrent with `addNodes`                                            | Yes  same critical region                                        |
| Same node removed by N threads concurrently                                      | Yes  keepMask re-checked inside critical; only 1 thread removes  |
| `build` concurrent with `addNodes`/`rmvNodes`                                    | No  `build` is not guarded                                       |
| `destroy` concurrent with anything                                               | No                                                                |


---

## Use case examples

### Nearest-neighbour graph construction

```fortran
type(KdTree)                 :: t
type(NdNodePtr), allocatable :: allNodes(:), neighbours(:)
real(real64)                 :: r = 1.5_real64
integer                      :: i

call t%build(pts)
allNodes = t%getAllNodes()

do i = 1, size(allNodes)
    neighbours = t%rNN_Node(allNodes(i), r, excludeTarget=.true.)
    ! neighbours(:)%p -- the adjacent nodes to allNodes(i)
end do
```

### DBSCAN then working with individual clusters

```fortran
type(NdNodeBucket), allocatable :: res(:)
integer :: nClusters, i, j

res       = t%DBSCAN(minPts=4_int64, radius=0.5_real64)
nClusters = size(res) - 1

do i = 1, nClusters
    do j = 1, size(res(i)%nodes)
        print *, res(i)%nodes(j)%p%getCoords()
    end do
end do

! Noise bucket is always last
do j = 1, size(res(nClusters+1)%nodes)
    print *, 'noise:', res(nClusters+1)%nodes(j)%p%getCoords()
end do
```

### Track node identity across removals

```fortran
type(KdTree)                    :: t
type(NdNodePtr),    allocatable :: found(:)
type(NodeId)                    :: id(1)
type(NdNodeBucket), allocatable :: res(:)
integer(int64)                         :: numRmv

call t%build(pts)

found  = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
id(1)  = found(1)%p%getNodeId()

res = t%rNN_RadIds(q, r, id)
if (size(res(1)%nodes) > 0) then
    numRmv = t%rmvNodes(ids=id)
end if
```

### Concurrent DBSCAN

```fortran
type(KdTree)                    :: t
type(NdNodeBucket), allocatable :: res(:)

call t%build(pts)

!$OMP PARALLEL DEFAULT(NONE) SHARED(t) NUM_THREADS(4) PRIVATE(res)
res = t%DBSCAN(minPts=3_int64, radius=0.5_real64)
! Each thread gets its own result copy; tree is read-only
!$OMP END PARALLEL
```

### Population-invariant DBSCAN result

DBSCAN results do not depend on tree population when only noise is added or removed outside cluster boundaries:

```fortran
integer :: n1, n2

res = t%DBSCAN(minPts=3_int64, radius=0.5_real64)
n1  = size(res) - 1   ! nClusters

call t%addNodes(distant_pts)   ! points too far to join any cluster
res = t%DBSCAN(minPts=3_int64, radius=0.5_real64)
n2  = size(res) - 1

! n1 == n2; cluster membership is unchanged
```

---

## Default constants


| Constant              | Value         | Type               |
| --------------------- | ------------- | ------------------ |
| `DEFAULT_BUFFER_SIZE` | `1000`        | `integer(int64)`   |
| `DEFAULT_METRIC`      | `'euclidean'` | `character(len=9)` |
| `DEFAULT_EPSILON`     | `1e-15`       | `real(real64)`     |
