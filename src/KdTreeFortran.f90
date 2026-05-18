module KdTreeFortran
    
    use iso_fortran_env, only: int64, real64, output_unit
    use iso_c_binding,   only: c_int64_t
    implicit none
    private
    public                  :: KdTree, KdNode, KdNodePtr, KdNodeBucket, NodeId
    public                  :: DEFAULT_BUFFER_SIZE, DEFAULT_METRIC, DEFAULT_EPSILON
    integer(int64), save    :: nextTreeId = 0_int64

    !> Default buffer size for search functions
    integer,          parameter :: DEFAULT_BUFFER_SIZE = 1000

    !> Default metric for search functions
    character(len=9), parameter :: DEFAULT_METRIC      = 'euclidean'
    
    !> Default epsilon value for floating point comparisons
    real(real64),     parameter :: DEFAULT_EPSILON      = 1.0e-15_real64

    !> 128-bit composite node identifier.
    !! node_id is stable for the lifetime of the node.
    !! pool_idx is a mutable hint to the node's current position in the pool;
    !! it may go stale after removes or additions.
    type, bind(c) :: NodeId
        integer(c_int64_t) :: node_id  = 0
        integer(c_int64_t) :: pool_idx = 0
    end type NodeId

    !> A node for a kd-tree
    type :: KdNode
        private
        logical                         :: hasData = .false. !> flagged true if node contains data
        class(*), allocatable           :: data
        real(kind=real64), allocatable  :: coords(:)
        integer(int64)                  :: splitAxis !> tracks which index in coords is the splitting plane
        type(NodeId)                    :: nodeId
        integer(int64)                  :: lch = 0_int64, rch = 0_int64  !> indices into nodePool; 0 = no child
        integer(int64)                  :: treeId
        contains    
            procedure                   :: chebyshevDist 
            procedure                   :: chebyshevDistPoint
            procedure                   :: euclideanDist
            procedure                   :: euclideanDistPoint
            procedure                   :: getCoords
            procedure                   :: getData
            procedure                   :: getNodeId
            procedure                   :: getSplitAxis
            procedure                   :: manhattanDist
            procedure                   :: manhattanDistPoint
            procedure                   :: printNode
            procedure                   :: printNodeSingle
    end type KdNode
    
    !> Pointer to an owned copy of a Node returned by a search.
    !! p is always a pointer to a heap-allocated copy; call destroy() or let it
    !! go out of scope to free the copy.
    type :: KdNodePtr
        type(KdNode), pointer :: p => null()
        contains
            procedure         :: destroy => destroyNodePtr
            final             :: finalizerNodePtr
    end type KdNodePtr

    !> Container for a "bucket" -> array of KdNodePtr
    type :: KdNodeBucket
        type(KdNodePtr), allocatable :: nodes(:)
        contains
            procedure         :: destroy => destroyNodeBucket
            final             :: finalizerNodeBucket
    end type KdNodeBucket

    !> A kd-tree
    type :: KdTree
    private
    integer(int64)        :: dim = 0_int64, pop = 0_int64, TreeId = 0_int64
    integer(int64)        :: currNodeId = 0_int64
    logical               :: initialized = .false.       !> true iff tree%build() is called successfully
    type(KdNode), pointer :: nodePool(:) => null()       !> pool of allocated nodes
    integer(int64)        :: rootIdx = 0_int64           !> index into nodePool for the root; 0 = empty
    integer(int64)        :: modifications = 0_int64     !> total number of insertions/deletions on tree
    real(real64)          :: rebuildRatio = 0.25_real64  !> if modifications > rebuildRatio * pop, trigger rebuild 

        contains
            procedure     :: addNodes
            procedure     :: assert
            procedure     :: associatedNodePool
            procedure     :: associatedRoot
            procedure     :: build
            procedure     :: destroy
            procedure     :: getAllNodeIds
            procedure     :: getAllNodes
            procedure     :: getAllCoords
            procedure     :: getDim
            procedure     :: getInitState
            procedure     :: getNumMods
            procedure     :: getPop
            procedure     :: getRebuildRatio
            procedure     :: getTreeId
            procedure     :: isMember
            procedure     :: linScan
            procedure     :: printTree
            procedure     :: rNN_Centroid
            procedure     :: rNN_Coords
            procedure     :: rNN_Ids
            procedure     :: rNN_Node
            procedure     :: rNN_Rad
            procedure     :: rNN_RadIds
            procedure     :: DBSCAN
            procedure     :: rmvNodes
            procedure     :: setRebuildRatio
            final         :: finalizer
    end type KdTree

    interface
        
        !=====================================!
        !========= KdNodeGetters.f90 =========!
        !=====================================!

        !> Returns the data stored in this node.
        !! The dynamic type of the result matches 
        !! the type passed as data to build().  
        !! Use a select type construct 
        !! to compare or assign the value.
        module function getData(this) result(data)
            class(KdNode), intent(in)  :: this
            class(*),      allocatable :: data
        end function getData

        !> Returns the coordinates of this node
        module function getCoords(this) result(coords)
            class(KdNode), intent(in)  :: this
            real(real64),  allocatable :: coords(:)
        end function getCoords

        !> Returns the splitting axis of this node
        module function getSplitAxis(this) result(splitAxs)
            class(KdNode), intent(in) :: this
            integer(int64)            :: splitAxs
        end function getSplitAxis

        !> Returns the composite node id.
        !! node_id is the stable identifier; pool_idx is a mutable hint
        !! to the node's current pool position (may go stale after mutations).
        !! Pass the returned NodeId to id-based search and remove functions.
        module function getNodeId(this) result(id)
            class(KdNode), intent(in) :: this
            type(NodeId)              :: id
        end function getNodeId

        !=================================================!

        !======================================!
        !========= KdNodeDistance.f90 =========!
        !======================================!

        !> Calculates the euclidean distance between two Nodes
        !! @param[in] that The Node to calculate distance from          
        !!
        !! @return The euclidean distance between two Nodes
        module function euclideanDist(this, that) result(dist)
            class(KdNode), intent(in)  :: this
            type(KdNode),  intent(in)  :: that
            real(kind=real64)          :: dist
        end function euclideanDist

        !> Calculates the euclidean distance between this node 
        !! and a point.
        !! @param[in] point the point 
        !! 
        !! @return the euclidean distance from the node to the point
        module function euclideanDistPoint(this, point) result(dist)
            class(KdNode),             intent(in) :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function euclideanDistPoint

        !> Calculates the Manhattan (L1) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Manhattan distance between two Nodes
        module function manhattanDist(this, that) result(dist)
            class(KdNode), intent(in)  :: this
            type(KdNode),  intent(in)  :: that
            real(kind=real64)          :: dist
        end function manhattanDist

        !> Calculates the Manhattan (L1) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Manhattan distance from the node to the point
        module function manhattanDistPoint(this, point) result(dist)
            class(KdNode),             intent(in) :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function manhattanDistPoint

        !> Calculates the Chebyshev (L∞) distance between two Nodes
        !! @param[in] that The Node to calculate distance from
        !!
        !! @return The Chebyshev distance between two Nodes
        module function chebyshevDist(this, that) result(dist)
            class(KdNode), intent(in) :: this
            type(KdNode),  intent(in) :: that
            real(kind=real64)         :: dist
        end function chebyshevDist

        !> Calculates the Chebyshev (L∞) distance between this node
        !! and a point.
        !! @param[in] point the point
        !!
        !! @return the Chebyshev distance from the node to the point
        module function chebyshevDistPoint(this, point) result(dist)
            class(KdNode),             intent(in) :: this
            real(real64), allocatable, intent(in) :: point(:)
            real(kind=real64)                     :: dist
        end function chebyshevDistPoint

        !==========================================================!
        
        !===================================!
        !========= KdNodeUtils.f90 =========!
        !===================================!

        !> Recursively prints this Node and its subtree in pre-order.
        !! @param[in] depth the depth of this Node
        !! @param[in] unit  optional output unit (defaults to stdout)
        module recursive subroutine printNode(this, depth, nodePool, unit)
            class(KdNode),  intent(in)           :: this
            integer(int64), intent(in)           :: depth
            type(KdNode),   intent(in)           :: nodePool(:)
            integer,        intent(in), optional :: unit
        end subroutine printNode

        !> Prints the current node, but not its subtree
        !!
        !! Use: printNode(depth) for the full subtree
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printNodeSingle(this, unit)
            class(KdNode), intent(in)           :: this
            integer,       intent(in), optional :: unit
        end subroutine printNodeSingle

        !> Frees the owned node copy and nulls p.
        module subroutine destroyNodePtr(this)
            class(KdNodePtr), intent(inout) :: this
        end subroutine destroyNodePtr

        !> Frees the owned node copy when NodePtr goes out of scope.
        module subroutine finalizerNodePtr(this)
            type(KdNodePtr), intent(inout) :: this
        end subroutine finalizerNodePtr

        !> Frees node ptrs in node bucket.
        module subroutine destroyNodeBucket(this)
            class(KdNodeBucket), intent(inout) :: this
        end subroutine destroyNodeBucket

        !> Frees node ptrs in node bucket when bucket goes out of scope.
        module subroutine finalizerNodeBucket(this)
            type(KdNodeBucket), intent(inout) :: this
        end subroutine finalizerNodeBucket


        !================================================================!

        !=====================================!
        !========= KdTreeGetters.f90 =========!
        !=====================================!

        !> Returns the dimension (number of splitting axis) of the tree
        module function getDim(this) result(k)
            class(KdTree), intent(in) :: this
            integer(int64)            :: k
        end function getDim

        !> Returns the number of nodes in the tree
        module function getPop(this) result(n)
            class(KdTree), intent(in) :: this
            integer(int64)            :: n
        end function getPop

        !> Sets isInit to true iff the tree is initialized
        !! param[inout] isInit the state of the tree
        module subroutine getInitState(this, isInit)
            class(KdTree), intent(in)    :: this
            logical,       intent(inout) :: isInit
        end subroutine getInitState

        !> Returns the unique integer ID assigned to this tree at build time
        module function getTreeId(this) result(id)
            class(KdTree), intent(in) :: this
            integer(int64)            :: id
        end function getTreeId


        !> Returns the rebuildRatio.
        !! If this%modifications > this%rebuildRatio * this%pop,
        !! trigger a rebuild of the tree.
        module function getRebuildRatio(this) result(rebuildRatio)
            class(KdTree), intent(in) :: this
            real(real64)              :: rebuildRatio
        end function getRebuildRatio

        !> Returns the number of modifications done on tree.
        !! If this%modifications > this%rebuildRatio * this%pop,
        !! trigger a rebuild of the tree.
        module function getNumMods(this) result(numMods)
            class(KdTree), intent(in) :: this
            integer(int64)            :: numMods
        end function getNumMods

        !> Returns the NodeId of every node currently in the pool, in pool order.
        !! Each NodeId has pool_idx set to the node's current position, so
        !! linScan takes the O(1) fast path immediately on all returned ids.
        !! Returns a zero-length array when pop=0 (tree initialized but empty).
        !!
        !! @return ids  NodeId array of length pop
        module function getAllNodeIds(this) result(ids)
            class(KdTree), intent(in)   :: this
            type(NodeId),  allocatable  :: ids(:)
        end function getAllNodeIds

        !> Returns deep copies of every node currently in the tree pool.
        !! Each copy has pool_idx stamped to the current pool position,
        !! so isMember takes the fast path immediately on all returned nodes.
        !! Returns a zero-length array when pop=0 (tree initialized but empty).
        !!
        !! @return nodes  KdNodePtr array of length pop; caller owns the copies
        module function getAllNodes(this) result(nodes)
            class(KdTree),   intent(in)  :: this
            type(KdNodePtr), allocatable :: nodes(:)
        end function getAllNodes

        !> Returns a (dim, pop) real array of all node coordinates in pool order.
        !! Intended for use as the coordsList argument to batch search functions
        !! (e.g. rNN_Coords) when every node must be queried, as in DBSCAN.
        !! Returns a (dim, 0) array when pop=0 (tree initialized but empty).
        !!
        !! @return coords  shape [dim, pop]; 
        !!                 column i is the coordinate of nodePool(i)
        module function getAllCoords(this) result(coords)
            class(KdTree), intent(in)   :: this
            real(real64),  allocatable  :: coords(:,:)
        end function getAllCoords

        !=======================================================================!

        !===================================!
        !========= KdTreeUtils.f90 =========!
        !===================================!

        !> Prints the entire tree in post-order.
        !! @param[in] unit  optional output unit (defaults to stdout)
        module subroutine printTree(this, unit)
            class(KdTree), intent(in)           :: this
            integer,       intent(in), optional :: unit
        end subroutine printTree

        !> Checks if a target node is a member of this tree instance
        !! @param[in] target the target node to check
        !!
        !! @return true if a member, false if not
        module function isMember(this, target) result(res)
            class(KdTree),         intent(in) :: this
            type(KdNode), pointer, intent(in) :: target
            logical                           :: res
        end function isMember

        !> Compares t%printTree output against expected as a sorted
        !! multiset of coord-tuples (indent and `[axis=N]` prefix stripped).
        !! Looseness is required: with ties on a split axis, points can
        !! swap siblings or change depth across runs.
        !! @param[in] testName diagnostic label
        !! @param[in] expected per-node lines in printNode format
        module subroutine assert(this, testName, expected)
            class(KdTree),    intent(in) :: this 
            character(len=*), intent(in) :: testName
            character(len=*), intent(in) :: expected(:)
        end subroutine assert

        !> Sets assoc to true iff a nodePool is allocated.
        !! Should agree with associatedSubtree and !(this%initialized)
        !! @param[inout] assoc true if this%nodePool is allocated, 
        !!                     false otherwise
        module subroutine associatedNodePool(this, assoc)
            class(KdTree),    intent(in)    :: this 
            logical,          intent(inout) :: assoc
        end subroutine associatedNodePool

        !> Sets assoc to true iff a root node is allocated.
        !! Should agree with associatedNodePool and !(this%initialized)
        !! @param[inout] assoc true if this%root is allocated, false otherwise
        module subroutine associatedRoot(this, assoc)
            class(KdTree),    intent(in)    :: this 
            logical,          intent(inout) :: assoc
        end subroutine associatedRoot

        !> Calculates the split axis of a node.
        !! Param "a" can either be depth of node or 
        !! parent's splitting axis. 
        !!
        !! @param[in] a the split axis of the parent or the depth of the node
        !! @param[in] k the dimension of the tree
        !!
        !! @return    the new split axis
        !!
        !! Examples:
        !!  - Depth Counter (0-based)
        !!
        !!  mod(0, k)+1 = 1, mod(1, k)+1 = 2, ... cycles through axes 1→2→...→dim→1
        !!
        !!   - Parent's split axis (1-based)
        !!
        !!  mod(parent_axis, dim)+1 steps to the next axis 
        !! e.g: mod(2, 3)+1 = 3, mod(3, 3)+1 = 1
        module function saxs(a, k) result(splitAxis)
            integer(int64), intent(in) :: a, k
            integer(int64)             :: splitAxis
        end function saxs

        !> Frees the node pool and resets tree state.
        module subroutine destroy(this)
            class(KdTree), intent(inout) :: this
        end subroutine destroy

        !> Frees tree automatically   when it goes out of scope
        module subroutine finalizer(this)
            type(KdTree), intent(inout) :: this
        end subroutine finalizer

        !=========================================================================!

        !===================================! 
        !========= KdTreeBuild.f90 =========! 
        !===================================!

        !> Builds a balanced Kd-Tree from a set of points.
        !! @param[in] coords       A (k, n) array where n is the number of points
        !!                         and k is the dimensionality of the splitting axes.
        !! @param[in] data         (Optional) A rank-1 array of size n, where each
        !!                         element is the data associated with a point in coords.
        !!                         The element type determines what select type cases the
        !!                         caller must handle when calling getData() on a result node.
        !! @param[in] rebuildRatio If this%modifications > this%rebuildRatio * this%pop,
        !!                         then a tree rebuild is triggered. Defaulted to 0.25.
        module subroutine build(this, coords, data, rebuildRatio)
            class(KdTree),     intent(inout)        :: this
            real(kind=real64), intent(in)           :: coords(:,:)
            class(*),          intent(in), optional :: data(:)
            real(kind=real64), intent(in), optional :: rebuildRatio
        end subroutine build

        !> Recursively builds a balanced subtree from the node pool.
        !! @param[inout] this      the tree being built
        !! @param[out]   root      index of the root of the tree
        !! @param[in]    depth     current depth, used to cycle the split axis
        !! @param[inout] indices   index permutation array, rearranged in-place by quickSelect
        !! @param[in]    lowerIdx  lower bound of the index range for this subtree
        !! @param[in]    upperIdx  upper bound of the index range for this subtree
        recursive module subroutine buildSubtree( &
            this,                                 &
            rootIdx,                              &
            depth,                                &
            indices,                              &
            lowerIdx,                             &
            upperIdx                              &
        )
            type(KdTree),   intent(inout) :: this
            integer(int64), intent(out)   :: rootIdx
            integer(int64), intent(inout) :: indices(:)
            integer(int64), intent(in)    :: lowerIdx, upperIdx, depth
        end subroutine buildSubtree

        !====================================================================================!

        !=====================================!
        !========= KdTreeModders.f90 =========!
        !=====================================!

        !> Overwrites rebuildRatio. Must be in (0, 1).
        !! Not thread-safe; do not call concurrently with addNodes().
        !! @param[in] ratio the new rebuildRatio
        module subroutine setRebuildRatio(this, ratio)
            class(KdTree),  intent(inout) :: this
            real(real64), intent(in)    :: ratio
        end subroutine setRebuildRatio

        !> Inserts new nodes into the tree.
        !!
        !! coordsList is a (k, n) array of n points in k dimensions;
        !! k must match the dimension of the tree.
        !! If the tree holds data, dataList must be provided and have
        !! exactly n elements of the same type as the existing data.
        !! New nodes are appended to the pool then either inserted at leaves
        !! or trigger a full rebuild if modifications exceed rebuildRatio * pop.
        !!
        !! Thread safety: concurrent calls to addNodes() are serialized internally
        !! via !$OMP CRITICAL. The precondition checks (before the critical section)
        !! are NOT thread-safe; callers must ensure the tree is initialized and
        !! stable before calling.
        !!
        !! @param[in] coordsList  (k, n) array of coordinates to add
        !! @param[in] dataList    optional rank-1 array of n data values
        module subroutine addNodes(this, coordsList, dataList)
            class(KdTree),intent(inout)        :: this
            real(real64), intent(in)           :: coordsList(:,:)
            class(*),     intent(in), optional :: dataList(:)
        end subroutine addNodes

        !> Removes nodes from the pool according to one or more optional filters.
        !!
        !!  - coordsList alone:
        !!      removes nodes whose coordinates match coordsList(:,i) for some i,
        !!      within tolerance epsilon.
        !!
        !!  - coordsList + radii:
        !!      removes all nodes within radii(i) of coordsList(:,i), for each i.
        !!
        !!  - ids alone:
        !!      removes all nodes whose id appears anywhere in ids.
        !!      Uses pool_idx for O(1) fast path; falls back to O(n) scan if stale.
        !!
        !!  - coordsList + ids (no radii):
        !!      ids and coordsList are treated as paired: removes nodes where
        !!      node%id == ids(i) AND node%coords match coordsList(:,i),
        !!      for each i. Requires size(ids) == size(coordsList, 2).
        !!
        !!  - coordsList + radii + ids:
        !!      removes nodes within radii(i) of coordsList(:,i) whose id also
        !!      appears anywhere in ids (ids treated as an unordered set).
        !!      ids and coordsList need not be the same size.
        !!
        !! Preconditions:
        !!
        !!  - whenever coordsList is passed:
        !!      (size(coordsList, 1) == this%dim) && (size(coordsList, 2) >= 1)
        !!  - whenever radii is passed:
        !!      coordsList must also be passed &&
        !!      (size(radii) == size(coordsList, 2))
        !!  - ids passed alone or with coordsList + radii:
        !!      ids is not empty; no size constraint relative to coordsList
        !!  - coordsList + ids (no radii):
        !!      ids is not empty && (size(ids) == size(coordsList, 2))
        !!
        !! @param[in] coordsList  (dim, n) array of target coordinates
        !! @param[in] radii       n radii paired with coordsList columns;
        !!                        if omitted, matching uses epsilon tolerance
        !! @param[in] ids         NodeIds to filter by (obtain via getNodeId());
        !!                        paired with coordsList when radii is absent, treated as a set otherwise
        !! @param[in] epsilon     coordinate-match tolerance (default DEFAULT_EPSILON);
        !!                        used only when radii is absent
        !! @param[in] metric      'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param[in] bufferSize  initial match-list capacity before reallocation
        !!                        (default DEFAULT_BUFFER_SIZE)
        !!
        !! @return    numRmv      total number of nodes removed
        !!
        !! Thread safety: the search phase is read-only and executes outside the
        !! critical section; concurrent reads are safe. Pool compaction and rebuild
        !! are serialized via !$OMP CRITICAL (tree_mutate). Callers must ensure the
        !! tree is initialized and stable before calling.
        module function rmvNodes( &
            this,                 &
            coordsList,           &
            radii,                &
            ids,                  &
            epsilon,              &
            metric,               &
            bufferSize            &
        ) result(numRmv)
            class(KdTree),    intent(inout)        :: this
            real(real64),     intent(in), optional :: radii(:)
            real(real64),     intent(in), optional :: coordsList(:,:)
            type(NodeId),     intent(in), optional :: ids(:)
            real(real64),     intent(in), optional :: epsilon
            character(len=*), intent(in), optional :: metric
            integer,          intent(in), optional :: bufferSize
            integer                                :: numRmv
        end function rmvNodes

        !===================================================================================!

        !================================================! 
        !========= search_modules/KdTreeRnn.f90 =========! 
        !================================================! 
        
        !> Radius Nearest Neighbour search. Walks the kd-tree from currIdx,
        !! appending matching nodes to res and pruning subtrees whose
        !! splitting hyperplane lies further than radius from target.
        !! @param[in]    target   the query node (used as the search centre)
        !! @param[in]    currIdx  nodePool index of the current subtree root; 
        !!                        0 terminates recursion
        !! @param[in]    nodePool the tree's node pool
        !! @param[in]    radius   search radius
        !! @param[inout] res      result buffer; doubles in size when full
        !! @param[inout] arrSize  number of results written into res so far
        !! @param[in]    metric   'euclidean', 'manhattan', 'chebyshev'; 
        !!                        default DEFAULT_METRIC
        module subroutine rNN( &
            target,            &
            currIdx,           &
            nodePool,          &
            radius,            &
            res,               &
            arrSize,           &
            metric             &
        )
                type(KdNode),      intent(in)                :: target
                integer(int64),    intent(in)                :: currIdx
                type(KdNode),      intent(in)                :: nodePool(:)
                real(kind=real64), intent(in)                :: radius
                integer,           intent(inout)             :: arrSize
                type(KdNodePtr),  allocatable, intent(inout) :: res(:)
                character(len=*), intent(in)                 :: metric
        end subroutine rNN 

        !> Performs radius nearest neighbour search on a target node
        !!
        !! Searches for nodes within a given radius of target node;
        !! Includes target node in results by default.
        !! @param[in] target        the target node
        !! @param[in] radius        the search radius; error stop if negative
        !! @param[in] bufferSize    initial result buffer size; doubles when full; 
        !!                           default DEFAULT_BUFFER_SIZE; error stop if <= 0
        !! @param[in] metric        'euclidean', 'manhattan', 'chebyshev'; 
        !!                          default DEFAULT_METRIC
        !! @param[in] excludeTarget if .true., removes the target node from the returned list
        !!
        !! @return list of nodes within the search radius
        module function rNN_Node( &
            this,                 &
            target,               &
            radius,               &
            bufferSize,           &
            metric,               &
            excludeTarget         &
        ) result(res)
            class(KdTree),       intent(in)            :: this
            type(KdNodePtr),     intent(in)            :: target
            real(kind=real64),   intent(in)            :: radius
            integer,             intent(in), optional  :: bufferSize
            character(len=*),    intent(in), optional  :: metric
            logical,             intent(in), optional  :: excludeTarget
            type(KdNodePtr),     allocatable           :: res(:)
        end function rNN_Node

        !> Performs radius nearest neighbour search on a centroid
        !!
        !! Searches for nodes within a given radius of an arbitrary point;
        !! @param[in] centroid    the centre of the search sphere; must match the tree dimension
        !! @param[in] radius      the search radius; error stop if negative
        !! @param[in] bufferSize  initial result buffer size; doubles when full; 
        !!                        default DEFAULT_BUFFER_SIZE; error stop if <= 0
        !! @param[in] metric      'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !!
        !! @return list of nodes within the search radius
        module function rNN_Centroid(   &
            this,                       &
            centroid,                   &
            radius,                     &
            bufferSize,                 &
            metric                      &
        ) result(res)
            class(KdTree),     intent(in)           :: this
            real(kind=real64), intent(in)           :: radius, centroid(:)
            integer,           intent(in), optional :: bufferSize
            character(len=*),  intent(in), optional :: metric 
            type(KdNodePtr),   allocatable          :: res(:)

        end function rNN_Centroid

        !> Search the tree for nodes matching a set of query coordinates.
        !!
        !! coords(:,:) is laid out as (ndim, nQuery), one column per query point.
        !! Returns a parallel array res(nQuery) of KdNodeBucket; res(i) holds all
        !! nodes within epsilon of coords(:,i). If no match is found for query i,
        !! res(i) is empty (size 0).
        !!
        !! @param metric     'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param epsilon    match radius; default DEFAULT_EPSILON
        !! @param bufferSize initial capacity of each bucket before reallocation; 
        !!                   default DEFAULT_BUFFER_SIZE
        !!
        !! @return res a NodeBucket containing results
        module function rNN_Coords( &
            this,                   &
            coords,                 &
            metric,                 &
            epsilon,                &
            bufferSize              &
        ) result(res)
            class(KdTree),      intent(in)           :: this
            real(real64),       intent(in)           :: coords(:,:)
            character(len=*),   intent(in), optional :: metric 
            real(real64),       intent(in), optional :: epsilon
            integer,            intent(in), optional :: bufferSize
            type(KdNodeBucket), allocatable          :: res(:)
        end function rNN_Coords

        !> Search the tree for nodes matching both a coordinate and a node id.
        !!
        !! coords(:,:) is laid out as (ndim, nQuery); ids(nQuery) is a parallel
        !! array of target node ids. For each query i, the search first collects
        !! all nodes within epsilon of coords(:,i), then filters to those whose
        !! id equals ids(i). Returns a parallel array res(nQuery) of KdNodeBucket;
        !! res(i) is empty if no node satisfies both criteria.
        !!
        !! @param metric     'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param epsilon    match radius; default DEFAULT_EPSILON
        !! @param bufferSize initial capacity of each bucket before reallocation; 
        !!                   default DEFAULT_BUFFER_SIZE
        !!
        !! @return res a NodeBucket containing results
        module function rNN_Ids( &
            this,                &
            coords,              &
            ids,                 &
            metric,              &
            epsilon,             &
            bufferSize           &
        ) result(res)
            class(KdTree),      intent(in)           :: this
            real(real64),       intent(in)           :: coords(:,:)
            type(NodeId),       intent(in)           :: ids(:)
            character(len=*),   intent(in), optional :: metric
            real(real64),       intent(in), optional :: epsilon
            integer,            intent(in), optional :: bufferSize
            type(KdNodeBucket), allocatable          :: res(:)
        end function rNN_Ids

        !> Search all nodes within radii(i) of coordsList(:,i), for each i.
        !!
        !! coords(:,:) is laid out as (ndim, nQuery); radii(nQuery) is a parallel
        !! list of radii to search for. Returns a parallel array res(nQuery) of KdNodeBucket;
        !! res(i) is empty if no node satisfies criteria.
        !!
        !! @param metric     'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param bufferSize initial capacity of each bucket before reallocation; 
        !!                   default DEFAULT_BUFFER_SIZE
        !!
        !! @return res a NodeBucket containing results
        module function rNN_Rad( &
            this,                &
            coords,              &
            radii,               &
            metric,              &
            bufferSize           &
        ) result(res)
            class(KdTree),      intent(in)           :: this
            real(real64),       intent(in)           :: coords(:,:), radii(:)
            character(len=*),   intent(in), optional :: metric 
            integer,            intent(in), optional :: bufferSize
            type(KdNodeBucket), allocatable          :: res(:)
        end function rNN_Rad
    
        !> For each query point i, searches all nodes within radii(i) of
        !! coordsList(:,i) whose id appears anywhere in ids.
        !! ids is treated as an unordered set and is not paired with coordsList.
        !!
        !! coordsList is laid out as (ndim, nQuery); radii(nQuery) is a parallel
        !! list of radii, one per query point. Returns a parallel array res(nQuery)
        !! of KdNodeBucket; res(i) is empty if no node satisfies the criteria
        !! for query point i.
        !!
        !! @param[in] coordsList  (dim, nQuery) array of query coordinates
        !! @param[in] radii       nQuery radii, paired with coordsList columns
        !! @param[in] ids         unordered set of NodeIds to filter by;
        !!                        not paired with coordsList; obtain via getNodeId()
        !! @param[in] metric      'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param[in] bufferSize  initial capacity of each bucket before reallocation; 
        !!                        default DEFAULT_BUFFER_SIZE
        !!
        !! @return    res         res(nQuery) of KdNodeBucket; res(i) contains all
        !!                        nodes within radii(i) of coordsList(:,i) whose id
        !!                        appears in ids; empty bucket if no match
        module function rNN_RadIds( &
            this,                   &
            coords,                 &
            radii,                  &
            ids,                    &
            metric,                 &
            bufferSize              &
        ) result(res)
            class(KdTree),      intent(in)           :: this
            real(real64),       intent(in)           :: coords(:,:), radii(:)
            type(NodeId),       intent(in)           :: ids(:)
            character(len=*),   intent(in), optional :: metric
            integer,            intent(in), optional :: bufferSize
            type(KdNodeBucket), allocatable          :: res(:)
        end function rNN_RadIds

        !======================================================================================!
        
        !====================================================!
        !========= search_modules/KdTreeLinScan.f90 =========!
        !====================================================!

        !> Looks up nodes by NodeId, using pool_idx for an O(1) fast path.
        !! Falls back to O(n) scan only when the pool_idx hint is stale.
        !!
        !! Returns a zero-length array when pop=0 or ids is empty.
        !!
        !! @param[in] ids  NodeIds to find (unordered set); obtain via getNodeId()
        !!
        !! @return    res  KdNodePtr array of matched nodes; empty if no match
        module function linScan(this, ids) result(res)
            class(KdTree),   intent(in)  :: this
            type(NodeId),    intent(in)  :: ids(:)
            type(KdNodePtr), allocatable :: res(:)
        end function linScan

        !==========================================================================!

        !===================================================!
        !========= search_modules/KdTreeDBSCAN.f90 =========!
        !===================================================!

        !> Density-based spatial clustering (DBSCAN).
        !!
        !! Returns an array of KdNodeBucket, one bucket per cluster found.
        !!
        !! @param[in] minPts      minimum neighbourhood size to classify a point as a core point
        !! @param[in] radius      neighbourhood search radius (eps)
        !! @param[in] metric      'euclidean', 'manhattan', 'chebyshev'; default DEFAULT_METRIC
        !! @param[in] bufferSize  initial rNN result buffer size; default DEFAULT_BUFFER_SIZE
        !!
        !! @return res  array of KdNodeBucket; res(1:n-1) contains buckets for each cluster,
        !!                       res(n) contains "noise". If tree is empty, then res will be empty.
        module function DBSCAN(this, minPts, radius, metric, bufferSize) result(res)
            class(KdTree),      intent(in)           :: this
            integer,            intent(in)           :: minPts
            real(real64),       intent(in)           :: radius
            character(len=*),   intent(in), optional :: metric
            integer,            intent(in), optional :: bufferSize
            type(KdNodeBucket), allocatable          :: res(:)
        end function DBSCAN

        !=========================================================================================!
    
    end interface

end module KdTreeFortran