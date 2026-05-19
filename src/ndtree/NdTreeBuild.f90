submodule(NdTreeFortran) NdTreeBuild
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure build

            integer(int64), allocatable :: indices(:)
            integer(int64)              :: i, id

            if (this%initialized) error stop "build: tree is already initialized (call destroy first)"

            ! dim and pop are derived from coords shape; stored on tree for use by submodules
            this%dim = size(coords, 1)
            this%pop = size(coords, 2)

            if (present(data)) then
                if ((size(data) .ne. this%pop) .and. (size(data) .ne. 0_int64)) then
                    error stop "build: data array length must equal number of points"
                end if
            end if

            ! populate node pool: coords, optional data, and stable node ids
            ! node ids are monotonically increasing across all trees in the program
            ! NOTE: NOT MULTITHREAD SAFE
            allocate(this%nodePool(this%pop))
            do i = 1, this%pop
                allocate(this%nodePool(i)%coords(this%dim))
                this%nodePool(i)%coords(:) = coords(:, i)
                if ((present(data)) .and. (size(data) .ne. 0_int64)) then
                    allocate(this%nodePool(i)%data, source=data(i))
                    this%nodePool(i)%hasData = .true.
                end if
                this%currNodeId                  = this%currNodeId + 1_int64
                this%nodePool(i)%nodeId%node_id  = this%currNodeId
                this%nodePool(i)%nodeId%pool_idx = i
            end do

            ! indices is a permutation array: buildSubtree rearranges it in-place
            ! without touching nodePool, keeping pool positions stable
            allocate(indices(this%pop))
            indices = [(i, i=1_int64, this%pop)]

            ! treeId is globally unique across concurrent builds; atomic capture
            ! ensures no two trees share an id even under OpenMP parallelism
            if (nextTreeId .eq. huge(nextTreeId)) error stop "build: treeID overflow! (how the f**k did you get this \[^_^]/)"
            !$OMP ATOMIC CAPTURE
            nextTreeId = nextTreeId + 1_int64
            id = nextTreeId
            !$OMP END ATOMIC
            this%treeId = id

            ! depth is optional in the interface; tree types that need it (KdTree)
            ! receive 0 as the root depth; types that don't (BallTree) omit it
            call this%buildSubtree(this%rootIdx, indices, 1_int64, this%pop, depth=0_int64)

            deallocate(indices)

            if (present(rebuildRatio)) call this%setRebuildRatio(rebuildRatio)
            this%initialized = .true.

        end procedure build

end submodule NdTreeBuild
