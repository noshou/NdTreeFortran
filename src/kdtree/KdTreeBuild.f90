submodule(NdTreeFortran) KdTreeBuild
    implicit none 
    contains 
        module procedure buildKDT

            integer(int64), allocatable :: indices(:)
            integer(int64)              :: i, id

            if (this%initialized) error stop "build: tree is already initialized (call destroy first)"

            ! initialize dimension and population size
            this%dim = size(coords, 1)
            this%pop = size(coords, 2) 

            ! ensure number of data points is equal to number of coordinates 
            if (present(data)) then
                if ((size(data) .ne. this%pop) .and. (size(data) .ne. 0_int64)) then 
                    error stop "data array length must equal number of points"
                end if 
            end if

            ! initialize node pool 
            !! NOTE: THIS IS NOT MULTITHREAD SAFE
            allocate(this%nodePool(this%pop))
            do i = 1, this%pop
                allocate(this%nodePool(i)%coords(this%dim))
                this%nodePool(i)%coords(:) = coords(:, i)
                if ((present(data)) .and. (size(data) .ne. 0_int64)) then
                    allocate(this%nodePool(i)%data, source=data(i))
                    this%nodePool(i)%hasData = .true.
                end if
                this%currNodeId                   = this%currNodeId + 1_int64
                this%nodePool(i)%nodeId%node_id  = this%currNodeId
                this%nodePool(i)%nodeId%pool_idx = i
            end do

            ! allocate indices; that way we don't have to modify the list of nodes
            allocate(indices(this%pop))
            indices = [(i, i=1_int64, this%pop)]

            ! initialize tree id 
            ! must use: -fopenmp for gfortran or -qopenmp for ifort for thread saftey
            
            if (nextTreeId .eq. huge(nextTreeId)) error stop "build: treeID overflow! (how the f**k did you get this \[^_^]/)"
            
            !$OMP ATOMIC CAPTURE
            nextTreeId = nextTreeId + 1_int64
            id = nextTreeId
            !$OMP END ATOMIC
            this%treeId = id

            ! build tree
            call this%buildSubtree(this%rootIdx, 0_int64, indices, 1_int64, this%pop)

            deallocate(indices)
            
            if (present(rebuildRatio)) then 
                call this%setRebuildRatio(rebuildRatio)
            end if            
            this%initialized = .true.

        end procedure buildKDT

        module procedure buildSubtreeKdt

            integer(int64) :: axis, median, middleBounds(2), targetIdx

            ! base case: we are at a leaf (or tree is empty)
            if (lowerIdx > upperIdx) then
                rootIdx = 0_int64
            else
                axis = this%saxs(depth, this%dim)
                targetIdx = (lowerIdx + upperIdx) / 2_int64
                median = quickSelect( &
                    this%nodePool,    &
                    indices,          &
                    lowerIdx,         &
                    upperIdx,         &
                    axis,             &
                    middleBounds,     &
                    targetIdx         &
                )
                rootIdx = indices(median)
                allocate(this%nodePool(rootIdx)%nodeParams(1))
                allocate(this%nodePool(rootIdx)%children(2))
                this%nodePool(rootIdx)%nodeParams(1) = real(axis, real64)
                this%nodePool(rootIdx)%children(:)   = 0_int64
                this%nodePool(rootIdx)%treeId        = this%treeId
                call this%buildSubtree(this%nodePool(rootIdx)%children(1), depth+1_int64, indices, lowerIdx, median-1_int64)
                call this%buildSubtree(this%nodePool(rootIdx)%children(2), depth+1_int64, indices, median+1_int64, upperIdx)
            end if

        end procedure buildSubtreeKdt

        !> Rearranges indices so that indices(targetIdx) holds the
        !! median element along the given axis, with smaller values to its left and
        !! larger values to its right. Returns targetIdx.
        !!
        !! @param[in] nodes             array of kd-tree nodes
        !! @param[inout] indices        index permutation array, modified in-place
        !! @param[in] lowerIdx          lower bound of the current subarray
        !! @param[in] upperIdx          upper bound of the current subarray
        !! @param[in] axis              coordinate axis to compare on
        !! @param[inout] middleBounds   bounds of the equal-to-pivot region
        !! @param[in] targetIdx         rank being searched for (fixed across all recursive calls)
        !!
        !! @return the index of the median value
        recursive function quickSelect( &
            nodes,                      & 
            indices,                    &
            lowerIdx,                   &
            upperIdx,                   &
            axis,                       &
            middleBounds,               &
            targetIdx                   &
            ) result(median)
            
            type(NdNode), intent(in)          :: nodes(:)
            integer(int64), intent(inout)   :: indices(:)
            integer(int64), intent(in)      :: lowerIdx, upperIdx, axis, targetIdx
            integer(int64), intent(inout)   :: middleBounds(2)
            integer(int64)                  :: pivotIdx, median
            real(kind=real64)               :: pivotVal, randomNumber

            if (lowerIdx .eq. upperIdx) then 
                median = lowerIdx
            else 

                call random_number(randomNumber)
                pivotIdx = lowerIdx + floor(randomNumber * (upperIdx - lowerIdx + 1_int64))
                pivotVal = nodes(indices(pivotIdx))%coords(axis)
                call quickSelectPartition(  &
                    nodes,                  &
                    indices,                &
                    lowerIdx,               &
                    upperIdx,               &
                    middleBounds,           &
                    axis,                   &
                    pivotVal                &
                )

                if (targetIdx .lt. middleBounds(1)) then 
                    median = quickSelect(   &
                        nodes,              &
                        indices,            &
                        lowerIdx,           &
                        middleBounds(1)-1,  &
                        axis,               &
                        middleBounds,       &
                        targetIdx           &
                    )
                else if (targetIdx .gt. middleBounds(2)) then 
                    median = quickSelect(   &
                        nodes,              &
                        indices,            &
                        middleBounds(2)+1,  &
                        upperIdx,           &
                        axis,               &
                        middleBounds,       &
                        targetIdx           &
                    )
                else 
                    median = targetIdx
                end if
            end if
        end function quickSelect

        !> Three-way partition of indices(lowerIdx:upperIdx) around pivot
        !! On exit, the subarray is split into three contiguous regions
        !!  [ lowerIdx ... middleBounds(1)-1 | middleBounds(1) ... middleBounds(2) | middleBounds(2)+1 ... upperIdx ]
        !!  [ < pivot .......................| = pivot ............................| > pivot .......................]
        !! @param[in] nodes             array of kd-tree nodes
        !! @param[inout] indices        index permutation array, modified in-place
        !! @param[in] lowerIdx          lower bound of the subarray to partition
        !! @param[in] upperIdx          upper bound of the subarray to partition
        !! @param[inout] middleBounds   on exit: (1) first index of equal region, (2) last index
        !! @param[in] axis              coordinate axis to compare on
        !! @param[in] pivot             pivot value
        recursive subroutine quickSelectPartition(  &
            nodes,                                  & 
            indices,                                &
            lowerIdx,                               &
            upperIdx,                               &
            middleBounds,                           &
            axis,                                   &
            pivot                                   &
            )

            type(NdNode), intent(in)         :: nodes(:)
            integer(int64), intent(inout)  :: indices(:), middleBounds(2)
            integer(int64), intent(in)     :: lowerIdx, upperIdx, axis
            integer(int64)                 :: i, tmp, lowerMiddleIdx, upperMiddleIdx
            real(kind=real64), intent(in)  :: pivot

            i = lowerIdx
            lowerMiddleIdx = lowerIdx
            upperMiddleIdx = upperIdx
            do while(i .le. upperMiddleIdx)
                if (nodes(indices(i))%coords(axis) < pivot) then 
                    tmp = indices(i)
                    indices(i) = indices(lowerMiddleIdx)
                    indices(lowerMiddleIdx) = tmp
                    i = i + 1_int64
                    lowerMiddleIdx = lowerMiddleIdx + 1_int64
                else if (nodes(indices(i))%coords(axis) > pivot) then
                    tmp = indices(i)
                    indices(i) = indices(upperMiddleIdx)
                    indices(upperMiddleIdx) = tmp
                    upperMiddleIdx = upperMiddleIdx - 1_int64
                else
                    i = i + 1_int64
                end if
            end do 

            middleBounds(1) = lowerMiddleIdx
            middleBounds(2) = upperMiddleIdx

        end subroutine quickSelectPartition


end submodule KdTreeBuild