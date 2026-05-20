submodule(NdTreeFortran) KdTreeBuild
    implicit none
    contains
        module procedure buildSubtreeKdt

            integer(int64) :: axis, median, middleBounds(2), targetIdx

            ! depth is required for KdTree: it drives the axis-cycling split rule
            if (.not. present(depth)) error stop "buildSubtreeKDT: depth is required"

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
                call this%buildSubtree(this%nodePool(rootIdx)%children(1), indices, lowerIdx,        median-1_int64, depth=depth+1_int64)
                call this%buildSubtree(this%nodePool(rootIdx)%children(2), indices, median+1_int64, upperIdx,       depth=depth+1_int64)
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
            
            type(NdNode), intent(in)        :: nodes(:)
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
        !!
        !!  [ lowerIdx ... middleBounds(1)-1 | middleBounds(1) ... middleBounds(2) | middleBounds(2)+1 ... upperIdx ]
        !!
        !!  [ < pivot .......................| = pivot ............................| > pivot .......................]
        !!
        !! @param[in]    nodes        array of nodes
        !! @param[inout] indices      index permutation array, modified in-place
        !! @param[in]    lowerIdx     lower bound of the subarray to partition
        !! @param[in]    upperIdx     upper bound of the subarray to partition
        !! @param[inout] middleBounds on exit: (1) first index of equal region, (2) last index
        !! @param[in]    axis         coordinate axis to compare on
        !! @param[in]    pivot        pivot value
        subroutine quickSelectPartition( &
            nodes,                       & 
            indices,                     &
            lowerIdx,                    &
            upperIdx,                    &
            middleBounds,                &
            axis,                        &
            pivot                        &
            )

            type(NdNode),      intent(in)    :: nodes(:)
            integer(int64),    intent(inout) :: indices(:), middleBounds(2)
            integer(int64),    intent(in)    :: lowerIdx, upperIdx, axis
            integer(int64)                   :: i, tmp, lowerMiddleIdx, upperMiddleIdx
            real(kind=real64), intent(in)    :: pivot

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