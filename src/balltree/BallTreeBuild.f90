submodule(NdTreeFortran) BallTreeBuild
    implicit none
    contains
        module procedure buildSubtreeBLT
            real(real64)              :: radius, maxDst, minDst, dist, randomNumber
            real(real64), allocatable :: pivotCrd(:), centroid(:)
            integer(int64)            :: pivotIdx, poleA, poleB, i, rootPos, partBound, tmp

            if (lowerIdx > upperIdx) then
                rootIdx = 0_int64
            else

                ! select a random pivotIdx within our idx range
                ! to act as the "center point"
                call random_number(randomNumber)
                pivotIdx = lowerIdx + floor(randomNumber * (upperIdx - lowerIdx + 1_int64))
                pivotCrd = this%nodePool(indices(pivotIdx))%coords

                ! search for poleA -> point furthest away from pivotCrd
                maxDst = -1_int64
                do i = lowerIdx, upperIdx
                    select case(this%metric)
                        case('euclidean')
                            dist = this%nodePool(indices(i))%euclideanDistPoint(pivotCrd)
                        case('chebyshev')
                            dist = this%nodePool(indices(i))%chebyshevDistPoint(pivotCrd)
                        case('manhattan')
                            dist = this%nodePool(indices(i))%manhattanDistPoint(pivotCrd)
                        case default
                            error stop "buildSubtreeBLT: unknown metric!"
                    end select
                    if (dist .gt. maxDst) then
                        maxDst = dist
                        poleA = indices(i)
                    end if
                end do

                ! search for poleB -> point furthest away from poleA
                maxDst = -1_int64
                do i = lowerIdx, upperIdx
                    select case(this%metric)
                        case('euclidean')
                            dist = this%nodePool(indices(i))%euclideanDist(this%nodePool(poleA))
                        case('chebyshev')
                            dist = this%nodePool(indices(i))%chebyshevDist(this%nodePool(poleA))
                        case('manhattan')
                            dist = this%nodePool(indices(i))%manhattanDist(this%nodePool(poleA))
                        case default
                            error stop "buildSubtreeBLT: unknown metric!"
                    end select
                    if (dist .gt. maxDst) then
                        maxDst = dist
                        poleB = indices(i)
                    end if
                end do

                ! centroid = midpoint between poleA and poleB
                centroid = 0.5_real64 * (this%nodePool(poleA)%coords + this%nodePool(poleB)%coords)

                ! find the point closest to the centroid — this becomes the subtree root
                minDst = huge(minDst)
                do i = lowerIdx, upperIdx
                    select case(this%metric)
                        case('euclidean')
                            dist = this%nodePool(indices(i))%euclideanDistPoint(centroid)
                        case('chebyshev')
                            dist = this%nodePool(indices(i))%chebyshevDistPoint(centroid)
                        case('manhattan')
                            dist = this%nodePool(indices(i))%manhattanDistPoint(centroid)
                        case default
                            error stop "buildSubtreeBLT: unknown metric!"
                    end select
                    if (dist .lt. minDst) then
                        minDst = dist
                        rootIdx = indices(i)
                        rootPos = i
                    end if
                end do

                ! rootIdx is now the point closest to the midpoint, will be root of this subtree
                allocate(this%nodePool(rootIdx)%nodeParams(1))
                allocate(this%nodePool(rootIdx)%children(2))
                this%nodePool(rootIdx)%children(:) = 0_int64
                this%nodePool(rootIdx)%treeId = this%treeId

                ! bounding sphere: radius from root to whichever pole is farther
                select case(this%metric)
                    case('euclidean')
                        radius = max(                                                   &
                            this%nodePool(rootIdx)%euclideanDist(this%nodePool(poleA)), &
                            this%nodePool(rootIdx)%euclideanDist(this%nodePool(poleB))  &
                        )
                    case('chebyshev')
                        radius = max(                                                   &
                            this%nodePool(rootIdx)%chebyshevDist(this%nodePool(poleA)), &
                            this%nodePool(rootIdx)%chebyshevDist(this%nodePool(poleB))  &
                        )
                    case('manhattan')
                        radius = max(                                                   &
                            this%nodePool(rootIdx)%manhattanDist(this%nodePool(poleA)), &
                            this%nodePool(rootIdx)%manhattanDist(this%nodePool(poleB))  &
                        )
                    case default
                        error stop "buildSubtreeBLT: unknown metric!"
                end select
                this%nodePool(rootIdx)%nodeParams(1) = radius

                ! stash root at end so the partition doesn't touch it
                tmp = indices(rootPos)
                indices(rootPos) = indices(upperIdx)
                indices(upperIdx) = tmp

                call quickSelectPartition(       &
                    this%nodePool,               &
                    indices,                     &
                    lowerIdx,                    &
                    upperIdx - 1_int64,          &
                    this%nodePool(poleA)%coords, &
                    this%nodePool(poleB)%coords, &
                    this%metric,                 &
                    partBound                    &
                )

                ! place root at partition boundary: left=A-closer, root, right=B-closer
                tmp = indices(partBound)
                indices(partBound) = indices(upperIdx)
                indices(upperIdx) = tmp

                call this%buildSubtree(                 &
                    this%nodePool(rootIdx)%children(1), &
                    indices,                            &
                    lowerIdx,                           &
                    partBound - 1_int64                 &
                )
                call this%buildSubtree(                 &
                    this%nodePool(rootIdx)%children(2), &
                    indices,                            &
                    partBound + 1_int64,                &
                    upperIdx                            &
                )
            end if
        end procedure buildSubtreeBLT

        !> Two-way partition of indices(lowerIdx:upperIdx) around two poles.
        !! On exit the subarray is split into two contiguous regions:
        !!
        !!  [ lowerIdx ............. | ............... upperIdx ]
        !!  [ closer to poleA ...... | ...... closer to poleB ..]
        !!
        !! Uses Dutch National Flag: equal-distance elements stay in place.
        !!
        !! @param[in]    nodes      array of nodes
        !! @param[inout] indices    index permutation array, modified in-place
        !! @param[in]    lowerIdx   lower bound of the subarray to partition
        !! @param[in]    upperIdx   upper bound of the subarray to partition
        !! @param[in]    poleACrds  coordinates of poleA
        !! @param[in]    poleBCrds  coordinates of poleB
        !! @param[in]    metric     distance metric ('euclidean'|'chebyshev'|'manhattan')
        subroutine quickSelectPartition( &
            nodes,                       &
            indices,                     &
            lowerIdx,                    &
            upperIdx,                    &
            poleACrds,                   &
            poleBCrds,                   &
            metric,                      &
            partBound                    &
            )

            type(NdNode),               intent(in)    :: nodes(:)
            integer(int64),             intent(inout) :: indices(:)
            integer(int64),             intent(in)    :: lowerIdx, upperIdx
            real(real64), allocatable,  intent(in)    :: poleACrds(:), poleBCrds(:)
            character(len=*),           intent(in)    :: metric
            integer(int64),             intent(out)   :: partBound
            real(real64)                              :: dstA, dstB
            integer(int64)                            :: i, tmp, lowerMiddleIdx, upperMiddleIdx

            i              = lowerIdx
            lowerMiddleIdx = lowerIdx
            upperMiddleIdx = upperIdx
            do while (i .le. upperMiddleIdx)
                select case(metric)
                case('euclidean')
                    dstA = nodes(indices(i))%euclideanDistPoint(poleACrds)
                    dstB = nodes(indices(i))%euclideanDistPoint(poleBCrds)
                case('chebyshev')
                    dstA = nodes(indices(i))%chebyshevDistPoint(poleACrds)
                    dstB = nodes(indices(i))%chebyshevDistPoint(poleBCrds)
                case('manhattan')
                    dstA = nodes(indices(i))%manhattanDistPoint(poleACrds)
                    dstB = nodes(indices(i))%manhattanDistPoint(poleBCrds)
                case default
                    error stop "quickSelectPartition: unknown metric!"
                end select
                if (dstA .lt. dstB) then
                    tmp = indices(i)
                    indices(i) = indices(lowerMiddleIdx)
                    indices(lowerMiddleIdx) = tmp
                    lowerMiddleIdx = lowerMiddleIdx + 1_int64
                    i = i + 1_int64
                else if (dstA .gt. dstB) then
                    tmp = indices(i)
                    indices(i) = indices(upperMiddleIdx)
                    indices(upperMiddleIdx) = tmp
                    upperMiddleIdx = upperMiddleIdx - 1_int64
                else
                    i = i + 1_int64
                end if
            end do
            partBound = lowerMiddleIdx
        end subroutine quickSelectPartition
end submodule BallTreeBuild
