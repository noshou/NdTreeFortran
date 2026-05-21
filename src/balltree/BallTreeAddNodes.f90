submodule(NdTreeFortran) BallTreeModders
    implicit none
    contains 

        module procedure addNodesBLT
            integer(int64) :: i, numNodeToAdd, currIdx, c1, c2
            real(real64)   :: radius, dst, dstA, dstB

            numNodeToAdd = int(size(coordsList, 2), int64)

            ! no need for rebuild; insert new nodes at leaves.
            ! rootIdx=0 (empty tree) can never reach this branch; the rebuild condition
            ! simplifies to numNodeToAdd > 0, which is always true when pop was zero.
            do i = this%pop-numNodeToAdd + 1, this%pop
                currIdx = this%rootIdx
                do
                    ! distance from the new node to the current node's center
                    select case (this%metric)
                        case ('euclidean')
                            dst = this%nodePool(i)%euclideanDist(this%nodePool(currIdx))
                        case ('chebyshev')
                            dst = this%nodePool(i)%chebyshevDist(this%nodePool(currIdx))
                        case ('manhattan')
                            dst = this%nodePool(i)%manhattanDist(this%nodePool(currIdx))
                        case default
                            error stop "addNodesBLT: unknown metric"
                    end select

                    ! the new node joins this subtree, so its bounding ball
                    ! must expand to cover the new point
                    radius = this%nodePool(currIdx)%nodeParams(1)
                    this%nodePool(currIdx)%nodeParams(1) = max(radius, dst)

                    c1 = this%nodePool(currIdx)%children(1)
                    c2 = this%nodePool(currIdx)%children(2)

                    if (c1 .eq. 0_int64) then
                        this%nodePool(currIdx)%children(1) = i
                        allocate(this%nodePool(i)%nodeParams(1))
                        allocate(this%nodePool(i)%children(2))
                        this%nodePool(i)%nodeParams(1) = 0.0_real64
                        this%nodePool(i)%children(:)   = 0_int64
                        exit
                    else if (c2 .eq. 0_int64) then
                        this%nodePool(currIdx)%children(2) = i
                        allocate(this%nodePool(i)%nodeParams(1))
                        allocate(this%nodePool(i)%children(2))
                        this%nodePool(i)%nodeParams(1) = 0.0_real64
                        this%nodePool(i)%children(:)   = 0_int64
                        exit
                    else
                        ! both slots full: descend into the child with the closer center
                        select case (this%metric)
                            case ('euclidean')
                                dstA = this%nodePool(i)%euclideanDist(this%nodePool(c1))
                                dstB = this%nodePool(i)%euclideanDist(this%nodePool(c2))
                            case ('chebyshev')
                                dstA = this%nodePool(i)%chebyshevDist(this%nodePool(c1))
                                dstB = this%nodePool(i)%chebyshevDist(this%nodePool(c2))
                            case ('manhattan')
                                dstA = this%nodePool(i)%manhattanDist(this%nodePool(c1))
                                dstB = this%nodePool(i)%manhattanDist(this%nodePool(c2))
                            case default
                                error stop "addNodesBLT: unknown metric"
                        end select
                        if (dstA .le. dstB) then
                            currIdx = c1
                        else
                            currIdx = c2
                        end if
                    end if
                end do
            end do
        end procedure addNodesBLT
end submodule BallTreeModders