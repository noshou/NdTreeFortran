submodule(NdTreeFortran) BallTreeRnn
    implicit none
    contains
            module procedure rNN_BLT
                integer(int64), allocatable  :: stack(:), stmp(:)
                integer(int64)               :: stackTop, stackSize, childIdx
                real(kind=real64)            :: dst, childRad
                type(NdNodePtr), allocatable :: tmp(:)
                integer                      :: i
                type(NdNode)                 :: parent
                type(NdNode), pointer        :: copy
                logical                      :: withinRadius
                
                if (currIdx .eq. 0_int64) then 
                    return
                else
                    stackSize = 64_int64
                    allocate(stack(stackSize))
                    stackTop  = 1_int64
                    stack(1)  = currIdx

                    do while (stackTop > 0_int64)
                        parent   = nodePool(stack(stackTop))
                        stackTop = stackTop - 1_int64

                        select case (this%metric)
                            case ('euclidean')
                                withinRadius = target%euclideanDist(parent) .le. radius
                            case ('manhattan')
                                withinRadius = target%manhattanDist(parent) .le. radius
                            case ('chebyshev')
                                withinRadius = target%chebyshevDist(parent) .le. radius
                            case default
                                error stop "rNN_BLT: unknown metric"
                        end select

                        ! append to found nodes
                        if (withinRadius) then 
                            if (size(res) .eq. arrSize) then 
                                allocate(tmp(2*size(res)))
                                do i = 1, arrSize
                                    tmp(i)%p => res(i)%p
                                    res(i)%p => null() 
                                end do
                                call move_alloc(from=tmp, to=res)
                            end if
                            arrSize = arrSize + 1
                            allocate(copy, source=parent)
                            res(arrSize)%p => copy
                        end if

                        ! grow stack if needed before pushing up to 2 children
                        if (stackTop + 2 > stackSize) then
                            stackSize = stackSize * 2_int64
                            allocate(stmp(stackSize))
                            stmp(1:stackTop) = stack(1:stackTop)
                            call move_alloc(from=stmp, to=stack)
                        end if

                        ! for each child node: 
                        ! case 1: bounding sphere is entirely within search sphere; add all nodes in this subtree
                        ! case 2: bounding sphere overlaps with search sphere; push the child to stack 
                        if (parent%children(1) .ne. 0_int64) then
                            childIdx = parent%children(1)
                            childRad = nodePool(childIdx)%nodeParams(1)
                            select case (this%metric)
                                case ('euclidean')
                                    dst = target%euclideanDist(nodePool(childIdx))
                                case ('manhattan')
                                    dst = target%manhattanDist(nodePool(childIdx))
                                case ('chebyshev')
                                    dst = target%chebyshevDist(nodePool(childIdx))
                                case default
                                    error stop "rNN_BLT: unknown metric"
                            end select
                            if (dst + childRad .le. radius) then
                                call addSubtree(nodePool, childIdx, res, arrSize)
                            else if (dst - childRad .le. radius) then
                                stackTop        = stackTop + 1_int64
                                stack(stackTop) = childIdx
                            end if
                        end if
                        if (parent%children(2) .ne. 0_int64) then
                            childIdx = parent%children(2)
                            childRad = nodePool(childIdx)%nodeParams(1)
                            select case (this%metric)
                                case ('euclidean')
                                    dst = target%euclideanDist(nodePool(childIdx))
                                case ('manhattan')
                                    dst = target%manhattanDist(nodePool(childIdx))
                                case ('chebyshev')
                                    dst = target%chebyshevDist(nodePool(childIdx))
                                case default
                                    error stop "rNN_BLT: unknown metric"
                            end select
                            if (dst + childRad .le. radius) then
                                call addSubtree(nodePool, childIdx, res, arrSize)
                            else if (dst - childRad .le. radius) then
                                stackTop        = stackTop + 1_int64
                                stack(stackTop) = childIdx
                            end if
                        end if
                    end do
                end if 
            end procedure rNN_BLT


            !> Walks a subtree and appends every node to res, with no distance checks.
            !! Called when the subtree's bounding sphere lies entirely within the search sphere,
            !! so all nodes are guaranteed results.
            !! @param[in]    nodePool  the tree's node pool
            !! @param[in]    root      pool index of the subtree root; 0 is a no-op
            !! @param[inout] res       result buffer; doubled in size when full
            !! @param[inout] arrSize   number of results written into res so far
            recursive subroutine addSubtree(nodePool, root, res, arrSize)
                type(NdNode),                  intent(in)    :: nodePool(:)
                integer(int64),                intent(in)    :: root
                type(NdNodePtr), allocatable,  intent(inout) :: res(:)
                integer,                       intent(inout) :: arrSize
                integer                                      :: i
                type(NdNode),    pointer                     :: copy
                type(NdNodePtr), allocatable                 :: tmp(:)

                if (root .eq. 0_int64) then 
                    return 
                else 
                    call addSubtree(nodePool, nodePool(root)%children(1), res, arrSize)

                    if (size(res) .eq. arrSize) then 
                        allocate(tmp(2*size(res)))
                        do i = 1, arrSize
                            tmp(i)%p => res(i)%p
                            res(i)%p => null() 
                        end do
                        call move_alloc(from=tmp, to=res)
                    end if
                    arrSize = arrSize + 1_int64
                    allocate(copy, source=nodePool(root))
                    res(arrSize)%p => copy

                    call addSubtree(nodePool, nodePool(root)%children(2), res, arrSize)
                end if
            end subroutine addSubtree

end submodule BallTreeRnn