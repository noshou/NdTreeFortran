submodule(NdTreeFortran) QuOcTreeRnn
    implicit none
    contains
        module procedure rNN_QOT
            integer(int64), allocatable  :: stack(:), stmp(:)
            integer(int64)               :: stackTop, stackSize, node, i
            type(NdNodePtr), allocatable :: tmp(:)
            real(real64),    allocatable :: lo(:), hi(:)
            type(NdNode), pointer        :: copy
            logical                      :: withinRadius

            if (currIdx .eq. 0_int64) return

            stackSize = 64_int64
            allocate(stack(stackSize))
            stackTop  = 1_int64
            stack(1)  = currIdx

            do while (stackTop > 0_int64)
                
                ! pop node
                node     = stack(stackTop)
                stackTop = stackTop - 1_int64

                ! check if node is within search sphere
                select case (metric)
                    case ('euclidean'); withinRadius = target%euclideanDist(nodePool(node)) .le. radius
                    case ('manhattan'); withinRadius = target%manhattanDist(nodePool(node)) .le. radius
                    case ('chebyshev'); withinRadius = target%chebyshevDist(nodePool(node)) .le. radius
                end select

                ! append to found nodes if within search sphere
                if (withinRadius) then
                    if (size(res, kind=int64) .eq. arrSize) then
                        allocate(tmp(2*size(res)))
                        do i = 1_int64, arrSize
                            tmp(i)%p => res(i)%p
                            res(i)%p => null()
                        end do
                        call move_alloc(from=tmp, to=res)
                    end if
                    arrSize = arrSize + 1_int64
                    allocate(copy, source=nodePool(node))
                    res(arrSize)%p => copy
                end if
                
                ! grow stack if needed before pushing up to 8 children
                if (stackTop + 8 > stackSize) then
                    stackSize = stackSize * 2_int64
                    allocate(stmp(stackSize))
                    stmp(1:stackTop) = stack(1:stackTop)
                    call move_alloc(from=stmp, to=stack)
                end if

                ! check condition for each child
                do i = 1_int64, size(nodePool(node)%children)
                    
                    ! null child -> skip
                    if (nodePool(node)%children(i) .eq. 0_int64) then 
                        cycle 
                    
                    ! explore children
                    else 

                        ! get coordinates of bounding box
                        select case (this%type)
                            case (QOT_QUT)
                                allocate(lo, source=nodePool(nodePool(node)%children(i))%nodeParams(1:2))
                                allocate(hi, source=nodePool(nodePool(node)%children(i))%nodeParams(5:6))
                            case (QOT_OCT)
                                allocate(lo, source=nodePool(nodePool(node)%children(i))%nodeParams(1:3))
                                allocate(hi, source=nodePool(nodePool(node)%children(i))%nodeParams(22:24))
                        end select 

                        ! case 1:
                        ! if bboxMaxDist <= radius, that means bbox is fully within search sphere;
                        ! we can therefore bulk add this subtree
                        ! case 2:
                        ! if bboxMinDist <= radius, that means bbox is partially intersected by search sphere;
                        ! we push the root of this subtree to the stack
                        ! case 3:
                        ! if neither of the above are true, bbox is not intersected by search sphere;
                        ! we prune this subtree
                        if (target%bboxMaxDist(lo, hi, metric) <= radius) then 
                            call addSubtree(nodePool, nodePool(node)%children(i), res, arrSize)
                        else if (target%bboxMinDist(lo, hi, metric) <= radius) then
                            stackTop        = stackTop + 1_int64
                            stack(stackTop) = nodePool(node)%children(i)
                        end if
                    end if
                    deallocate(lo)
                    deallocate(hi)
                end do
            end do 

        end procedure rNN_QOT

        !> Walks a subtree and appends every node to res, with no distance checks.
        !! Called when the subtree's bounding box lies entirely within the search sphere,
        !! so all nodes are guaranteed results.
        !! @param[in]    nodePool  the tree's node pool
        !! @param[in]    root      pool index of the subtree root; 0 is a no-op
        !! @param[inout] res       result buffer; doubled in size when full
        !! @param[inout] arrSize   number of results written into res so far
        subroutine addSubtree(nodePool, root, res, arrSize)
            type(NdNode),                  intent(in)    :: nodePool(:)
            integer(int64),                intent(in)    :: root
            type(NdNodePtr), allocatable,  intent(inout) :: res(:)
            integer(int64),                intent(inout) :: arrSize
            integer(int64)                               :: i, node, stackTop, stackSize, c
            integer(int64),  allocatable                 :: stack(:), stmp(:)
            type(NdNode),    pointer                     :: copy
            type(NdNodePtr), allocatable                 :: tmp(:)

            if (root .eq. 0_int64) return

            stackSize = 64_int64
            allocate(stack(stackSize))
            stackTop = 1_int64
            stack(1) = root

            do while (stackTop > 0_int64)
                node     = stack(stackTop)
                stackTop = stackTop - 1_int64

                ! append node to found nodes
                if (size(res, kind=int64) .eq. arrSize) then
                    allocate(tmp(2*size(res)))
                    do i = 1_int64, arrSize
                        tmp(i)%p => res(i)%p
                        res(i)%p => null()
                    end do
                    call move_alloc(from=tmp, to=res)
                end if
                arrSize = arrSize + 1_int64
                allocate(copy, source=nodePool(node))
                res(arrSize)%p => copy

                ! grow stack if needed before pushing up to 8 children
                if (stackTop + 8 > stackSize) then
                    stackSize = stackSize * 2_int64
                    allocate(stmp(stackSize))
                    stmp(1:stackTop) = stack(1:stackTop)
                    call move_alloc(from=stmp, to=stack)
                end if
                
                do c = 1_int64, size(nodePool(node)%children)
                    if (nodePool(node)%children(c) .ne. 0_int64) then
                        stackTop        = stackTop + 1_int64
                        stack(stackTop) = nodePool(node)%children(c)
                    end if
                end do

            end do
        end subroutine addSubtree

end submodule QuOcTreeRnn 