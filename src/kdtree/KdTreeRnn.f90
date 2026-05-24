submodule(NdTreeFortran) KdTreeRnn
    implicit none 
    contains
        module procedure rNN_KDT
            integer(int64), allocatable  :: stack(:), stmp(:)
            integer(int64)               :: stackTop, stackSize, node
            real(kind=real64)            :: delta
            type(NdNodePtr), allocatable :: tmp(:)
            integer(int64)               :: axis
            integer(int64)               :: i
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
                    if (size(res) .eq. arrSize) then
                        allocate(tmp(2*size(res)))
                        do i = 1, arrSize
                            tmp(i)%p => res(i)%p
                            res(i)%p => null()
                        end do
                        call move_alloc(from=tmp, to=res)
                    end if
                    arrSize = arrSize + 1
                    allocate(copy, source=nodePool(node))
                    res(arrSize)%p => copy
                end if
                
                axis  = int(nodePool(node)%nodeParams(1), int64)
                delta = target%coords(axis) - nodePool(node)%coords(axis)

                ! grow stack if needed before pushing up to 2 children
                if (stackTop + 2 > stackSize) then
                    stackSize = stackSize * 2_int64
                    allocate(stmp(stackSize))
                    stmp(1:stackTop) = stack(1:stackTop)
                    call move_alloc(from=stmp, to=stack)
                end if

                if (delta < 0.0_real64) then
                    if (nodePool(node)%children(1) .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%children(1)
                    end if
                    if (-delta .le. radius .and. nodePool(node)%children(2) .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%children(2)
                    end if
                else
                    if (nodePool(node)%children(2) .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%children(2)
                    end if
                    if (delta .le. radius .and. nodePool(node)%children(1) .ne. 0_int64) then
                        stackTop         = stackTop + 1_int64
                        stack(stackTop)  = nodePool(node)%children(1)
                    end if
                end if
            end do

        end procedure rNN_KDT
end submodule KdTreeRnn
