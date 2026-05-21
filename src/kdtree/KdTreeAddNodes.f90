submodule(NdTreeFortran) KdTreeAddNodes
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure addNodesKDT

            integer(int64) :: i, dim, numNodeToAdd, currIdx, currAxis

            numNodeToAdd = int(size(coordsList, 2), int64)
            dim = this%dim

            ! no need for rebuild; insert new nodes at leaves.
            ! rootIdx=0 (empty tree) can never reach this branch; the rebuild condition
            ! simplifies to numNodeToAdd > 0, which is always true when pop was zero.
            do i = this%pop-numNodeToAdd + 1, this%pop
                currIdx = this%rootIdx
                do
                    currAxis = int(this%nodePool(currIdx)%nodeParams(1), int64)
                    if (this%nodePool(i)%coords(currAxis) .le. this%nodePool(currIdx)%coords(currAxis)) then
                        if (this%nodePool(currIdx)%children(1) .eq. 0_int64) then
                            this%nodePool(currIdx)%children(1) = i
                            allocate(this%nodePool(i)%nodeParams(1))
                            allocate(this%nodePool(i)%children(2))
                            this%nodePool(i)%nodeParams(1) = real(this%saxs(currAxis, dim), real64)
                            this%nodePool(i)%children(:)   = 0_int64
                            exit
                        else
                            currIdx = this%nodePool(currIdx)%children(1)
                        end if
                    else
                        if (this%nodePool(currIdx)%children(2) .eq. 0_int64) then
                            this%nodePool(currIdx)%children(2) = i
                            allocate(this%nodePool(i)%nodeParams(1))
                            allocate(this%nodePool(i)%children(2))
                            this%nodePool(i)%nodeParams(1) = real(this%saxs(currAxis, dim), real64)
                            this%nodePool(i)%children(:)   = 0_int64
                            exit
                        else
                            currIdx = this%nodePool(currIdx)%children(2)
                        end if
                    end if
                end do
            end do

        end procedure addNodesKDT

end submodule KdTreeAddNodes