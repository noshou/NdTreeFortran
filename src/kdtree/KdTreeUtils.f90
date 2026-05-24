submodule(NdTreeFortran) KdTreePrint
    use iso_fortran_env, only: output_unit, int64, real64
    implicit none
    contains

        module procedure printKdTree
            integer :: u
            u = output_unit
            if (present(unit)) u = unit
            if (this%rootIdx .ne. 0_int64) then
                call printKdNode(this%nodePool, this%rootIdx, 0_int64, u)
            else
                write(u, '(A)') '**empty tree**'
            end if
        end procedure printKdTree

        recursive subroutine printKdNode(nodePool, idx, depth, u)
            type(NdNode),   intent(in) :: nodePool(:)
            integer(int64), intent(in) :: idx, depth
            integer,        intent(in) :: u
            integer(int64)             :: i, d

            do d = 1, int(depth)
                write(u, '(A)', advance='no') '  '
            end do
            write(u, '(A,I0,A)', advance='no') '[axis=', int(nodePool(idx)%nodeParams(1), int64), '] ('
            do i = 1, size(nodePool(idx)%coords)
                if (i > 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') nodePool(idx)%coords(i)
            end do
            write(u, '(A)') ')'

            if (nodePool(idx)%children(1) .ne. 0_int64) &
                call printKdNode(nodePool, nodePool(idx)%children(1), depth + 1_int64, u)
            if (nodePool(idx)%children(2) .ne. 0_int64) &
                call printKdNode(nodePool, nodePool(idx)%children(2), depth + 1_int64, u)
        end subroutine printKdNode

        module procedure saxs
            splitAxis = mod(a, k) + 1_int64
        end procedure saxs

        module procedure getSplitAxis
            if (.not. this%isMember(node)) &
                error stop "getSplitAxis: node not a member of this tree"
            axis = int(node%nodeParams(1), int64)
        end procedure getSplitAxis

        module procedure finalizerKDT
            call this%destroy()
        end procedure finalizerKDT

end submodule KdTreePrint
