program Testv030_ADD_NODES_GET_POP_SINGLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call addNodesGetPopSingle()
    contains
        !> getPop must return initial + 1 after adding one node.
        subroutine addNodesGetPopSingle()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                 0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64)   :: new_coords(2, 1) = reshape([5.0_real64, 5.0_real64], [2, 1])
            integer(int64) :: pop

            call t%build(init_coords)
            call t%addNodes(new_coords)
            pop = t%getPop()

            if (pop .ne. 5_int64) then
                write(*, '(A)')         '--- Testv030_ADD_NODES_GET_POP_SINGLE ---'
                write(*, '(A,I0,A,I0)') 'expected pop = 5, got: ', pop
                stop 1
            end if
        end subroutine addNodesGetPopSingle
end program Testv030_ADD_NODES_GET_POP_SINGLE
