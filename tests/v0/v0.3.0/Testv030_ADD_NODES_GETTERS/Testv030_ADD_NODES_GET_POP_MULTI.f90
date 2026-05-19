program Testv030_ADD_NODES_GET_POP_MULTI
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call addNodesGetPopMulti()
    contains
        !> getPop must return initial + n after adding n nodes at once.
        subroutine addNodesGetPopMulti()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                 0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64)   :: new_coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 3])
            integer(int64) :: pop

            call t%build(init_coords)
            call t%addNodes(new_coords)
            pop = t%getPop()

            if (pop .ne. 7_int64) then
                write(*, '(A)')         '--- Testv030_ADD_NODES_GET_POP_MULTI ---'
                write(*, '(A,I0,A,I0)') 'expected pop = 7, got: ', pop
                stop 1
            end if
        end subroutine addNodesGetPopMulti
end program Testv030_ADD_NODES_GET_POP_MULTI
