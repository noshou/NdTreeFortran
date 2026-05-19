program Testv021_TREE_GETTERS_GET_POP_POPULATED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getPopPopulated()
    contains

        !> getPop must return n after building with n points.
        subroutine getPopPopulated()
            integer, parameter :: N = 5
            type(KdTree)         :: t
            real(real64)       :: coords(2, N) = reshape( &
                [1.0_real64, 2.0_real64,                  &
                 3.0_real64, 4.0_real64,                  &
                 5.0_real64, 6.0_real64,                  &
                 7.0_real64, 8.0_real64,                  &
                 9.0_real64, 0.0_real64], [2, N])
            integer(int64) :: pop

            call t%build(coords)
            pop = t%getPop()

            if (pop .ne. N) then
                write(*, '(A)')                      '--- Testv021_TREE_GETTERS_GET_POP_POPULATED ---'
                write(*, '(A,I0,A,I0)') 'expected: getPop = ', N, ', got: ', pop
                stop 1
            end if

        end subroutine getPopPopulated

end program Testv021_TREE_GETTERS_GET_POP_POPULATED
