program Testv021_TREE_GETTERS_GET_POP_FREED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getPopFreed()
    contains

        !> getPop must return 0 after build then destroy.
        subroutine getPopFreed()
            type(KdTree)     :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64,               &
                 3.0_real64, 4.0_real64,               &
                 5.0_real64, 6.0_real64], [2, 3])
            integer(int64) :: n

            call t%build(coords)
            call t%destroy()
            n = t%getPop()

            if (n .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_POP_FREED ---'
                write(*, '(A,I0)') 'expected: getPop = 0, got: ', n
                stop 1
            end if

        end subroutine getPopFreed

end program Testv021_TREE_GETTERS_GET_POP_FREED
