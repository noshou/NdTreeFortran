program Testv021_TREE_GETTERS_GET_POP_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: int64
    implicit none
    call getPopUninitialized()
    contains

        !> getPop must return 0 before build is called.
        subroutine getPopUninitialized()
            type(KdTree)     :: t
            integer(int64) :: n

            n = t%getPop()

            if (n .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_POP_UNINITIALIZED ---'
                write(*, '(A,I0)') 'expected: getPop = 0, got: ', n
                stop 1
            end if

        end subroutine getPopUninitialized

end program Testv021_TREE_GETTERS_GET_POP_UNINITIALIZED
