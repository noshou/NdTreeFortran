program Testv021_TREE_GETTERS_GET_DIM_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: int64
    implicit none
    call getDimUninitialized()
    contains

        !> getDim must return 0 before build is called.
        subroutine getDimUninitialized()
            type(KdTree)     :: t
            integer(int64) :: d

            d = t%getDim()

            if (d .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_DIM_UNINITIALIZED ---'
                write(*, '(A,I0)') 'expected: getDim = 0, got: ', d
                stop 1
            end if

        end subroutine getDimUninitialized

end program Testv021_TREE_GETTERS_GET_DIM_UNINITIALIZED
