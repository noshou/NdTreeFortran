program Testv021_TREE_GETTERS_GET_DIM_FREED_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: int64
    implicit none
    call getDimFreedUninitialized()
    contains

        !> getDim must return 0 after destroy is called on a never-built tree.
        subroutine getDimFreedUninitialized()
            type(KdTree)     :: t
            integer(int64) :: d

            call t%destroy()
            d = t%getDim()

            if (d .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_DIM_FREED_UNINITIALIZED ---'
                write(*, '(A,I0)') 'expected: getDim = 0, got: ', d
                stop 1
            end if

        end subroutine getDimFreedUninitialized

end program Testv021_TREE_GETTERS_GET_DIM_FREED_UNINITIALIZED
