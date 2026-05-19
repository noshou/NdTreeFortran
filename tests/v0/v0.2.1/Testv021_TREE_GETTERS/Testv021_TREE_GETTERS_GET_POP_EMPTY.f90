program Testv021_TREE_GETTERS_GET_POP_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getPopEmpty()
    contains

        !> getPop must return 0 after build with zero points.
        subroutine getPopEmpty()
            type(KdTree)     :: t
            real(real64)   :: coords(3, 0)
            integer(int64) :: n

            call t%build(coords)
            n = t%getPop()

            if (n .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_POP_EMPTY ---'
                write(*, '(A,I0)') 'expected: getPop = 0, got: ', n
                stop 1
            end if

        end subroutine getPopEmpty

end program Testv021_TREE_GETTERS_GET_POP_EMPTY
