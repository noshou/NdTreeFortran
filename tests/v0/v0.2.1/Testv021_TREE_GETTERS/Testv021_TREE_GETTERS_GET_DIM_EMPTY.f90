program Testv021_TREE_GETTERS_GET_DIM_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call treeGettersGetDimEmpty()
    contains

        !> getDim must return k after build with k-dimensional coords and zero points.
        subroutine treeGettersGetDimEmpty()
            type(KdTree)     :: t
            real(real64)   :: coords(3, 0)
            integer(int64) :: d

            call t%build(coords)
            d = t%getDim()

            if (d .ne. 3_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_DIM_EMPTY ---'
                write(*, '(A,I0)') 'expected: getDim = 3, got: ', d
                stop 1
            end if

        end subroutine treeGettersGetDimEmpty

end program Testv021_TREE_GETTERS_GET_DIM_EMPTY
