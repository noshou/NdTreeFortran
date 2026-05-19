program Testv021_TREE_GETTERS_GET_DIM_POPULATED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getDimPopulated()
    contains

        !> getDim must return k after building with k-dimensional points.
        subroutine getDimPopulated()
            type(KdTree)     :: t
            real(real64)   :: coords(4, 3) = reshape( &
                [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                 5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64, &
                 9.0_real64, 0.0_real64, 1.0_real64, 2.0_real64], [4, 3])
            integer(int64) :: d

            call t%build(coords)
            d = t%getDim()

            if (d .ne. 4_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_DIM_POPULATED ---'
                write(*, '(A,I0)') 'expected: getDim = 4, got: ', d
                stop 1
            end if

        end subroutine getDimPopulated

end program Testv021_TREE_GETTERS_GET_DIM_POPULATED
