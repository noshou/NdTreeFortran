program Testv021_TREE_GETTERS_GET_DIM_FREED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getDimFreed()
    contains

        !> getDim must return 0 after build then destroy.
        subroutine getDimFreed()
            type(KdTree)     :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64,               &
                 3.0_real64, 4.0_real64,               &
                 5.0_real64, 6.0_real64], [2, 3])
            integer(int64) :: d

            call t%build(coords)
            call t%destroy()
            d = t%getDim()

            if (d .ne. 0_int64) then
                write(*, '(A)')     '--- Testv021_TREE_GETTERS_GET_DIM_FREED ---'
                write(*, '(A,I0)') 'expected: getDim = 0, got: ', d
                stop 1
            end if

        end subroutine getDimFreed

end program Testv021_TREE_GETTERS_GET_DIM_FREED
