!> Expected-fail: setRebuildRatio(0.0) must error stop (ratio must be > 0).
!! Registered with WILL_FAIL in CTest.
program Testv030_ADD_NODES_SET_REBUILD_RATIO_ZERO
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call setRebuildRatioZero()
    contains
        subroutine setRebuildRatioZero()
            type(KdTree)   :: t
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            call t%build(coords)
            call t%setRebuildRatio(0.0_real64)
            write(*, '(A)') '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_ZERO ---'
            write(*, '(A)') 'expected error stop, but setRebuildRatio returned normally'
        end subroutine setRebuildRatioZero
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_ZERO
