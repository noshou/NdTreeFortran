!> Expected-fail: setRebuildRatio(1.0) must error stop (ratio must be < 1).
!! Registered with WILL_FAIL in CTest.
program Testv030_ADD_NODES_SET_REBUILD_RATIO_ONE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call setRebuildRatioOne()
    contains
        subroutine setRebuildRatioOne()
            type(KdTree)   :: t
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            call t%build(coords)
            call t%setRebuildRatio(1.0_real64)
            write(*, '(A)') '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_ONE ---'
            write(*, '(A)') 'expected error stop, but setRebuildRatio returned normally'
        end subroutine setRebuildRatioOne
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_ONE
