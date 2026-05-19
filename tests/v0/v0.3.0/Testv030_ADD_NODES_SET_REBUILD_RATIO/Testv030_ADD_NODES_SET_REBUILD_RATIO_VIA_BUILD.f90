program Testv030_ADD_NODES_SET_REBUILD_RATIO_VIA_BUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call setRebuildRatioViaBuild()
    contains
        !> build(rebuildRatio=0.75) sets the ratio; getRebuildRatio returns 0.75.
        !! Also verifies ratio resets to 0.25 after destroy, and a second build without
        !! the param leaves ratio at 0.25 (the post-destroy default).
        subroutine setRebuildRatioViaBuild()
            type(KdTree)   :: t
            real(real64) :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: ratio
            integer(int64) :: pop, dim
            logical        :: isInit

            call t%build(coords, rebuildRatio=0.75_real64)
            ratio = t%getRebuildRatio()
            pop   = t%getPop()
            dim   = t%getDim()
            call t%getInitState(isInit)
            if (ratio .ne. 0.75_real64 .or. pop .ne. 4_int64 .or. &
                dim .ne. 2_int64 .or. .not. isInit) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_VIA_BUILD ---'
                write(*, '(A,F8.4)') 'expected ratio = 0.75 from build param, got: ', ratio
                write(*, '(A,I0)')   'expected pop = 4, got: ', pop
                write(*, '(A,I0)')   'expected dim = 2, got: ', dim
                write(*, '(A,L2)')   'initialized: ', isInit
                stop 1
            end if

            ! destroy resets ratio to 0.25; rebuild without param keeps 0.25
            call t%destroy()
            call t%build(coords)
            ratio = t%getRebuildRatio()
            if (ratio .ne. 0.25_real64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_VIA_BUILD ---'
                write(*, '(A,F8.4)') 'expected ratio = 0.25 after destroy+rebuild, got: ', ratio
                stop 1
            end if
        end subroutine setRebuildRatioViaBuild
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_VIA_BUILD
