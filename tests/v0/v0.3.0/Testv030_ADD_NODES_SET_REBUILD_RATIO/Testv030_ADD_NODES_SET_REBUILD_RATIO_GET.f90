program Testv030_ADD_NODES_SET_REBUILD_RATIO_GET
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call setRebuildRatioGet()
    contains
        !> getRebuildRatio returns 0.25 by default; setRebuildRatio changes it; all tree state consistent.
        !! Also verifies ratio persists across addNodes (not reset by addNodes).
        subroutine setRebuildRatioGet()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64) :: new_coords(2, 1) = reshape([50.0_real64, 50.0_real64], [2, 1])
            real(real64)   :: ratio
            integer(int64) :: numMods, pop, dim
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)

            ! default ratio must be 0.25
            ratio = t%getRebuildRatio()
            if (ratio .ne. 0.25_real64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_GET ---'
                write(*, '(A,F8.4)') 'expected default getRebuildRatio = 0.25, got: ', ratio
                stop 1
            end if

            ! change ratio and verify all state
            call t%setRebuildRatio(0.6_real64)
            ratio      = t%getRebuildRatio()
            numMods    = t%getNumMods()
            pop        = t%getPop()
            dim        = t%getDim()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)
            if (ratio .ne. 0.6_real64 .or. numMods .ne. 0_int64 .or. pop .ne. 8_int64 .or.       &
                dim .ne. 2_int64 .or. .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc &
                ) then
                    write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_GET ---'
                    write(*, '(A,F8.4)') 'expected ratio = 0.6, got: ', ratio
                    write(*, '(A,I0)')   'expected numMods = 0, got: ', numMods
                    write(*, '(A,I0)')   'expected pop = 8, got: ', pop
                    write(*, '(A,I0)')   'expected dim = 2, got: ', dim
                    write(*, '(A,L2)')   'initialized:   ', isInit
                    write(*, '(A,L2)')   'nodePool:      ', nodePoolAssoc
                    write(*, '(A,L2)')   'root:          ', rootAssoc
                    stop 1
            end if

            ! ratio must survive a leaf-insert addNodes (0+1 > 0.6*8=4.8 => 1>4.8 => FALSE)
            call t%addNodes(new_coords)
            ratio   = t%getRebuildRatio()
            numMods = t%getNumMods()
            pop     = t%getPop()
            if (ratio .ne. 0.6_real64 .or. numMods .ne. 1_int64 .or. pop .ne. 9_int64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_GET ---'
                write(*, '(A,F8.4)') 'ratio changed after addNodes: ', ratio
                write(*, '(A,I0)')   'expected numMods = 1, got: ', numMods
                write(*, '(A,I0)')   'expected pop = 9, got: ', pop
                stop 1
            end if
        end subroutine setRebuildRatioGet
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_GET
