program Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call setRebuildRatioForcesRebuild()
    contains

        !> With rebuildRatio=0.01: 0+2 > 0.01*4=0.04 -> TRUE -> rebuild.
        !! numMods=0 after; all 6 nodes findable; full tree state valid.
        subroutine setRebuildRatioForcesRebuild()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 60.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:)
            real(real64)               :: ratio
            integer(int64)             :: numMods, pop
            logical                    :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%setRebuildRatio(0.01_real64)
            call t%addNodes(new_coords)

            ratio   = t%getRebuildRatio()
            numMods = t%getNumMods()
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (ratio .ne. 0.01_real64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD ---'
                write(*, '(A,F8.4)') 'ratio changed unexpectedly, got: ', ratio
                stop 1
            end if
            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD ---'
                write(*, '(A,I0)') 'expected numMods=0 after forced rebuild, got: ', numMods
                stop 1
            end if
            if (pop .ne. 6_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD ---'
                write(*, '(A,I0)') 'expected pop=6, got: ', pop
                stop 1
            end if
            if (.not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD ---'
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if

            res = t%rNN_Centroid([5.0_real64, 5.0_real64], 100.0_real64, metric='euclidean')
            if (size(res) .ne. 6) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD ---'
                write(*, '(A,I0)') 'expected all 6 nodes findable after rebuild, got: ', size(res)
                stop 1
            end if
        end subroutine setRebuildRatioForcesRebuild
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_FORCES_REBUILD
