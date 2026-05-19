program Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call setRebuildRatioPreventsRebuild()
    contains
        !> Default ratio=0.25 would rebuild when adding 3 to a 4-node tree (0+3>0.25*4=1.0 -> TRUE).
        !! With ratio=0.9: 0+3>0.9*4=3.6 is FALSE -> leaf insert; numMods=3.
        subroutine setRebuildRatioPreventsRebuild()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64) :: new_coords(2, 3) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 60.0_real64, &
                70.0_real64, 70.0_real64], [2, 3])
            real(real64)   :: ratio
            integer(int64) :: numMods, pop
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%setRebuildRatio(0.9_real64)
            call t%addNodes(new_coords)

            ratio   = t%getRebuildRatio()
            numMods = t%getNumMods()
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (ratio .ne. 0.9_real64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD ---'
                write(*, '(A,F8.4)') 'ratio changed unexpectedly, got: ', ratio
                stop 1
            end if
            if (numMods .ne. 3_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD ---'
                write(*, '(A,I0)') 'expected numMods=3 (leaf insert), got: ', numMods
                stop 1
            end if
            if (pop .ne. 7_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD ---'
                write(*, '(A,I0)') 'expected pop=7, got: ', pop
                stop 1
            end if
            if (.not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD ---'
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if
        end subroutine setRebuildRatioPreventsRebuild
end program Testv030_ADD_NODES_SET_REBUILD_RATIO_PREVENTS_REBUILD
