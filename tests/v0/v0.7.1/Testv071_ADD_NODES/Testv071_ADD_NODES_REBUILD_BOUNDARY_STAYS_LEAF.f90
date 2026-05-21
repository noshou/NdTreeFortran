program Testv071_ADD_NODES_REBUILD_BOUNDARY_STAYS_LEAF
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildBoundaryStaysLeaf()
    contains
        !> Exact boundary: add nodes equal to ratio*pop with 0 mods -> stays leaf insert.
        !! Build 4, ratio=0.5: 0.5*4=2.0; add 2: 0+2>2.0 is FALSE -> leaf, numMods=2.
        subroutine rebuildBoundaryStaysLeaf()
            type(BallTree) :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64) :: new_coords(2, 2) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 60.0_real64], [2, 2])
            integer(int64) :: numMods, pop
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%setRebuildRatio(0.5_real64)
            call t%addNodes(new_coords)

            numMods = t%getNumMods()
            ratio   = t%getRebuildRatio()
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (numMods .ne. 2_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_STAYS_LEAF ---'
                write(*, '(A,I0)') 'expected numMods=2 (leaf insert at boundary), got: ', numMods
                stop 1
            end if
            if (pop .ne. 6_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_STAYS_LEAF ---'
                write(*, '(A,I0)') 'expected pop=6, got: ', pop
                stop 1
            end if
            if (ratio .ne. 0.5_real64 .or. .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')      '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_STAYS_LEAF ---'
                write(*, '(A,F8.4)') 'ratio:       ', ratio
                write(*, '(A,L2)')   'initialized: ', isInit
                write(*, '(A,L2)')   'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)')   'root:        ', rootAssoc
                stop 1
            end if
        end subroutine rebuildBoundaryStaysLeaf
end program Testv071_ADD_NODES_REBUILD_BOUNDARY_STAYS_LEAF
