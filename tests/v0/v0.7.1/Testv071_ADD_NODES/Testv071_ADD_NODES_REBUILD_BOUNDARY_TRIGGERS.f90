program Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildBoundaryTriggersRebuild()
    contains
        !> One over boundary: add N > ratio*pop nodes with 0 mods -> triggers rebuild.
        !! Build 4, ratio=0.5: 0.5*4=2.0; add 3: 0+3>2.0 is TRUE -> rebuild, numMods=0.
        !! All 7 nodes findable after rebuild.
        subroutine rebuildBoundaryTriggersRebuild()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 3) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 60.0_real64, &
                70.0_real64, 70.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: numMods, pop
            logical                    :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%setRebuildRatio(0.5_real64)
            call t%addNodes(new_coords)

            numMods = t%getNumMods()
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS ---'
                write(*, '(A,I0)') 'expected numMods=0 (rebuild triggered), got: ', numMods
                stop 1
            end if
            if (pop .ne. 7_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS ---'
                write(*, '(A,I0)') 'expected pop=7, got: ', pop
                stop 1
            end if
            if (.not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS ---'
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if

            res = t%rNN_Centroid([5.0_real64, 5.0_real64], 100.0_real64, metric='euclidean')
            if (size(res) .ne. 7) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS ---'
                write(*, '(A,I0)') 'expected all 7 nodes findable after rebuild, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildBoundaryTriggersRebuild
end program Testv071_ADD_NODES_REBUILD_BOUNDARY_TRIGGERS
