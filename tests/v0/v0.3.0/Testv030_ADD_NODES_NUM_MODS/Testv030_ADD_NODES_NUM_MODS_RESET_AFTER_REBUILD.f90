program Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call numModsResetAfterRebuild()
    contains
        !> One leaf insert raises numMods to 1; a second add crosses the threshold and triggers
        !! a rebuild, resetting numMods to 0. Pop and full state are verified at each step.
        !! Build 4, ratio=0.25.
        !!   add 1: 0+1 > 0.25*4=1.0 -> FALSE, leaf, mods=1, pop=5
        !!   add 2: 1+2 > 0.25*5=1.25 -> TRUE, rebuild, mods=0, pop=7
        subroutine numModsResetAfterRebuild()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64) :: leaf_coord(2, 1)  = reshape([100.0_real64, 100.0_real64], [2, 1])
            real(real64) :: rebuild_coords(2, 2) = reshape( &
                [200.0_real64, 200.0_real64, 300.0_real64, 300.0_real64], [2, 2])
            integer(int64) :: numMods, pop
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)

            ! leaf insert
            call t%addNodes(leaf_coord)
            numMods = t%getNumMods() 
            pop     = t%getPop()
            if (numMods .ne. 1_int64 .or. pop .ne. 5_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD ---'
                write(*, '(A)')    'after leaf insert:'
                write(*, '(A,I0)') 'expected numMods=1, got: ', numMods
                write(*, '(A,I0)') 'expected pop=5,     got: ', pop
                stop 1
            end if

            ! rebuild: 1+2 > 0.25*5=1.25 -> 3 > 1.25 -> TRUE
            call t%addNodes(rebuild_coords)
            numMods = t%getNumMods() 
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)
            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD ---'
                write(*, '(A,I0)') 'expected numMods=0 after rebuild, got: ', numMods
                stop 1
            end if
            if (pop .ne. 7_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD ---'
                write(*, '(A,I0)') 'expected pop=7, got: ', pop
                stop 1
            end if
            if (.not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD ---'
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if
        end subroutine numModsResetAfterRebuild
end program Testv030_ADD_NODES_NUM_MODS_RESET_AFTER_REBUILD
