program Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call numModsZeroAfterDestroyBuild()
    contains
        !> Leaf inserts raise numMods; destroy resets it to 0; rebuild keeps it at 0.
        !! Also verifies getRebuildRatio resets to 0.25 after destroy.
        subroutine numModsZeroAfterDestroyBuild()
            type(KdTree)   :: t
            real(real64) :: coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64) :: new_coords(2, 1) = reshape([50.0_real64, 0.0_real64], [2, 1])
            integer(int64) :: numMods, pop
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(coords)
            call t%setRebuildRatio(0.9_real64)
            call t%addNodes(new_coords)

            ! verify non-zero mods before destroy
            numMods = t%getNumMods() 
            if (numMods .ne. 1_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD ---'
                write(*, '(A,I0)') 'expected numMods=1 before destroy, got: ', numMods
                stop 1
            end if

            call t%destroy()
            call t%build(coords)

            numMods = t%getNumMods() 
            ratio   = t%getRebuildRatio()
            pop     = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD ---'
                write(*, '(A,I0)') 'expected numMods=0 after destroy+rebuild, got: ', numMods
                stop 1
            end if
            if (ratio .ne. 0.25_real64) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD ---'
                write(*, '(A,F8.4)') 'expected ratio reset to 0.25 after destroy, got: ', ratio
                stop 1
            end if
            if (pop .ne. 8_int64 .or. .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD ---'
                write(*, '(A,I0)') 'pop:         ', pop
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if
        end subroutine numModsZeroAfterDestroyBuild
end program Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_DESTROY_BUILD
