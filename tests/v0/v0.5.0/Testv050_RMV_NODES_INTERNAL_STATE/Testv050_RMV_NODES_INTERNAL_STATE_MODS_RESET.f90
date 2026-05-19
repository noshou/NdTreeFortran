program Testv050_RMV_NODES_INTERNAL_STATE_MODS_RESET
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesInternalStateModsReset()
    contains
        !> rmvNodes always triggers a rebuild, so getNumMods resets to 0 afterward.
        !! Build 8 nodes with ratio=0.9. Add 2 leaf nodes (mods=2). rmvNodes one
        !! node -> rebuild -> mods=0.
        subroutine rmvNodesInternalStateModsReset()
            type(KdTree)   :: t
            real(real64)   :: init(2, 8) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, &
                 4.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, &
                 6.0_real64, 0.0_real64, 7.0_real64, 0.0_real64], [2, 8])
            real(real64)   :: extra(2, 2) = reshape( &
                [20.0_real64, 0.0_real64, 30.0_real64, 0.0_real64], [2, 2])
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: numMods

            call t%build(init)
            call t%setRebuildRatio(0.9_real64)
            call t%addNodes(extra)
            numMods = t%getNumMods()
            if (numMods .ne. 2_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_MODS_RESET ---'
                write(*, '(A,I0)') 'expected numMods=2 before rmvNodes, got: ', numMods
                stop 1
            end if

            numRmv  = t%rmvNodes(coordsList=query)
            numMods = t%getNumMods()
            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_MODS_RESET ---'
                write(*, '(A,I0)') 'expected numMods=0 after rmvNodes rebuild, got: ', numMods
                stop 1
            end if
        end subroutine rmvNodesInternalStateModsReset
end program Testv050_RMV_NODES_INTERNAL_STATE_MODS_RESET
