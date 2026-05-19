program Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_BUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call numModsZeroAfterBuild()
    contains
        !> getNumMods returns 0 immediately after build; getRebuildRatio is 0.25; all state valid.
        subroutine numModsZeroAfterBuild()
            type(KdTree)   :: t
            real(real64) :: coords(2, 5) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 5])
            integer(int64) :: numMods, pop, dim
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(coords)
            numMods = t%getNumMods() 
            ratio   = t%getRebuildRatio()
            pop     = t%getPop()
            dim     = t%getDim()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_BUILD ---'
                write(*, '(A,I0)') 'expected numMods=0 after build, got: ', numMods
                stop 1
            end if
            if (ratio .ne. 0.25_real64 .or. pop .ne. 5_int64 .or. dim .ne. 2_int64 .or. &
                .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')      '--- Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_BUILD ---'
                write(*, '(A,F8.4)') 'ratio:       ', ratio
                write(*, '(A,I0)')   'pop:         ', pop
                write(*, '(A,I0)')   'dim:         ', dim
                write(*, '(A,L2)')   'initialized: ', isInit
                write(*, '(A,L2)')   'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)')   'root:        ', rootAssoc
                stop 1
            end if
        end subroutine numModsZeroAfterBuild
end program Testv030_ADD_NODES_NUM_MODS_ZERO_AFTER_BUILD
