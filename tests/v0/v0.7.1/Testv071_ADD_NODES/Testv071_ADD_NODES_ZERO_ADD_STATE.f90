program Testv071_ADD_NODES_ZERO_ADD_STATE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call zeroAddState()
    contains
        !> addNodes with a zero-column coordsList silently succeeds: pop, numMods,
        !! getRebuildRatio, and all tree state flags remain unchanged.
        subroutine zeroAddState()
            type(BallTree) :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: zero_coords(2, 0)
            integer(int64) :: numMods, pop, dim
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%addNodes(zero_coords)

            numMods = t%getNumMods()
            ratio   = t%getRebuildRatio()
            pop     = t%getPop()
            dim     = t%getDim()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_ZERO_ADD_STATE ---'
                write(*, '(A,I0)') 'expected numMods=0 after zero-node add, got: ', numMods
                stop 1
            end if
            if (pop .ne. 4_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_ZERO_ADD_STATE ---'
                write(*, '(A,I0)') 'expected pop=4 unchanged, got: ', pop
                stop 1
            end if
            if (ratio .ne. 0.25_real64 .or. dim .ne. 2_int64 .or. &
                .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')      '--- Testv071_ADD_NODES_ZERO_ADD_STATE ---'
                write(*, '(A,F8.4)') 'ratio:       ', ratio
                write(*, '(A,I0)')   'dim:         ', dim
                write(*, '(A,L2)')   'initialized: ', isInit
                write(*, '(A,L2)')   'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)')   'root:        ', rootAssoc
                stop 1
            end if
        end subroutine zeroAddState
end program Testv071_ADD_NODES_ZERO_ADD_STATE
