program Testv040_FIND_NODES_COORDS_TREE_STATE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesCoordsTreeState()
    contains
        !> rNN_Coords must not mutate any tree state.
        !! pop, dim, numMods, and initState must be identical before and after the call.
        !! The found node's coords and splitAxis must be accessible and valid.
        subroutine findNodesCoordsTreeState()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64), allocatable :: found_coords(:)
            logical :: isInit
            integer(int64) :: dim_before, pop_before, mods_before
            integer(int64) :: dim_after, pop_after, mods_after
            integer(int64) :: split_axis
            real(real64) :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])

            call t%build(coords)
            dim_before  = t%getDim()
            pop_before  = t%getPop()
            mods_before = t%getNumMods()

            res = t%rNN_Coords(query, epsilon=0.5_real64)

            dim_after  = t%getDim()
            pop_after  = t%getPop()
            mods_after = t%getNumMods()
            call t%getInitState(isInit)

            if (dim_after .ne. dim_before .or. pop_after .ne. pop_before .or. &
                mods_after .ne. mods_before .or. .not. isInit) then
                write(*, '(A)')         '--- Testv040_FIND_NODES_COORDS_TREE_STATE ---'
                write(*, '(A,I0,A,I0)') 'dim:  before=', dim_before,  ' after=', dim_after
                write(*, '(A,I0,A,I0)') 'pop:  before=', pop_before,  ' after=', pop_after
                write(*, '(A,I0,A,I0)') 'mods: before=', mods_before, ' after=', mods_after
                write(*, '(A,L2)')       'initState: ', isInit
                stop 1
            end if
            ! verify found node fields are accessible and in range
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_TREE_STATE ---'
                write(*, '(A,I0)') 'expected 1 result node, got: ', size(res(1)%nodes)
                stop 1
            end if
            found_coords = res(1)%nodes(1)%p%getCoords()
            split_axis   = t%getSplitAxis(res(1)%nodes(1)%p)
            if (size(found_coords) .ne. 2) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_TREE_STATE ---'
                write(*, '(A,I0)') 'expected coord size 2, got: ', size(found_coords)
                stop 1
            end if
            if (split_axis .lt. 1_int64 .or. split_axis .gt. 2_int64) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_TREE_STATE ---'
                write(*, '(A,I0)') 'split axis out of range [1,2]: ', split_axis
                stop 1
            end if
        end subroutine findNodesCoordsTreeState
end program Testv040_FIND_NODES_COORDS_TREE_STATE
