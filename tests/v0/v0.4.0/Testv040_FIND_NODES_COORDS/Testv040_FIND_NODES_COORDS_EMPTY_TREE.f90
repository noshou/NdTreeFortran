program Testv040_FIND_NODES_COORDS_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesCoordsEmptyTree()
    contains
        !> rNN_Coords on a tree built with zero nodes must return empty buckets.
        !! Tree state (pop=0, initState=true) must be unchanged after the call.
        subroutine findNodesCoordsEmptyTree()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            logical :: isInit
            integer(int64) :: pop
            real(real64) :: empty_coords(2, 0)
            real(real64) :: query(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 1.0_real64], [2, 2])

            call t%build(empty_coords)
            res = t%rNN_Coords(query, epsilon=1.0_real64)

            ! verify result shape
            if (size(res) .ne. 2) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected res size 2, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 0 .or. size(res(2)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_EMPTY_TREE ---'
                write(*, '(A,I0,A,I0)') 'expected empty buckets, got sizes: ', &
                    size(res(1)%nodes), ' and ', size(res(2)%nodes)
                stop 1
            end if
            ! verify tree state unchanged
            call t%getInitState(isInit)
            pop = t%getPop()
            if (.not. isInit .or. pop .ne. 0_int64) then
                write(*, '(A)')       '--- Testv040_FIND_NODES_COORDS_EMPTY_TREE ---'
                write(*, '(A,L2,I0)') 'expected initState=T pop=0, got: ', isInit, pop
                stop 1
            end if
        end subroutine findNodesCoordsEmptyTree
end program Testv040_FIND_NODES_COORDS_EMPTY_TREE
