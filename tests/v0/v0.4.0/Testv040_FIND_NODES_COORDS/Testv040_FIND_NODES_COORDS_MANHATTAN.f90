program Testv040_FIND_NODES_COORDS_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsmanhattan()
    contains
        !> 4-point unit square; query at (0,0) epsilon=1.4 manhattan.
        !! Manhattan: (1,1) has L1 dist 2 > 1.4, so 3 of 4 nodes match.
        subroutine findNodesCoordsmanhattan()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            ! (0,0),(1,0),(0,1),(1,1)
            real(real64) :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=1.4_real64, metric='manhattan')

            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 3 nodes (L1 <= 1.4 from origin), got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesCoordsmanhattan
end program Testv040_FIND_NODES_COORDS_MANHATTAN
