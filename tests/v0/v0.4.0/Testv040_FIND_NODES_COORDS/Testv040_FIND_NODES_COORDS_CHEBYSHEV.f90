program Testv040_FIND_NODES_COORDS_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsChebyhev()
    contains
        !> 4-point unit square; query at (0,0) epsilon=1.0 chebyshev.
        !! Chebyshev: all 4 nodes have L-inf dist <= 1.0 (diagonal is max(1,1)=1).
        !! Same query with euclidean would return only 3 (diagonal sqrt(2) > 1).
        subroutine findNodesCoordsChebyhev()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            ! (0,0),(1,0),(0,1),(1,1)
            real(real64) :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=1.0_real64, metric='chebyshev')

            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 4 nodes (L-inf <= 1.0 from origin), got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesCoordsChebyhev
end program Testv040_FIND_NODES_COORDS_CHEBYSHEV
