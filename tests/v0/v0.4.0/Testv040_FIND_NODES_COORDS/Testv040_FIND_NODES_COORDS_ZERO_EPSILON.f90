program Testv040_FIND_NODES_COORDS_ZERO_EPSILON
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsZeroEpsilon()
    contains
        !> epsilon=0.0 on a query at an exact node coord must return exactly that node.
        !! Neighbour at distance 1.0 must not appear.
        subroutine findNodesCoordsZeroEpsilon()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([1.0_real64, 0.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=0.0_real64)

            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_ZERO_EPSILON ---'
                write(*, '(A,I0)') 'expected exactly 1 node at zero epsilon, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesCoordsZeroEpsilon
end program Testv040_FIND_NODES_COORDS_ZERO_EPSILON
