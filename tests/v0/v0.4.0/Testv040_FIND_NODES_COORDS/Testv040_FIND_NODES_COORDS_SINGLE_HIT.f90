program Testv040_FIND_NODES_COORDS_SINGLE_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsOneHit()
    contains
        !> Single query at (1,1) on a 3x3 grid with epsilon=0.5 must return exactly one node.
        subroutine findNodesCoordsOneHit()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64), allocatable :: found(:)
            ! 3x3 grid: (0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(0,2),(1,2),(2,2)
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64) :: query(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=0.5_real64)

            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_SINGLE_HIT ---'
                write(*, '(A,I0)') 'expected res size 1, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_SINGLE_HIT ---'
                write(*, '(A,I0)') 'expected 1 node in bucket, got: ', size(res(1)%nodes)
                stop 1
            end if
            found = res(1)%nodes(1)%p%getCoords()
            if (abs(found(1) - 1.0_real64) > 1e-10_real64 .or. &
                abs(found(2) - 1.0_real64) > 1e-10_real64) then
                write(*, '(A)')         '--- Testv040_FIND_NODES_COORDS_SINGLE_HIT ---'
                write(*, '(A,2F8.4)')   'expected (1.0, 1.0), got: ', found(1), found(2)
                stop 1
            end if
        end subroutine findNodesCoordsOneHit
end program Testv040_FIND_NODES_COORDS_SINGLE_HIT
