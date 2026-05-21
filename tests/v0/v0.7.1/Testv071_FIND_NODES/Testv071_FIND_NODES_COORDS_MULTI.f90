program Testv071_FIND_NODES_COORDS_MULTI
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsMultiQuery()
    contains
        !> Two queries on a 3x3 grid; each must return exactly one matching node.
        subroutine findNodesCoordsMultiQuery()
            type(BallTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64), allocatable :: c1(:), c2(:)
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            ! query at (0,0) and (2,2)
            real(real64) :: query(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 2.0_real64], [2, 2])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=0.5_real64)

            if (size(res) .ne. 2) then
                write(*, '(A)')    '--- Testv071_FIND_NODES_COORDS_MULTI ---'
                write(*, '(A,I0)') 'expected res size 2, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 1 .or. size(res(2)%nodes) .ne. 1) then
                write(*, '(A)')      '--- Testv071_FIND_NODES_COORDS_MULTI ---'
                write(*, '(A,I0)')   'expected 1 node in each bucket, got: ', &
                    size(res(1)%nodes), ' and ', size(res(2)%nodes)
                stop 1
            end if
            c1 = res(1)%nodes(1)%p%getCoords()
            c2 = res(2)%nodes(1)%p%getCoords()
            if (abs(c1(1) - 0.0_real64) > 1e-10_real64 .or. abs(c1(2) - 0.0_real64) > 1e-10_real64) then
                write(*, '(A)')       '--- Testv071_FIND_NODES_COORDS_MULTI ---'
                write(*, '(A,2F8.4)') 'query 1: expected (0,0), got: ', c1(1), c1(2)
                stop 1
            end if
            if (abs(c2(1) - 2.0_real64) > 1e-10_real64 .or. abs(c2(2) - 2.0_real64) > 1e-10_real64) then
                write(*, '(A)')       '--- Testv071_FIND_NODES_COORDS_MULTI ---'
                write(*, '(A,2F8.4)') 'query 2: expected (2,2), got: ', c2(1), c2(2)
                stop 1
            end if
        end subroutine findNodesCoordsMultiQuery
end program Testv071_FIND_NODES_COORDS_MULTI
