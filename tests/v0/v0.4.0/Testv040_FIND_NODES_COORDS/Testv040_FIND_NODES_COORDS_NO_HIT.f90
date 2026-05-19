program Testv040_FIND_NODES_COORDS_NO_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsNoHit()
    contains
        !> Query at a point far from all tree nodes must return an empty bucket.
        subroutine findNodesCoordsNoHit()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([5.0_real64, 5.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, epsilon=0.1_real64)

            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_NO_HIT ---'
                write(*, '(A,I0)') 'expected res size 1, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_NO_HIT ---'
                write(*, '(A,I0)') 'expected empty bucket, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesCoordsNoHit
end program Testv040_FIND_NODES_COORDS_NO_HIT
