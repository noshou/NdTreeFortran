program Testv040_FIND_NODES_COORDS_BAD_BUFFER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesBadBuffer()
    contains
        !> rNN_Coords with bufferSize <= 0 must stop 1.
        subroutine findNodesBadBuffer()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])

            call t%build(coords)
            res = t%rNN_Coords(query, bufferSize=-1)
            write(*, '(A)') '--- Testv040_FIND_NODES_COORDS_BAD_BUFFER ---'
            write(*, '(A)') 'expected stop 1, but rNN_Coords returned normally'
        end subroutine findNodesBadBuffer
end program Testv040_FIND_NODES_COORDS_BAD_BUFFER
