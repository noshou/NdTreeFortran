program Testv060_DBSCAN_BAD_BUFFER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanBadBuffer()
    contains
        subroutine dbscanBadBuffer()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=1.0_real64, bufferSize=0)
            write(*, '(A)') '--- Testv060_DBSCAN_BAD_BUFFER ---'
            write(*, '(A)') 'expected error stop for bufferSize=0, but returned normally'
        end subroutine dbscanBadBuffer
end program Testv060_DBSCAN_BAD_BUFFER
