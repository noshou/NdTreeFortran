program Testv060_DBSCAN_BAD_METRIC
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanBadMetric()
    contains
        subroutine dbscanBadMetric()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=1.0_real64, metric='unknown')
            write(*, '(A)') '--- Testv060_DBSCAN_BAD_METRIC ---'
            write(*, '(A)') 'expected error stop for metric=''unknown'', but returned normally'
        end subroutine dbscanBadMetric
end program Testv060_DBSCAN_BAD_METRIC
