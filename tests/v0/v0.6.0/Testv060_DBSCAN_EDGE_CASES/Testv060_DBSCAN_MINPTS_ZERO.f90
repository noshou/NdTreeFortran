program Testv060_DBSCAN_MINPTS_ZERO
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanMinPtsZero()
    contains
        !> minPts=0: every point is a core point (neighbourhood size >= 0 always).
        !! All points end up in one cluster (transitively connected via overlapping radii).
        subroutine dbscanMinPtsZero()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.2_real64, 0.0_real64, 0.3_real64, 0.0_real64], [2, 4])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=0_int64, radius=0.15_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .lt. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ZERO ---'
                write(*, '(A,I0)') 'expected at least 1 cluster with minPts=0, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ZERO ---'
                write(*, '(A,I0)') 'expected 0 noise with minPts=0, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanMinPtsZero
end program Testv060_DBSCAN_MINPTS_ZERO
