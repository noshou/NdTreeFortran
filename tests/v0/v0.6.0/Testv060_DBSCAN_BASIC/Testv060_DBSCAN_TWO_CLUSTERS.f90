program Testv060_DBSCAN_TWO_CLUSTERS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanTwoClusters()
    contains
        !> 3 points near origin, 3 points near (10,10) -> 2 clusters, 0 noise.
        subroutine dbscanTwoClusters()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 6) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 6])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_TWO_CLUSTERS ---'
                write(*, '(A,I0)') 'expected 2 clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_TWO_CLUSTERS ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_TWO_CLUSTERS ---'
                write(*, '(A,I0)') 'expected cluster 1 size==3, got: ', size(res(1)%nodes)
                stop 1
            end if
            if (size(res(2)%nodes) .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_TWO_CLUSTERS ---'
                write(*, '(A,I0)') 'expected cluster 2 size==3, got: ', size(res(2)%nodes)
                stop 1
            end if
        end subroutine dbscanTwoClusters
end program Testv060_DBSCAN_TWO_CLUSTERS
