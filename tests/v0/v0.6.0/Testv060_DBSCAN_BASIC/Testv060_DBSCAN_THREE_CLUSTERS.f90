program Testv060_DBSCAN_THREE_CLUSTERS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanThreeClusters()
    contains
        !> 3 groups of 3 points each, well-separated -> 3 clusters, 0 noise.
        subroutine dbscanThreeClusters()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 0.0_real64, 10.1_real64, 0.0_real64, 10.0_real64, 0.1_real64, &
                 0.0_real64, 10.0_real64, 0.1_real64, 10.0_real64, 0.0_real64, 10.1_real64], [2, 9])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_THREE_CLUSTERS ---'
                write(*, '(A,I0)') 'expected 3 clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_THREE_CLUSTERS ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanThreeClusters
end program Testv060_DBSCAN_THREE_CLUSTERS
