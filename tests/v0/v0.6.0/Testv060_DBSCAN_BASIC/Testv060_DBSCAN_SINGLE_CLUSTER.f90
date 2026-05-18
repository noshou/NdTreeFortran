program Testv060_DBSCAN_SINGLE_CLUSTER
    use KdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanSingleCluster()
    contains
        !> 4 close points all within radius of each other -> 1 cluster, 0 noise.
        !! size(res)==2: res(1)=cluster, res(2)=empty noise bucket.
        subroutine dbscanSingleCluster()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.1_real64, 0.1_real64], [2, 4])
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected cluster size==4, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine dbscanSingleCluster
end program Testv060_DBSCAN_SINGLE_CLUSTER
