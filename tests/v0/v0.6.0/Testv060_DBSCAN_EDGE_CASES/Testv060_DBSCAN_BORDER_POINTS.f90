program Testv060_DBSCAN_BORDER_POINTS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanBorderPoints()
    contains
        !> Border point test: a point visited as noise before its core point is
        !! processed should be reassigned to the cluster.
        !!
        !! Pool layout (column order):
        !!   idx1=(0.4,0)  idx2=(0,0)  idx3=(0.1,0)  idx4=(0.2,0)  idx5=(15,0)
        !! radius=0.25, minPts=2
        !!
        !! (0.4,0) is border of cluster via (0.2,0), visited first as noise.
        !! (15,0) is isolated noise.
        !!
        !! Per spec: 1 cluster {(0,0),(0.1,0),(0.2,0),(0.4,0)}, 1 noise {(15,0)}.
        !! size(res)==2, res(1) has 4 nodes, res(2) has 1 node.
        subroutine dbscanBorderPoints()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.4_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.2_real64, 0.0_real64, 15.0_real64, 0.0_real64], [2, 5])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, clusterSize, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.25_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_BORDER_POINTS ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            clusterSize = size(res(1)%nodes)
            if (clusterSize .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_BORDER_POINTS ---'
                write(*, '(A,I0,A)') 'expected cluster size==4 (border point must be assigned', &
                    clusterSize, ' -- border point may have been incorrectly left as noise)'
                stop 1
            end if
            if (noiseSize .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_BORDER_POINTS ---'
                write(*, '(A,I0)') 'expected noise size==1, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanBorderPoints
end program Testv060_DBSCAN_BORDER_POINTS
