program Testv060_DBSCAN_LARGE_GRID_SINGLE_CLUSTER
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanLargeGridSingleCluster()
    contains
        !> 100x100=10000 grid points, spacing=0.1, radius=0.15, minPts=2.
        !! Every point has neighbours -> 1 cluster, 0 noise.
        subroutine dbscanLargeGridSingleCluster()
            type(KdTree)                    :: t
            integer,            parameter   :: n = 10000
            integer,            parameter   :: side = 100
            real(real64),       allocatable :: coords(:,:)
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: i, j, idx, nClusters, noiseSize

            allocate(coords(2, n))
            idx = 0
            do j = 1, side
                do i = 1, side
                    idx = idx + 1
                    coords(1, idx) = real(i - 1, real64) * 0.1_real64
                    coords(2, idx) = real(j - 1, real64) * 0.1_real64
                end do
            end do

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.15_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_GRID_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (size(res(1)%nodes) .ne. n) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_GRID_SINGLE_CLUSTER ---'
                write(*, '(A,I0,A,I0)') 'expected cluster size==', n, ', got: ', size(res(1)%nodes)
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_GRID_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanLargeGridSingleCluster
end program Testv060_DBSCAN_LARGE_GRID_SINGLE_CLUSTER
