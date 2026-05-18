program Testv060_DBSCAN_1M_SINGLE_CLUSTER
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscan1MSingleCluster()
    contains
        !> 1000x1000=1000000 points in tiny grid with spacing=0.001, radius=0.002, minPts=2.
        !! Every point has neighbours -> 1 cluster, 0 noise.
        subroutine dbscan1MSingleCluster()
            type(KdTree)                    :: t
            integer,            parameter   :: n = 1000000
            integer,            parameter   :: side = 1000
            real(real64),       allocatable :: coords(:,:)
            type(KdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: i, j, idx, nClusters, noiseSize

            allocate(coords(2, n))
            idx = 0
            do j = 1, side
                do i = 1, side
                    idx = idx + 1
                    coords(1, idx) = real(i - 1, real64) * 0.001_real64
                    coords(2, idx) = real(j - 1, real64) * 0.001_real64
                end do
            end do

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.002_real64)

            pop      = t%getPop()
            total    = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do
            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1M_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (size(res(1)%nodes) .ne. n) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1M_SINGLE_CLUSTER ---'
                write(*, '(A,I0,A,I0)') 'expected cluster size==', n, ', got: ', size(res(1)%nodes)
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1M_SINGLE_CLUSTER ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1M_SINGLE_CLUSTER ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscan1MSingleCluster
end program Testv060_DBSCAN_1M_SINGLE_CLUSTER
