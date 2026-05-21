program Testv060_DBSCAN_LARGE_MIXED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanLargeMixed()
    contains
        !> 50 clusters of 5 points + 50 isolated noise points = 300 total.
        !! Cluster i (1..50): 5 points near (i*100.0, 0.0), y spacing 0.1.
        !! Noise point i (1..50): at (i*100.0 + 0.5, 1000.0) - isolated from everything.
        !! radius=0.15, minPts=2.
        !! Expected: nClusters=50, noiseSize=50, total=300.
        subroutine dbscanLargeMixed()
            type(KdTree)                    :: t
            integer,            parameter   :: nClust = 50, clSz = 5, nNoise = 50
            integer,            parameter   :: n = nClust * clSz + nNoise
            real(real64),       allocatable :: coords(:,:)
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: ci, pi, ni, idx, nClusters, noiseSize, i

            allocate(coords(2, n))
            idx = 0
            do ci = 1, nClust
                do pi = 0, clSz - 1
                    idx = idx + 1
                    coords(1, idx) = real(ci, real64) * 100.0_real64
                    coords(2, idx) = real(pi, real64) * 0.1_real64
                end do
            end do
            do ni = 1, nNoise
                idx = idx + 1
                coords(1, idx) = real(ni, real64) * 100.0_real64 + 0.5_real64
                coords(2, idx) = 1000.0_real64
            end do

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.15_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            pop       = t%getPop()
            total     = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (nClusters .ne. nClust) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_MIXED ---'
                write(*, '(A,I0,A,I0)') 'expected ', nClust, ' clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. nNoise) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_MIXED ---'
                write(*, '(A,I0,A,I0)') 'expected ', nNoise, ' noise, got: ', noiseSize
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_MIXED ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscanLargeMixed
end program Testv060_DBSCAN_LARGE_MIXED
