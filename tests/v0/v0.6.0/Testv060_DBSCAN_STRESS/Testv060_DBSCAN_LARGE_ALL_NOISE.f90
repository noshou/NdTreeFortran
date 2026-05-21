program Testv060_DBSCAN_LARGE_ALL_NOISE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanLargeAllNoise()
    contains
        !> 10000 points on a line with spacing 1.0, radius=0.5, minPts=2.
        !! No point has any neighbour within radius -> all noise.
        subroutine dbscanLargeAllNoise()
            type(KdTree)                    :: t
            integer,            parameter   :: n = 10000
            real(real64),       allocatable :: coords(:,:)
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: i, nClusters, noiseSize

            allocate(coords(2, n))
            do i = 1, n
                coords(1, i) = real(i, real64)
                coords(2, i) = 0.0_real64
            end do

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            pop       = t%getPop()
            total     = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (nClusters .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_ALL_NOISE ---'
                write(*, '(A,I0)') 'expected 0 clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. n) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_ALL_NOISE ---'
                write(*, '(A,I0,A,I0)') 'expected noise size==', n, ', got: ', noiseSize
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_ALL_NOISE ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscanLargeAllNoise
end program Testv060_DBSCAN_LARGE_ALL_NOISE
