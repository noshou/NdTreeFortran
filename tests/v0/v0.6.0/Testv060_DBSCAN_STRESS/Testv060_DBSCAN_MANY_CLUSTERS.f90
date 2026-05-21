program Testv060_DBSCAN_MANY_CLUSTERS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanManyClusters()
    contains
        !> 20 clusters of 5 points each (100 total). Cluster i is centered at
        !! (i*20.0, 0.0); its 5 points span y=0..0.4 with spacing 0.1.
        !! radius=0.15 connects all 5 within a cluster; inter-cluster gap=20 >> radius.
        !! Expect: nClusters=20, noiseSize=0, total=100.
        subroutine dbscanManyClusters()
            type(KdTree)                    :: t
            integer,            parameter   :: nClust = 20, clSz = 5, n = nClust * clSz
            real(real64),       allocatable :: coords(:,:)
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: ci, pi, idx, nClusters, noiseSize, i

            allocate(coords(2, n))
            idx = 0
            do ci = 1, nClust
                do pi = 0, clSz - 1
                    idx = idx + 1
                    coords(1, idx) = real(ci, real64) * 20.0_real64
                    coords(2, idx) = real(pi, real64) * 0.1_real64
                end do
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
                write(*, '(A)')    '--- Testv060_DBSCAN_MANY_CLUSTERS ---'
                write(*, '(A,I0,A,I0)') 'expected ', nClust, ' clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MANY_CLUSTERS ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MANY_CLUSTERS ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscanManyClusters
end program Testv060_DBSCAN_MANY_CLUSTERS
