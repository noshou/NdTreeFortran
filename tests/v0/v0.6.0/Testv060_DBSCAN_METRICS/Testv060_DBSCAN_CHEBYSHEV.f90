program Testv060_DBSCAN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanChebyshev()
    contains
        !> Points (0,0) and (0.8,0.8): chebyshev dist=0.8, euclidean dist=1.131.
        !! With radius=1.0, minPts=2:
        !!   euclidean -> noise   (1.131 > 1.0)
        !!   chebyshev -> cluster (0.8   < 1.0)
        !! Verify chebyshev gives 1 cluster, res(1) has 2 nodes, noise bucket empty.
        subroutine dbscanChebyshev()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 0.8_real64, 0.8_real64], [2, 2])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=1.0_real64, metric='chebyshev')

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 1 cluster with chebyshev metric, got: ', nClusters
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected cluster size==2, got: ', size(res(1)%nodes)
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanChebyshev
end program Testv060_DBSCAN_CHEBYSHEV
