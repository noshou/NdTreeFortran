program Testv060_DBSCAN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanManhattan()
    contains
        !> Points (0,0) and (0.6,0.6): manhattan dist=1.2, euclidean dist=0.849.
        !! With radius=1.0, minPts=2:
        !!   euclidean -> cluster (0.849 < 1.0)
        !!   manhattan -> noise   (1.2   > 1.0)
        !! Verify manhattan gives 0 clusters, size(res)==1, noise size==2.
        subroutine dbscanManhattan()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 0.6_real64, 0.6_real64], [2, 2])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=1.0_real64, metric='manhattan')

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 0 clusters with manhattan metric, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 2 noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanManhattan
end program Testv060_DBSCAN_MANHATTAN
