program Testv071_DBSCAN_ALL_NOISE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanAllNoise()
    contains
        !> 4 points far apart, minPts=2, radius=0.5 -> all noise, 0 clusters.
        subroutine dbscanAllNoise()
            type(BallTree)                  :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                 0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 0) then
                write(*, '(A)')    '--- Testv071_DBSCAN_ALL_NOISE ---'
                write(*, '(A,I0)') 'expected 0 clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 4) then
                write(*, '(A)')    '--- Testv071_DBSCAN_ALL_NOISE ---'
                write(*, '(A,I0)') 'expected noise size==4, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanAllNoise
end program Testv071_DBSCAN_ALL_NOISE
