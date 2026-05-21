program Testv060_DBSCAN_MIXED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanMixed()
    contains
        !> 3 points clustered near origin + 2 isolated -> 1 cluster, 2 noise.
        subroutine dbscanMixed()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 50.0_real64, 0.0_real64, 0.0_real64, 50.0_real64], [2, 5])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MIXED ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MIXED ---'
                write(*, '(A,I0)') 'expected 2 noise, got: ', noiseSize
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MIXED ---'
                write(*, '(A,I0)') 'expected cluster size==3, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine dbscanMixed
end program Testv060_DBSCAN_MIXED
