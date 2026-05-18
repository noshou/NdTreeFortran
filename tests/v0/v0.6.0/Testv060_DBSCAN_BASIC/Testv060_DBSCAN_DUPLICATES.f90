program Testv060_DBSCAN_DUPLICATES
    use KdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanDuplicates()
    contains
        !> 5 points all at same location -> 1 cluster, 0 noise.
        subroutine dbscanDuplicates()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 5])
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.0_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_DUPLICATES ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_DUPLICATES ---'
                write(*, '(A,I0)') 'expected 0 noise, got: ', noiseSize
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 5) then
                write(*, '(A)')    '--- Testv060_DBSCAN_DUPLICATES ---'
                write(*, '(A,I0)') 'expected cluster size==5, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine dbscanDuplicates
end program Testv060_DBSCAN_DUPLICATES
