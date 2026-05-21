program Testv060_DBSCAN_ZERO_RADIUS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanZeroRadius()
    contains
        !> radius=0.0: only exact duplicates can be neighbours.
        !! 2 distinct points + 2 duplicates at origin -> 1 cluster (duplicates), 2 noise.
        subroutine dbscanZeroRadius()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 4])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize, total, i

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.0_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_ZERO_RADIUS ---'
                write(*, '(A,I0)') 'expected 1 cluster (exact duplicates), got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_ZERO_RADIUS ---'
                write(*, '(A,I0)') 'expected 2 noise (distinct points), got: ', noiseSize
                stop 1
            end if
            total = 0
            do i = 1, size(res)
                total = total + size(res(i)%nodes)
            end do
            if (total .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_ZERO_RADIUS ---'
                write(*, '(A,I0)') 'expected total==4, got: ', total
                stop 1
            end if
        end subroutine dbscanZeroRadius
end program Testv060_DBSCAN_ZERO_RADIUS
