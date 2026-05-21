program Testv060_DBSCAN_1D
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscan1D()
    contains
        !> 1D tree: 4 close points + 2 isolated.
        !! coords(1,6) = [0.0, 0.1, 0.2, 0.3, 10.0, 20.0]
        !! radius=0.15, minPts=2.
        !! (0.0..0.3) all within radius of their neighbours -> 1 cluster of 4.
        !! (10.0, 20.0) each has only itself within radius -> noise.
        subroutine dbscan1D()
            type(KdTree)                    :: t
            real(real64)                    :: coords(1, 6) = reshape( &
                [0.0_real64, 0.1_real64, 0.2_real64, 0.3_real64, 10.0_real64, 20.0_real64], [1, 6])
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: i, nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=0.15_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            pop       = t%getPop()
            total     = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (nClusters .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1D ---'
                write(*, '(A,I0)') 'expected 1 cluster, got: ', nClusters
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1D ---'
                write(*, '(A,I0)') 'expected cluster size==4, got: ', size(res(1)%nodes)
                stop 1
            end if
            if (noiseSize .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1D ---'
                write(*, '(A,I0)') 'expected 2 noise, got: ', noiseSize
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_1D ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscan1D
end program Testv060_DBSCAN_1D
