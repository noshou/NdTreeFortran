program Testv060_DBSCAN_MINPTS_ONE_ISOLATED
    use KdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanMinPtsOneIsolated()
    contains
        !> 4 completely isolated points (separation >> radius), minPts=1.
        !! rNN is self-inclusive, so each point's neighbourhood = {itself}, size=1 >= 1.
        !! Every point is a core point of its own singleton cluster -> 4 clusters, 0 noise.
        subroutine dbscanMinPtsOneIsolated()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 100.0_real64, 0.0_real64, &
                 0.0_real64, 100.0_real64, 100.0_real64, 100.0_real64], [2, 4])
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize, total, i

            call t%build(coords)
            res = t%DBSCAN(minPts=1, radius=0.01_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            total     = 0
            do i = 1, size(res)
                total = total + size(res(i)%nodes)
            end do

            if (nClusters .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ONE_ISOLATED ---'
                write(*, '(A,I0)') 'expected 4 singleton clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ONE_ISOLATED ---'
                write(*, '(A,I0)') 'expected 0 noise (each isolated point is its own core), got: ', noiseSize
                stop 1
            end if
            if (total .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ONE_ISOLATED ---'
                write(*, '(A,I0)') 'population invariant failed: total=', total
                stop 1
            end if
        end subroutine dbscanMinPtsOneIsolated
end program Testv060_DBSCAN_MINPTS_ONE_ISOLATED
