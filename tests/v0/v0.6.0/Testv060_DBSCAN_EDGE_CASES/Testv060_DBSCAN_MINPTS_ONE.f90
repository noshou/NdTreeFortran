program Testv060_DBSCAN_MINPTS_ONE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanMinPtsOne()
    contains
        !> minPts=1: a point is core if it has at least 1 neighbour (itself counts).
        !! 3 close points + 1 isolated far away.
        !! Close points form 1 cluster; isolated point with no neighbour within radius -> noise.
        subroutine dbscanMinPtsOne()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 100.0_real64, 100.0_real64], [2, 4])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=1_int64, radius=0.5_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .lt. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ONE ---'
                write(*, '(A,I0)') 'expected at least 1 cluster, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_ONE ---'
                write(*, '(A,I0)') 'expected 0 noise (isolated point is own core), got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanMinPtsOne
end program Testv060_DBSCAN_MINPTS_ONE
