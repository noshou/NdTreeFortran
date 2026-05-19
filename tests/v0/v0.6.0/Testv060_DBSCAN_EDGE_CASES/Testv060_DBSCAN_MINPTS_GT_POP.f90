program Testv060_DBSCAN_MINPTS_GT_POP
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanMinPtsGtPop()
    contains
        !> minPts=100 on a 5-node tree -> no point can be a core point -> all noise.
        subroutine dbscanMinPtsGtPop()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.2_real64, 0.0_real64, &
                 0.3_real64, 0.0_real64, 0.4_real64, 0.0_real64], [2, 5])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: nClusters, noiseSize

            call t%build(coords)
            res = t%DBSCAN(minPts=100, radius=100.0_real64)

            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)

            if (nClusters .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_GT_POP ---'
                write(*, '(A,I0)') 'expected 0 clusters, got: ', nClusters
                stop 1
            end if
            if (noiseSize .ne. 5) then
                write(*, '(A)')    '--- Testv060_DBSCAN_MINPTS_GT_POP ---'
                write(*, '(A,I0)') 'expected all 5 as noise, got: ', noiseSize
                stop 1
            end if
        end subroutine dbscanMinPtsGtPop
end program Testv060_DBSCAN_MINPTS_GT_POP
