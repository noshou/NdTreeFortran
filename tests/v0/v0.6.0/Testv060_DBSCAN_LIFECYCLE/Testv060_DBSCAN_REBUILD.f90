program Testv060_DBSCAN_REBUILD
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanRebuild()
    contains
        !> Full rebuild cycle: build(2 clusters) -> DBSCAN -> rmvAll -> DBSCAN -> addNodes -> DBSCAN.
        !! Verifies that DBSCAN state is reset correctly when the pool changes.
        subroutine dbscanRebuild()
            type(KdTree)                    :: t
            real(real64)                    :: coords1(2, 6) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 6])
            real(real64)                    :: coords2(2, 4) = reshape( &
                [5.0_real64, 5.0_real64, 5.1_real64, 5.0_real64, &
                 5.0_real64, 5.1_real64, 5.1_real64, 5.1_real64], [2, 4])
            type(KdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: numRmv, i

            ! Phase 1: initial build -> 2 clusters
            call t%build(coords1)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            if (size(res) - 1 .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (phase 1) ---'
                write(*, '(A,I0)') 'expected 2 clusters, got: ', size(res) - 1
                stop 1
            end if

            ! Phase 2: remove all nodes -> empty tree -> 0 clusters
            numRmv = t%rmvNodes(coordsList=coords1)
            if (numRmv .ne. 6) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (rmv) ---'
                write(*, '(A,I0)') 'expected numRmv==6, got: ', numRmv
                stop 1
            end if
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (phase 2 empty) ---'
                write(*, '(A,I0)') 'expected size(res)==0 for empty tree, got: ', size(res)
                stop 1
            end if

            ! Phase 3: add new nodes -> 1 cluster of 4
            call t%addNodes(coords2)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            pop   = t%getPop()
            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (size(res) - 1 .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (phase 3) ---'
                write(*, '(A,I0)') 'expected 1 cluster after rebuild, got: ', size(res) - 1
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (phase 3) ---'
                write(*, '(A,I0)') 'expected cluster size==4, got: ', size(res(1)%nodes)
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_REBUILD (phase 3) ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscanRebuild
end program Testv060_DBSCAN_REBUILD
