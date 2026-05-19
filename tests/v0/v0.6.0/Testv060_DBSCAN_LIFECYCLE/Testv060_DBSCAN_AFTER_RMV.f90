program Testv060_DBSCAN_AFTER_RMV
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanAfterRmv()
    contains
        !> DBSCAN before and after rmvNodes: second run has smaller population.
        subroutine dbscanAfterRmv()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 6) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 6])
            real(real64)                    :: toRemove(2, 3) = reshape( &
                [10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: numRmv, i

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            if (size(res) - 1 .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected 2 clusters before rmv, got: ', size(res) - 1
                stop 1
            end if

            numRmv = t%rmvNodes(coordsList=toRemove)
            if (numRmv .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected numRmv==3, got: ', numRmv
                stop 1
            end if

            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            pop   = t%getPop()
            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected pop==3 after rmv, got: ', pop
                stop 1
            end if
            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_RMV ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
            if (size(res) - 1 .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected 1 cluster after rmv, got: ', size(res) - 1
                stop 1
            end if
        end subroutine dbscanAfterRmv
end program Testv060_DBSCAN_AFTER_RMV
