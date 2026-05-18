program Testv060_DBSCAN_AFTER_ADD
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanAfterAdd()
    contains
        !> DBSCAN before and after addNodes: second run covers more nodes.
        subroutine dbscanAfterAdd()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64], [2, 3])
            real(real64)                    :: newCoords(2, 3) = reshape( &
                [10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 3])
            type(KdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total1, total2, pop1, pop2
            integer                         :: i

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            pop1   = t%getPop()
            total1 = 0_int64
            do i = 1, size(res)
                total1 = total1 + int(size(res(i)%nodes), int64)
            end do

            call t%addNodes(newCoords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            pop2   = t%getPop()
            total2 = 0_int64
            do i = 1, size(res)
                total2 = total2 + int(size(res(i)%nodes), int64)
            end do

            if (pop2 .ne. 6_int64) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_ADD ---'
                write(*, '(A,I0)') 'expected pop==6 after addNodes, got: ', pop2
                stop 1
            end if
            if (total2 .ne. pop2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_ADD ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total2, ', pop=', pop2
                stop 1
            end if
            if (total2 .le. total1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_ADD ---'
                write(*, '(A,I0,A,I0)') 'expected more nodes after add: before=', total1, ', after=', total2
                stop 1
            end if
            if (size(res) - 1 .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_AFTER_ADD ---'
                write(*, '(A,I0)') 'expected 2 clusters after addNodes, got: ', size(res) - 1
                stop 1
            end if
        end subroutine dbscanAfterAdd
end program Testv060_DBSCAN_AFTER_ADD
