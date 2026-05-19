program Testv060_DBSCAN_IDEMPOTENT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanIdempotent()
    contains
        !> Two consecutive DBSCAN calls on the same tree must give identical results.
        !! Catches the implicit-SAVE bug on clusterIdx: if clusterIdx accumulated
        !! across calls, the second call would return wrong cluster indices / counts.
        subroutine dbscanIdempotent()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 11) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 0.0_real64, 10.1_real64, 0.0_real64, 10.0_real64, 0.1_real64, &
                 0.0_real64, 10.0_real64, 0.1_real64, 10.0_real64, 0.0_real64, 10.1_real64, &
                 50.0_real64, 50.0_real64, 100.0_real64, 100.0_real64], [2, 11])
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total1, total2
            integer                         :: nc1, ns1, nc2, ns2, i

            call t%build(coords)

            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            nc1    = size(res) - 1
            ns1    = size(res(size(res))%nodes)
            total1 = 0_int64
            do i = 1, size(res)
                total1 = total1 + int(size(res(i)%nodes), int64)
            end do

            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            nc2    = size(res) - 1
            ns2    = size(res(size(res))%nodes)
            total2 = 0_int64
            do i = 1, size(res)
                total2 = total2 + int(size(res(i)%nodes), int64)
            end do

            if (nc1 .ne. 3) then
                write(*, '(A)')    '--- Testv060_DBSCAN_IDEMPOTENT ---'
                write(*, '(A,I0)') 'first call: expected 3 clusters, got: ', nc1
                stop 1
            end if
            if (ns1 .ne. 2) then
                write(*, '(A)')    '--- Testv060_DBSCAN_IDEMPOTENT ---'
                write(*, '(A,I0)') 'first call: expected 2 noise, got: ', ns1
                stop 1
            end if
            if (nc2 .ne. nc1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_IDEMPOTENT ---'
                write(*, '(A,I0,A,I0)') 'cluster count changed: first=', nc1, ', second=', nc2
                stop 1
            end if
            if (ns2 .ne. ns1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_IDEMPOTENT ---'
                write(*, '(A,I0,A,I0)') 'noise count changed: first=', ns1, ', second=', ns2
                stop 1
            end if
            if (total2 .ne. total1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_IDEMPOTENT ---'
                write(*, '(A,I0,A,I0)') 'total changed: first=', total1, ', second=', total2
                stop 1
            end if
        end subroutine dbscanIdempotent
end program Testv060_DBSCAN_IDEMPOTENT
