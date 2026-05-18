program Testv060_DBSCAN_CUSTOM_BUFFER
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanCustomBuffer()
    contains
        !> 20x20=400 grid with spacing 0.1. DBSCAN with bufferSize=2 (far below
        !! default) must produce the same result as the default bufferSize:
        !! 1 cluster of 400, 0 noise. Verifies that small bufferSize only affects
        !! performance, not correctness.
        subroutine dbscanCustomBuffer()
            type(KdTree)                    :: t
            integer,            parameter   :: side = 20, n = side * side
            real(real64),       allocatable :: coords(:,:)
            type(KdNodeBucket), allocatable :: resDefault(:), resSmall(:)
            integer(int64)                  :: totalDef, totalSmall, pop
            integer                         :: i, j, idx

            allocate(coords(2, n))
            idx = 0
            do j = 1, side
                do i = 1, side
                    idx = idx + 1
                    coords(1, idx) = real(i - 1, real64) * 0.1_real64
                    coords(2, idx) = real(j - 1, real64) * 0.1_real64
                end do
            end do

            call t%build(coords)
            resDefault = t%DBSCAN(minPts=2, radius=0.15_real64)
            resSmall   = t%DBSCAN(minPts=2, radius=0.15_real64, bufferSize=2)

            pop = t%getPop()

            totalDef = 0_int64
            do i = 1, size(resDefault)
                totalDef = totalDef + int(size(resDefault(i)%nodes), int64)
            end do
            totalSmall = 0_int64
            do i = 1, size(resSmall)
                totalSmall = totalSmall + int(size(resSmall(i)%nodes), int64)
            end do

            if (size(resDefault) - 1 .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CUSTOM_BUFFER ---'
                write(*, '(A,I0)') 'default: expected 1 cluster, got: ', size(resDefault) - 1
                stop 1
            end if
            if (size(resSmall) - 1 .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CUSTOM_BUFFER ---'
                write(*, '(A,I0)') 'bufferSize=2: expected 1 cluster, got: ', size(resSmall) - 1
                stop 1
            end if
            if (size(resDefault(1)%nodes) .ne. n) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CUSTOM_BUFFER ---'
                write(*, '(A,I0,A,I0)') 'default cluster size: expected ', n, ', got: ', &
                    size(resDefault(1)%nodes)
                stop 1
            end if
            if (size(resSmall(1)%nodes) .ne. n) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CUSTOM_BUFFER ---'
                write(*, '(A,I0,A,I0)') 'bufferSize=2 cluster size: expected ', n, ', got: ', &
                    size(resSmall(1)%nodes)
                stop 1
            end if
            if (totalDef .ne. pop .or. totalSmall .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_CUSTOM_BUFFER ---'
                write(*, '(A,I0,A,I0,A,I0)') 'population invariant failed: pop=', pop, &
                    ', totalDef=', totalDef, ', totalSmall=', totalSmall
                stop 1
            end if
        end subroutine dbscanCustomBuffer
end program Testv060_DBSCAN_CUSTOM_BUFFER
