program Testv060_MULTITHREAD_DBSCAN_MANY_THREADS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    use omp_lib
    implicit none
    call multithreadDbscanManyThreads()
    contains
        !> 8 threads each call DBSCAN on the same read-only tree with a
        !! multi-cluster layout. Verifies no data races in the read path.
        subroutine multithreadDbscanManyThreads()
            type(KdTree)                    :: t
            integer,            parameter   :: nClust = 5, clSz = 4, n = nClust * clSz
            real(real64)                    :: coords(2, n)
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: pop, total
            integer                         :: errors, ci, pi, idx, i

            idx = 0
            do ci = 1, nClust
                do pi = 0, clSz - 1
                    idx = idx + 1
                    coords(1, idx) = real(ci, real64) * 20.0_real64
                    coords(2, idx) = real(pi, real64) * 0.1_real64
                end do
            end do

            call t%build(coords)
            pop    = t%getPop()
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, pop, errors) NUM_THREADS(8) &
            !$OMP   PRIVATE(res, total, i)
            res   = t%DBSCAN(minPts=2_int64, radius=0.15_real64)
            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do
            if (size(res) - 1 .ne. nClust .or. total .ne. pop) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv060_MULTITHREAD_DBSCAN_MANY_THREADS: errors = ', errors
                stop 1
            end if
        end subroutine multithreadDbscanManyThreads
end program Testv060_MULTITHREAD_DBSCAN_MANY_THREADS
