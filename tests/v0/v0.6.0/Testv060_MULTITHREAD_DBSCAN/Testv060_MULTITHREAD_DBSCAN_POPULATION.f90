program Testv060_MULTITHREAD_DBSCAN_POPULATION
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    use omp_lib
    implicit none
    call multithreadDbscanPopulation()
    contains
        !> 4 threads each independently verify the population invariant.
        subroutine multithreadDbscanPopulation()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 8) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.1_real64, 0.1_real64, &
                 10.0_real64, 0.0_real64, 10.1_real64, 0.0_real64, &
                 5.0_real64, 5.0_real64, 20.0_real64, 20.0_real64], [2, 8])
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: pop, total
            integer                         :: errors, i

            call t%build(coords)
            pop    = t%getPop()
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, pop, errors) NUM_THREADS(4) &
            !$OMP   PRIVATE(res, total, i)
            res   = t%DBSCAN(minPts=2_int64, radius=0.5_real64)
            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do
            if (total .ne. pop) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv060_MULTITHREAD_DBSCAN_POPULATION: errors = ', errors
                stop 1
            end if
        end subroutine multithreadDbscanPopulation
end program Testv060_MULTITHREAD_DBSCAN_POPULATION
