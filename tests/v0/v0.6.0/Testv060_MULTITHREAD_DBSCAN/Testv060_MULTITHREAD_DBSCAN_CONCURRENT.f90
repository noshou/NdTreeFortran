program Testv060_MULTITHREAD_DBSCAN_CONCURRENT
    use KdTreeFortran
    use iso_fortran_env, only: real64
    use omp_lib
    implicit none
    call multithreadDbscanConcurrent()
    contains
        !> 4 threads each call DBSCAN on the same read-only tree.
        !! All must get the same cluster count and noise count.
        subroutine multithreadDbscanConcurrent()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 6) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, 0.0_real64, 0.1_real64, &
                 10.0_real64, 10.0_real64, 10.1_real64, 10.0_real64, 10.0_real64, 10.1_real64], [2, 6])
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: errors, nClusters, noiseSize

            call t%build(coords)
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, errors) NUM_THREADS(4) &
            !$OMP   PRIVATE(res, nClusters, noiseSize)
            res       = t%DBSCAN(minPts=2, radius=0.5_real64)
            nClusters = size(res) - 1
            noiseSize = size(res(size(res))%nodes)
            if (nClusters .ne. 2 .or. noiseSize .ne. 0) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv060_MULTITHREAD_DBSCAN_CONCURRENT: errors = ', errors
                stop 1
            end if
        end subroutine multithreadDbscanConcurrent
end program Testv060_MULTITHREAD_DBSCAN_CONCURRENT
