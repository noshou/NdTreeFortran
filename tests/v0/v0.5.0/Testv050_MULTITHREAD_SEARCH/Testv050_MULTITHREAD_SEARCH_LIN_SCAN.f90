program Testv050_MULTITHREAD_SEARCH_LIN_SCAN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    use omp_lib
    implicit none
    call multithreadLinScan()
    contains
        !> 4 threads concurrently call linScan on a read-only tree.
        !! All must return the correct count and the correct ids.
        subroutine multithreadLinScan()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: pool(:)
            type(NodeId)                 :: targets(3)
            integer                      :: errors

            call t%build(coords)
            pool       = t%getAllNodes()
            targets(1) = pool(1)%p%getNodeId()
            targets(2) = pool(3)%p%getNodeId()
            targets(3) = pool(5)%p%getNodeId()
            errors     = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, targets, errors) NUM_THREADS(4) &
            !$OMP   PRIVATE(pool)
            pool = t%linScan(targets)
            if (size(pool) .ne. 3) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_MULTITHREAD_SEARCH_LIN_SCAN: errors = ', errors
                stop 1
            end if
        end subroutine multithreadLinScan
end program Testv050_MULTITHREAD_SEARCH_LIN_SCAN
