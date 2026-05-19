program Testv050_MULTITHREAD_SEARCH_RNN_RAD
    use NdTreeFortran
    use iso_fortran_env, only: real64
    use omp_lib
    implicit none
    call multithreadRnnRad()
    contains
        !> 4 threads concurrently call rNN_Rad on a read-only tree.
        !! All must return the correct node count per query.
        subroutine multithreadRnnRad()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            ! q at (0,0), r=1.1 → captures (0,0), (1,0), (0,1) = 3 nodes
            real(real64)                    :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.1_real64]
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: errors

            call t%build(coords)
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, q, r, errors) NUM_THREADS(4) &
            !$OMP   PRIVATE(res)
            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 3) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_MULTITHREAD_SEARCH_RNN_RAD: errors = ', errors
                stop 1
            end if
        end subroutine multithreadRnnRad
end program Testv050_MULTITHREAD_SEARCH_RNN_RAD
