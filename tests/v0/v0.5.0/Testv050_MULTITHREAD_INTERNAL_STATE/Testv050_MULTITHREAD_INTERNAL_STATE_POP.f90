program Testv050_MULTITHREAD_INTERNAL_STATE_POP
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call multithreadInternalStatePop()
    contains
        !> 4 threads each call rmvNodes to remove 1 unique node from a 9-node tree.
        !! Each thread gets its own query at a distinct corner.
        !! After the parallel region pop=5 (9 - 4 removals).
        subroutine multithreadInternalStatePop()
            type(KdTree) :: t
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64) :: queries(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            integer(int64) :: pop
            integer        :: i

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, queries)
            do i = 1, 4
                block
                    real(real64) :: q(2, 1)
                    integer      :: numRmv
                    q(:, 1) = queries(:, i)
                    numRmv  = t%rmvNodes(coordsList=q)
                end block
            end do
            !$OMP END PARALLEL DO

            pop = t%getPop()
            if (pop .ne. 5_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_INTERNAL_STATE_POP ---'
                write(*, '(A,I0)') 'expected pop=5 after 4 concurrent removals, got: ', pop
                stop 1
            end if
        end subroutine multithreadInternalStatePop
end program Testv050_MULTITHREAD_INTERNAL_STATE_POP
