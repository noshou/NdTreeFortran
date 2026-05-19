program Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call concurrentRnnCentroidChebyshev()
    contains
        !> 4 threads concurrently call rNN_Centroid at (1,1) r=1 (chebyshev).
        !! All 9 pts of the 3x3 grid are at chebyshev distance <= 1 from (1,1) = 9.
        subroutine concurrentRnnCentroidChebyshev()
            type(KdTree)   :: t
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, &
                1.0_real64, 0.0_real64, &
                2.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, &
                1.0_real64, 1.0_real64, &
                2.0_real64, 1.0_real64, &
                0.0_real64, 2.0_real64, &
                1.0_real64, 2.0_real64, &
                2.0_real64, 2.0_real64], [2, 9])
            integer      :: i
            logical      :: failed = .false.

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, failed)
            do i = 1, 4
                block
                    type(NdNodePtr), allocatable :: res(:)
                    res = t%rNN_Centroid([1.0_real64, 1.0_real64], 1.0_real64, metric='chebyshev')
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_CHEBYSHEV ---'
                write(*, '(A)') 'one or more threads got wrong result from concurrent rNN_Centroid'
                stop 1
            end if
        end subroutine concurrentRnnCentroidChebyshev
end program Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_CHEBYSHEV
