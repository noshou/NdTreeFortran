program Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call concurrentRnnCentroidManhattan()
    contains
        !> 4 threads concurrently call rNN_Centroid at (1,1) r=1.5 (manhattan).
        !! pts within manhattan 1.5 of (1,1): (1,0)[1],(0,1)[1],(1,1)[0],(2,1)[1],(1,2)[1] = 5.
        subroutine concurrentRnnCentroidManhattan()
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
                    res = t%rNN_Centroid([1.0_real64, 1.0_real64], 1.5_real64, metric='manhattan')
                    if (size(res) .ne. 5) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_MANHATTAN ---'
                write(*, '(A)') 'one or more threads got wrong result from concurrent rNN_Centroid'
                stop 1
            end if
        end subroutine concurrentRnnCentroidManhattan
end program Testv030_MULTITHREAD_CONCURRENT_RNN_CENTROID_MANHATTAN
