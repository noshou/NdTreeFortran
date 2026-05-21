program Testv071_MULTITHREAD_CONCURRENT_RNN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call concurrentRnnCentroidEuclidean()
    contains
        !> Build shared 2D tree with 9 pts on a 3x3 grid [0,1,2]x[0,1,2].
        !! 4 threads concurrently call rNN_Centroid at (1,1) r=1.5 (euclidean).
        !! Expected: 5 nodes within r=1.0 (the cross-shaped neighborhood of center).
        subroutine concurrentRnnCentroidEuclidean()
            type(BallTree) :: t
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
                    res = t%rNN_Centroid([1.0_real64, 1.0_real64], 1.0_real64, metric='euclidean')
                    if (size(res) .ne. 5) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv071_MULTITHREAD_CONCURRENT_RNN ---'
                write(*, '(A)') 'one or more threads got wrong result from concurrent rNN_Centroid'
                stop 1
            end if
        end subroutine concurrentRnnCentroidEuclidean
end program Testv071_MULTITHREAD_CONCURRENT_RNN
