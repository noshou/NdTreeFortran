program Testv050_MULTITHREAD_CONCURRENT_RMV_RNN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call concurrentRmvRnn()
    contains
        !> 4 threads each remove 1 corner from a 9-node 3x3 grid concurrently.
        !! After the parallel region, rNN_Centroid from center (1,1) with r=1.1
        !! finds 5 survivors: the center node (dist=0) + 4 axis-adjacent (dist=1).
        subroutine concurrentRmvRnn()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)                 :: queries(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)               :: pop
            integer                      :: i

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
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_RNN ---'
                write(*, '(A,I0)') 'expected pop=5 after removing 4 corners, got: ', pop
                stop 1
            end if

            res = t%rNN_Centroid([1.0_real64, 1.0_real64], 1.1_real64)
            if (size(res) .ne. 5) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_RNN ---'
                write(*, '(A,I0)') 'expected 5 survivors after corner removal, got: ', size(res)
                stop 1
            end if
        end subroutine concurrentRmvRnn
end program Testv050_MULTITHREAD_CONCURRENT_RMV_RNN
