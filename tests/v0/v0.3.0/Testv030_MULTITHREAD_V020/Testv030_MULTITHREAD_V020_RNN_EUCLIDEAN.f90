program Testv030_MULTITHREAD_V020_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call v020RnnEuclidean()
    contains
        !> 4 threads each independently cover v0.2.0 search scenarios (euclidean):
        !! single-node query, empty result, duplicates, collinear.
        subroutine v020RnnEuclidean()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    type(NdNodePtr), allocatable :: res(:)
                    real(real64) :: unit_coords(2, 4) = reshape( &
                        [1.0_real64, 0.0_real64, 0.6_real64, 0.8_real64, &
                        0.9_real64, 0.9_real64, 1.9_real64, 0.9_real64], [2, 4])
                    real(real64) :: dup_coords(2, 9) = reshape( &
                        [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [2, 9])
                    real(real64) :: col_coords(2, 7) = reshape( &
                        [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, 4.0_real64, &
                         0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64, 7.0_real64, 0.0_real64], [2, 7])

                    ! single node query: r=1 finds 2 nodes on unit euclidean sphere
                    call t%build(unit_coords)
                    res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.0_real64, metric='euclidean')
                    if (size(res) .ne. 2) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! empty result: centroid far away
                    res = t%rNN_Centroid([100.0_real64, 100.0_real64], 0.1_real64, metric='euclidean')
                    if (size(res) .ne. 0) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! duplicates: all 9 at (5,5) found at r=0.01
                    call t%destroy()
                    call t%build(dup_coords)
                    res = t%rNN_Centroid([5.0_real64, 5.0_real64], 0.01_real64, metric='euclidean')
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! collinear: all 7 on x-axis found
                    call t%destroy()
                    call t%build(col_coords)
                    res = t%rNN_Centroid([4.0_real64, 0.0_real64], 3.5_real64, metric='euclidean')
                    if (size(res) .ne. 7) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V020_RNN_EUCLIDEAN ---'
                write(*, '(A)') 'one or more threads got wrong rNN result'
                stop 1
            end if
        end subroutine v020RnnEuclidean
end program Testv030_MULTITHREAD_V020_RNN_EUCLIDEAN
