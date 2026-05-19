program Testv030_MULTITHREAD_V020_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call v020RnnChebyshev()
    contains
        !> 4 threads each independently cover v0.2.0 search scenarios (chebyshev).
        subroutine v020RnnChebyshev()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    type(NdNodePtr), allocatable :: res(:)
                    ! 4 pts: (1,0),(0.6,0.8),(0.9,0.9),(1.9,0.9)
                    ! chebyshev r=0.8 from (0,0): max(0.6,0.8)=0.8 in; others out -> 1 node
                    real(real64) :: coords(2, 4) = reshape( &
                        [1.0_real64, 0.0_real64, 0.6_real64, 0.8_real64, &
                        0.9_real64, 0.9_real64, 1.9_real64, 0.9_real64], [2, 4])
                    ! chebyshev r=1 from (0,0): (1,0)[1 in],(0.6,0.8)[0.8 in],(0.9,0.9)[0.9 in] -> 3
                    real(real64) :: dup_coords(2, 9) = reshape( &
                        [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                        5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [2, 9])
                    real(real64) :: col_coords(2, 7) = reshape( &
                        [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, 4.0_real64, &
                         0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64, 7.0_real64, 0.0_real64], [2, 7])

                    call t%build(coords)
                    ! chebyshev r=0.8: only (0.6,0.8) in (max=0.8 <= 0.8)
                    res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.8_real64, metric='chebyshev')
                    if (size(res) .ne. 1) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%destroy()
                    call t%build(dup_coords)
                    res = t%rNN_Centroid([5.0_real64, 5.0_real64], 0.01_real64, metric='chebyshev')
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%destroy()
                    call t%build(col_coords)
                    res = t%rNN_Centroid([4.0_real64, 0.0_real64], 3.5_real64, metric='chebyshev')
                    if (size(res) .ne. 7) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V020_RNN_CHEBYSHEV ---'
                write(*, '(A)') 'one or more threads got wrong rNN result'
                stop 1
            end if
        end subroutine v020RnnChebyshev
end program Testv030_MULTITHREAD_V020_RNN_CHEBYSHEV
