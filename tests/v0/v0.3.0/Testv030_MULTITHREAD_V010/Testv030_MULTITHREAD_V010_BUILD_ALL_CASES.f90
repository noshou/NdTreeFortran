program Testv030_MULTITHREAD_V010_BUILD_ALL_CASES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call v010BuildAllCases()
    contains
        !> 4 threads each independently build trees mirroring v0.1.0 SIMPLE and MULTI_AXIS cases
        !! (1pt, 2pt, 1D/2D/3D/4D arrays). Each thread verifies pop and dim.
        subroutine v010BuildAllCases()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)   :: t1, t2, t3, t4, t5, t6

                    real(real64) :: one_pt(1, 1) = reshape([1.0_real64], [1, 1])
                    real(real64) :: two_pts(2, 2) = reshape( &
                        [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2])
                    real(real64) :: one_axis(1, 7) = reshape( &
                        [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                        5.0_real64, 6.0_real64, 7.0_real64], [1, 7])
                    real(real64) :: two_axis(2, 7) = reshape( &
                        [1.0_real64, 0.0_real64, 2.0_real64, 1.0_real64, 3.0_real64, 0.0_real64, &
                        4.0_real64, 2.0_real64, 5.0_real64, 1.0_real64, 6.0_real64, 0.0_real64, &
                        7.0_real64, 3.0_real64], [2, 7])
                    real(real64) :: three_axis(3, 7) = reshape( &
                        [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64, &
                        7.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
                        6.0_real64, 7.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
                        5.0_real64, 6.0_real64, 7.0_real64], [3, 7])
                    real(real64) :: four_axis(4, 7) = reshape( &
                        [3.0_real64, 1.0_real64, -13.31_real64,     0.92_real64,  &
                        1.0_real64, 4.0_real64,  34.14_real64,    92.0_real64,   &
                        4.0_real64, 1.0_real64, -1093.3_real64,  312.0_real64,   &
                        1.0_real64, 5.0_real64,   0.013_real64, 13112.0_real64,  &
                        5.0_real64, 9.0_real64,   33.13_real64,   -5.13_real64,  &
                        9.0_real64, 2.0_real64,  734.35_real64,    0.00_real64,  &
                        2.0_real64, 6.0_real64,  -93.13_real64,    0.94_real64], [4, 7])

                    call t1%build(one_pt)
                    if (t1%getPop() .ne. 1_int64 .or. t1%getDim() .ne. 1_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                    
                    call t2%build(two_pts)
                    if (t2%getPop() .ne. 2_int64 .or. t2%getDim() .ne. 2_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                    
                    call t3%build(one_axis)
                    if (t3%getPop() .ne. 7_int64 .or. t3%getDim() .ne. 1_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                    
                    call t4%build(two_axis)
                    if (t4%getPop() .ne. 7_int64 .or. t4%getDim() .ne. 2_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                    
                    call t5%build(three_axis); if (t5%getPop() .ne. 7_int64 .or. t5%getDim() .ne. 3_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                    
                    call t6%build(four_axis)
                    if (t6%getPop() .ne. 7_int64 .or. t6%getDim() .ne. 4_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V010_BUILD_ALL_CASES ---'
                write(*, '(A)') 'one or more threads got wrong pop or dim'
                stop 1
            end if
        end subroutine v010BuildAllCases
end program Testv030_MULTITHREAD_V010_BUILD_ALL_CASES
