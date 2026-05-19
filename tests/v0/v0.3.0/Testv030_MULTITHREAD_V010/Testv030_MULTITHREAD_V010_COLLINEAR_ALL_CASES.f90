program Testv030_MULTITHREAD_V010_COLLINEAR_ALL_CASES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call v010CollinearAllCases()
    contains
        !> 4 threads each build trees with collinear pts matching v0.1.0 COLLINEAR cases.
        !! Verifies pop and that rNN can find all collinear pts.
        subroutine v010CollinearAllCases()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t1, t2, t3
                    type(NdNodePtr), allocatable :: res(:)

                    ! 2D collinear along axis 1 (x-axis): (1,0),(2,0),...,(7,0)
                    real(real64) :: col1(2, 7) = reshape( &
                        [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, 4.0_real64, &
                         0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64, 7.0_real64, 0.0_real64], [2, 7])
                    ! 2D collinear along axis 2 (y-axis): (0,1),(0,2),...,(0,7)
                    real(real64) :: col2(2, 7) = reshape( &
                        [0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, &
                         4.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64, 7.0_real64], [2, 7])
                    ! 3D collinear along axis 3 (z-axis): (0,0,1),(0,0,2),...,(0,0,7)
                    real(real64) :: col3(3, 7) = reshape( &
                        [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 2.0_real64, &
                         0.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, 0.0_real64, 4.0_real64, &
                         0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 6.0_real64, &
                         0.0_real64, 0.0_real64, 7.0_real64], [3, 7])

                    call t1%build(col1)
                    res = t1%rNN_Centroid([4.0_real64, 0.0_real64], 3.5_real64)
                    if (size(res) .ne. 7) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t2%build(col2)
                    res = t2%rNN_Centroid([0.0_real64, 4.0_real64], 3.5_real64)
                    if (size(res) .ne. 7) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t3%build(col3)
                    res = t3%rNN_Centroid([0.0_real64, 0.0_real64, 4.0_real64], 3.5_real64)
                    if (size(res) .ne. 7) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V010_COLLINEAR_ALL_CASES ---'
                write(*, '(A)') 'one or more threads got wrong rNN count for collinear case'
                stop 1
            end if
        end subroutine v010CollinearAllCases
end program Testv030_MULTITHREAD_V010_COLLINEAR_ALL_CASES
