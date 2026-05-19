program Testv030_MULTITHREAD_V010_DUPLICATES_ALL_CASES
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call v010DuplicatesAllCases()
    contains
        !> 4 threads each build trees with all-duplicate pts (1D–4D), matching v0.1.0 DUPLICATES.
        !! Verifies rNN finds all N duplicate nodes.
        subroutine v010DuplicatesAllCases()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t1, t2, t3, t4
                    type(NdNodePtr), allocatable :: res(:)
                    integer                    :: k

                    real(real64) :: dup1(1, 9) = reshape([(5.0_real64, k=1,9)], [1, 9])
                    real(real64) :: dup2(2, 9) = reshape([(5.0_real64, k=1,18)], [2, 9])
                    real(real64) :: dup3(3, 9) = reshape([(5.0_real64, k=1,27)], [3, 9])
                    real(real64) :: dup4(4, 9) = reshape([(5.0_real64, k=1,36)], [4, 9])

                    call t1%build(dup1)
                    res = t1%rNN_Centroid([5.0_real64], 0.01_real64)
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t2%build(dup2)
                    res = t2%rNN_Centroid([5.0_real64, 5.0_real64], 0.01_real64)
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t3%build(dup3)
                    res = t3%rNN_Centroid([5.0_real64, 5.0_real64, 5.0_real64], 0.01_real64)
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t4%build(dup4)
                    res = t4%rNN_Centroid([5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], 0.01_real64)
                    if (size(res) .ne. 9) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V010_DUPLICATES_ALL_CASES ---'
                write(*, '(A)') 'one or more threads got wrong rNN count for duplicate case'
                stop 1
            end if
        end subroutine v010DuplicatesAllCases
end program Testv030_MULTITHREAD_V010_DUPLICATES_ALL_CASES
