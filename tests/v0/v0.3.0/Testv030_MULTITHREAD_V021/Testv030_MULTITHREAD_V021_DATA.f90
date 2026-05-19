program Testv030_MULTITHREAD_V021_DATA
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call v021Data()
    contains
        !> 4 threads each independently verify polymorphic data on nodes (v0.2.1 DATA_INPUT cases).
        subroutine v021Data()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    real(real64)               :: coords(3, 6) = reshape( &
                        [5.0_real64, 1.0_real64,  0.92_real64,           &
                        4.0_real64, 2.0_real64,  0.42_real64,           &
                        3.0_real64, 3.0_real64,  0.00003_real64,        &
                        0.0_real64, 0.0_real64,  0.00000031_real64,     &
                        1.0_real64, 5.0_real64, -9.0e7_real64,          &
                        0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
                    character(len=1)           :: data(6) = ['1', '2', '3', '4', '5', '6']
                    type(NdNodePtr), allocatable :: res(:)
                    real(real64)               :: r
                    integer                    :: j
                    logical                    :: found4 = .false., found6 = .false.

                    call t%build(coords, data)
                    r = sqrt(sum(([0.0_real64, 0.0_real64, 0.00000031_real64] - &
                                [0.0_real64, 0.0_real64, 0.0_real64])**2))
                    res = t%rNN_Centroid([0.0_real64, 0.0_real64, 0.0_real64], r)

                    do j = 1, size(res)
                        select type (d => res(j)%p%getData())
                            type is (character(*))
                                if (d .eq. '4') found4 = .true.
                                if (d .eq. '6') found6 = .true.
                        end select
                    end do

                    if (.not. (found4 .and. found6) .or. size(res) .ne. 2) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V021_DATA ---'
                write(*, '(A)') 'one or more threads failed data check'
                stop 1
            end if
        end subroutine v021Data
end program Testv030_MULTITHREAD_V021_DATA
