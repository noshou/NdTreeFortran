program Testv030_MULTITHREAD_V021_IS_MEMBER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call v021IsMember()
    contains
        !> 4 threads each independently run v0.2.1 isMember scenarios:
        !! member check, non-member after destroy, non-member after rebuild.
        subroutine v021IsMember()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    real(real64)               :: coords(2, 3) = reshape( &
                        [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
                    type(NdNodePtr), allocatable :: res(:)
                    type(NdNode),    pointer     :: n

                    ! node is member
                    call t%build(coords)
                    res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
                    n => res(1)%p
                    if (.not. t%isMember(n)) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! node not member after destroy
                    call t%destroy()
                    if (t%isMember(n)) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! node not member after rebuild (new treeId)
                    call t%build(coords)
                    if (t%isMember(n)) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V021_IS_MEMBER ---'
                write(*, '(A)') 'one or more threads failed isMember check'
                stop 1
            end if
        end subroutine v021IsMember
end program Testv030_MULTITHREAD_V021_IS_MEMBER
