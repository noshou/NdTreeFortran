program Testv030_MULTITHREAD_V021_LIFECYCLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call v021Lifecycle()
    contains
        !> 4 threads each independently run the v0.2.1 lifecycle suite:
        !! uninitialized checks, build, destroy, rebuild, state assertions.
        subroutine v021Lifecycle()
            integer  :: i
            logical  :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)   :: t
                    logical      :: assertNodePool, assertRoot, assertInitState
                    integer(int64) :: pop, dim
                    real(real64) :: coords(3, 5) = reshape( &
                        [1.0_real64, 0.0_real64, 0.0_real64, &
                        0.0_real64, 1.0_real64, 0.0_real64, &
                        0.0_real64, 0.0_real64, 1.0_real64, &
                        2.0_real64, 2.0_real64, 2.0_real64, &
                        3.0_real64, 0.0_real64, 0.0_real64], [3, 5])

                    ! uninitialized state
                    call t%associatedNodePool(assertNodePool)
                    call t%associatedRoot(assertRoot)
                    call t%getInitState(assertInitState)
                    if (assertNodePool .or. assertRoot .or. assertInitState) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! after build
                    call t%build(coords)
                    pop = t%getPop(); dim = t%getDim()
                    call t%associatedNodePool(assertNodePool)
                    call t%associatedRoot(assertRoot)
                    call t%getInitState(assertInitState)
                    if (pop .ne. 5_int64 .or. dim .ne. 3_int64 .or. &
                        .not. assertNodePool .or. .not. assertRoot  &
                        .or. .not. assertInitState                  &
                        ) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                    end if

                    ! after destroy
                    call t%destroy()
                    call t%associatedNodePool(assertNodePool)
                    call t%associatedRoot(assertRoot)
                    call t%getInitState(assertInitState)
                    if (assertNodePool .or. assertRoot .or. assertInitState) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! after rebuild
                    call t%build(coords)
                    pop = t%getPop()
                    call t%getInitState(assertInitState)
                    if (pop .ne. 5_int64 .or. .not. assertInitState) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_V021_LIFECYCLE ---'
                write(*, '(A)') 'one or more threads failed lifecycle check'
                stop 1
            end if
        end subroutine v021Lifecycle
end program Testv030_MULTITHREAD_V021_LIFECYCLE
