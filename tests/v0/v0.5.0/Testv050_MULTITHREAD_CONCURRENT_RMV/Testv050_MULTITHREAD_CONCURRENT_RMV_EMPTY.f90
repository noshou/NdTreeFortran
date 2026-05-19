program Testv050_MULTITHREAD_CONCURRENT_RMV_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call concurrentRmvEmpty()
    contains
        !> After draining the tree to pop=0, 4 threads concurrently call rmvNodes.
        !! All must return numRmv=0, not crash, and leave pop=0 and isInit=T.
        subroutine concurrentRmvEmpty()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)   :: bigQuery(2, 1)  = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)   :: bigRadii(1)     = [100.0_real64]
            logical        :: failed = .false.
            logical        :: isInit
            integer(int64) :: pop
            integer        :: i

            call t%build(coords)
            ! drain the tree first (single-threaded, guaranteed)
            i = t%rmvNodes(coordsList=bigQuery, radii=bigRadii)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, failed)
            do i = 1, 4
                block
                    real(real64)   :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
                    real(real64)   :: r(1)    = [1.0_real64]
                    integer        :: numRmv
                    numRmv = t%rmvNodes(coordsList=q, radii=r)
                    if (numRmv .ne. 0) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_CONCURRENT_RMV_EMPTY ---'
                write(*, '(A)') 'one or more threads returned numRmv > 0 on empty tree'
                stop 1
            end if

            pop = t%getPop()
            call t%getInitState(isInit)

            if (pop .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_EMPTY ---'
                write(*, '(A,I0)') 'expected pop=0 after concurrent rmvNodes on empty tree, got: ', pop
                stop 1
            end if
            if (.not. isInit) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_CONCURRENT_RMV_EMPTY ---'
                write(*, '(A)') 'expected isInit=T after concurrent rmvNodes on empty tree'
                stop 1
            end if
        end subroutine concurrentRmvEmpty
end program Testv050_MULTITHREAD_CONCURRENT_RMV_EMPTY
