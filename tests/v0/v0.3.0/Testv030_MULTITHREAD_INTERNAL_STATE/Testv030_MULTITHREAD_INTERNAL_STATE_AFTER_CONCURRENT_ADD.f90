program Testv030_MULTITHREAD_INTERNAL_STATE_AFTER_CONCURRENT_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call internalStateAfterConcurrentAdd()
    contains
        !> 4 threads each add 3 nodes concurrently; after the parallel region verify
        !! pop=13, getRebuildRatio=0.25, getInitState=T, nodePool associated, root associated.
        !! numMods is not checked here as its value depends on CRITICAL-section ordering.
        subroutine internalStateAfterConcurrentAdd()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)   :: all_coords(2, 3, 4)
            integer        :: i, j
            integer(int64) :: pop
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            do i = 1, 4
                do j = 1, 3
                    all_coords(1, j, i) = real((i-1)*3 + j, real64)
                    all_coords(2, j, i) = 0.0_real64
                end do
            end do

            call t%build(init_coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1)
            do i = 1, 4
                call t%addNodes(all_coords(:,:,i))
            end do
            !$OMP END PARALLEL DO

            pop   = t%getPop()
            ratio = t%getRebuildRatio()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (pop .ne. 13_int64) then
                write(*, '(A)')    '--- Testv030_MULTITHREAD_INTERNAL_STATE_AFTER_CONCURRENT_ADD ---'
                write(*, '(A,I0)') 'expected pop=13, got: ', pop
                stop 1
            end if
            if (ratio .ne. 0.25_real64) then
                write(*, '(A)')      '--- Testv030_MULTITHREAD_INTERNAL_STATE_AFTER_CONCURRENT_ADD ---'
                write(*, '(A,F8.4)') 'expected ratio=0.25 unchanged, got: ', ratio
                stop 1
            end if
            if (.not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                write(*, '(A)')    '--- Testv030_MULTITHREAD_INTERNAL_STATE_AFTER_CONCURRENT_ADD ---'
                write(*, '(A,L2)') 'initialized: ', isInit
                write(*, '(A,L2)') 'nodePool:    ', nodePoolAssoc
                write(*, '(A,L2)') 'root:        ', rootAssoc
                stop 1
            end if
        end subroutine internalStateAfterConcurrentAdd
end program Testv030_MULTITHREAD_INTERNAL_STATE_AFTER_CONCURRENT_ADD
