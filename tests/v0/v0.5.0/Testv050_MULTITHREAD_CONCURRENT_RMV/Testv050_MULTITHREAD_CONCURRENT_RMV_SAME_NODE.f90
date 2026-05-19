program Testv050_MULTITHREAD_CONCURRENT_RMV_SAME_NODE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call concurrentRmvSameNode()
    contains
        !> 4 threads all try to remove the same node by id concurrently.
        !! The critical section re-checks the pool, so only the first thread
        !! that enters the critical section performs the removal; the remaining
        !! 3 find the node already gone and remove nothing.
        !! Result: pop=4.
        subroutine concurrentRmvSameNode()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: res(:)
            type(NodeId)                 :: targetId(1)
            integer(int64)               :: pop
            integer                      :: i

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_SAME_NODE ---'
                write(*, '(A,I0)') 'expected 1 node at origin, got: ', size(res)
                stop 1
            end if
            targetId(1) = res(1)%p%getNodeId()

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, targetId)
            do i = 1, 4
                block
                    integer :: numRmv
                    numRmv = t%rmvNodes(ids=targetId)
                end block
            end do
            !$OMP END PARALLEL DO

            pop = t%getPop()

            if (pop .ne. 4_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_SAME_NODE ---'
                write(*, '(A,I0)') 'expected pop=4 (only 1 of 4 threads removes the node), got: ', pop
                stop 1
            end if
        end subroutine concurrentRmvSameNode
end program Testv050_MULTITHREAD_CONCURRENT_RMV_SAME_NODE
