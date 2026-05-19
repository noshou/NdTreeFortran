program Testv050_MULTITHREAD_CONCURRENT_RMV_POP
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call concurrentRmvPop()
    contains
        !> 4 threads simultaneously call rmvNodes targeting disjoint subsets of a
        !! 16-node grid (4 nodes per thread). After the parallel region pop=0.
        subroutine concurrentRmvPop()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 16)
            real(real64)   :: threadCoords(2, 4, 4)
            integer(int64) :: pop
            integer        :: i, j, k

            k = 0
            do j = 0, 3
                do i = 0, 3
                    k = k + 1
                    coords(1, k) = real(i, real64)
                    coords(2, k) = real(j, real64)
                end do
            end do

            do j = 0, 3
                do i = 1, 4
                    threadCoords(1, i, j+1) = real(i-1, real64)
                    threadCoords(2, i, j+1) = real(j,   real64)
                end do
            end do

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, threadCoords)
            do j = 1, 4
                block
                    real(real64) :: q(2, 4)
                    integer      :: numRmv
                    q = threadCoords(:, :, j)
                    numRmv = t%rmvNodes(coordsList=q)
                end block
            end do
            !$OMP END PARALLEL DO

            pop = t%getPop()
            if (pop .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_CONCURRENT_RMV_POP ---'
                write(*, '(A,I0)') 'expected pop=0 after 4 threads remove all 16 nodes, got: ', pop
                stop 1
            end if
        end subroutine concurrentRmvPop
end program Testv050_MULTITHREAD_CONCURRENT_RMV_POP
