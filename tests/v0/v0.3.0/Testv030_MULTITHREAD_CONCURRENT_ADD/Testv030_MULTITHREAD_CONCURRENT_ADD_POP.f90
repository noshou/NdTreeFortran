program Testv030_MULTITHREAD_CONCURRENT_ADD_POP
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call concurrentAddPop()
    contains
        !> 4 threads each add 3 nodes concurrently to the same tree.
        !! The CRITICAL section in addNodes serializes access; final pop must be exact.
        subroutine concurrentAddPop()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)   :: all_coords(2, 3, 4)
            integer        :: i, j
            integer(int64) :: pop

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

            pop = t%getPop()
            if (pop .ne. 13_int64) then
                write(*, '(A)')         '--- Testv030_MULTITHREAD_CONCURRENT_ADD_POP ---'
                write(*, '(A,I0,A,I0)') 'expected pop = 13, got: ', pop
                stop 1
            end if
        end subroutine concurrentAddPop
end program Testv030_MULTITHREAD_CONCURRENT_ADD_POP
