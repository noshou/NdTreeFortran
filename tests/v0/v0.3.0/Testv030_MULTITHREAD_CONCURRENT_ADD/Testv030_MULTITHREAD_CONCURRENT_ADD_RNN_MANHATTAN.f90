program Testv030_MULTITHREAD_CONCURRENT_ADD_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call concurrentAddRnnManhattan()
    contains
        subroutine concurrentAddRnnManhattan()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: all_coords(2, 3, 4)
            type(NdNodePtr), allocatable :: res(:)
            integer                    :: i, j

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

            res = t%rNN_Centroid([6.5_real64, 0.0_real64], 7.0_real64, metric='manhattan')
            if (size(res) .ne. 13) then
                write(*, '(A)')   '--- Testv030_MULTITHREAD_CONCURRENT_ADD_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 13 nodes, got: ', size(res)
                stop 1
            end if
        end subroutine concurrentAddRnnManhattan
end program Testv030_MULTITHREAD_CONCURRENT_ADD_RNN_MANHATTAN
