program Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesThenRnnManhattan()
    contains
        !> Sequential addNodes; then 4 threads concurrently search with manhattan metric.
        !! Initial pts are far from origin; added pts near origin.
        subroutine addNodesThenRnnManhattan()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [50.0_real64, 50.0_real64, -50.0_real64,  50.0_real64, &
                50.0_real64, -50.0_real64, -50.0_real64, -50.0_real64], [2, 4])
            real(real64) :: new_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            integer      :: i
            logical      :: failed = .false.

            call t%build(init_coords)
            call t%addNodes(new_coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, failed)
            do i = 1, 4
                block
                    type(NdNodePtr), allocatable :: res(:)
                    
                    ! centroid (0,0) r=1.5 manhattan: (0,0)[0],(1,0)[1],(0,1)[1] -> 3 nodes
                    res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='manhattan')
                    if (size(res) .ne. 3) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_MANHATTAN ---'
                write(*, '(A)') 'one or more threads got wrong concurrent rNN result after addNodes'
                stop 1
            end if
        end subroutine addNodesThenRnnManhattan
end program Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_MANHATTAN
