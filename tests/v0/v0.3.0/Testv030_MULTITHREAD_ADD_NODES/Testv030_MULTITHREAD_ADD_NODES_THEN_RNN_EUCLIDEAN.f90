program Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesThenRnnEuclidean()
    contains
        !> Sequential addNodes on shared tree; then 4 threads concurrently search.
        !! All threads must find the same count of nodes within the radius.
        subroutine addNodesThenRnnEuclidean()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64) :: new_coords(2, 5) = reshape( &
                [1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, &
                2.0_real64, 2.0_real64, 0.5_real64, 0.5_real64], [2, 5])
            integer      :: i
            logical      :: failed = .false.

            call t%build(init_coords)
            call t%addNodes(new_coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, failed)
            do i = 1, 4
                block
                    type(NdNodePtr), allocatable :: res(:)
                    
                    ! centroid (1.5,1.5) r=1.5 euclidean: finds pts within 1.5 of center
                    res = t%rNN_Centroid([1.5_real64, 1.5_real64], 1.5_real64, metric='euclidean')
                    
                    ! (1,1)[d=sqrt(0.5)~0.7],(2,1)[d=sqrt(0.5)],(1,2)[d=sqrt(0.5)],(2,2)[d=sqrt(0.5)],(0.5,0.5)[d=sqrt(2)~1.4]
                    if (size(res) .ne. 5) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_EUCLIDEAN ---'
                write(*, '(A)') 'one or more threads got wrong concurrent rNN result after addNodes'
                stop 1
            end if
        end subroutine addNodesThenRnnEuclidean
end program Testv030_MULTITHREAD_ADD_NODES_THEN_RNN_EUCLIDEAN
