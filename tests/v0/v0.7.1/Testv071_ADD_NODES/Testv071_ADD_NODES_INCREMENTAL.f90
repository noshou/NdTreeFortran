program Testv071_ADD_NODES_INCREMENTAL
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call incrementalRnnEuclidean()
    contains
        !> Add one node at a time at (k*100, 0); after each add verify rNN_Centroid
        !! at that point with r=0.5 (euclidean) finds exactly 1 node.
        subroutine incrementalRnnEuclidean()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64)               :: step(2, 1)
            type(NdNodePtr), allocatable :: res(:)
            integer                    :: k

            call t%build(init_coords)
            call t%setRebuildRatio(0.9_real64)

            do k = 1, 5
                step(1, 1) = real(k, real64) * 100.0_real64
                step(2, 1) = 0.0_real64
                call t%addNodes(step)

                res = t%rNN_Centroid([step(1,1), 0.0_real64], 0.5_real64, metric='euclidean')
                if (size(res) .ne. 1) then
                    write(*, '(A)')      '--- Testv071_ADD_NODES_INCREMENTAL ---'
                    write(*, '(A,I0,A,I0)') 'after adding node ', k, ' expected rNN size=1, got: ', size(res)
                    stop 1
                end if
            end do
        end subroutine incrementalRnnEuclidean
end program Testv071_ADD_NODES_INCREMENTAL
