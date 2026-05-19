program Testv030_ADD_NODES_MULTI_ADD_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call multiAddRnnEuclidean()
    contains
        !> Three addNodes calls add pts at x=1..9, y=0. All 10 pts are searchable.
        !! rNN_Centroid at (4.5,0) r=5 (euclidean) must find all 10 pts.
        subroutine multiAddRnnEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: batch1(2, 3) = reshape( &
                [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64], [2, 3])
            real(real64)               :: batch2(2, 3) = reshape( &
                [4.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64], [2, 3])
            real(real64)               :: batch3(2, 3) = reshape( &
                [7.0_real64, 0.0_real64, 8.0_real64, 0.0_real64, 9.0_real64, 0.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(batch1)
            call t%addNodes(batch2)
            call t%addNodes(batch3)
            res = t%rNN_Centroid([4.5_real64, 0.0_real64], 5.0_real64, metric='euclidean')

            if (size(res) .ne. 10) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_MULTI_ADD_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 10 nodes, got: ', size(res)
                stop 1
            end if
        end subroutine multiAddRnnEuclidean
end program Testv030_ADD_NODES_MULTI_ADD_RNN_EUCLIDEAN
