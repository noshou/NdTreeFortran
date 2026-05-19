program Testv021_ZERO_RADIUS_POPULATED_TREE_RNN_NODE_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call zeroRadiusPopulatedTree_rNN_Node_Euclidean()
    contains 

        !> Query should return one node (target node); tree is populated
        subroutine zeroRadiusPopulatedTree_rNN_Node_Euclidean()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                4.0_real64, 2.0_real64,  0.42_real64,             &
                3.0_real64, 3.0_real64,  0.00003_real64,          &
                0.0_real64, 0.0_real64,  0.00000031_real64,       &
                1.0_real64, 5.0_real64, -93131913.0_real64,       &
                0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)
            real(real64)               :: r
            
            call t%build(coords)
            r = sqrt(sum(([0.0_real64, 0.0_real64,  0.00000031_real64] - [0.0_real64, 0.0_real64,  0.0_real64])**2))
            centroid_res = t%rNN_Centroid([0.0_real64, 0.0_real64, 0.0_real64], r)
            
            res = t%rNN_Node(centroid_res(1), 0.0_real64, metric='euclidean')
            if (size(res) .ne. 1) then
                write(*, '(A)') '--- zeroRadiusPopulatedTree_rNN_Node_Euclidean ---'
                write(*,*) 'expected 1 node, got:', size(res)
                stop 1
            end if

        end subroutine zeroRadiusPopulatedTree_rNN_Node_Euclidean

end program Testv021_ZERO_RADIUS_POPULATED_TREE_RNN_NODE_EUCLIDEAN