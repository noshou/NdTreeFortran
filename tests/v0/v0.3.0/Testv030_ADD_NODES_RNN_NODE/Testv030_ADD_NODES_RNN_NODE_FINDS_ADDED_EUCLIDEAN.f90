program Testv030_ADD_NODES_RNN_NODE_FINDS_ADDED_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnNodeFindsAddedEuclidean()
    contains
        !> Build tree with (0,0),(10,0),(0,10). Add (1,0),(0,1).
        !! rNN_Node from (0,0) with r=1.5 (euclidean) finds (0,0),(1,0),(0,1) = 3 nodes.
        subroutine rnnNodeFindsAddedEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, 0.0_real64, 10.0_real64], [2, 3])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: anchor(:), res(:)
            type(NdNodePtr)              :: target

            call t%build(init_coords)
            anchor = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            target%p => anchor(1)%p
            call t%addNodes(new_coords)
            res = t%rNN_Node(target, 1.5_real64, metric='euclidean')

            if (size(res) .ne. 3) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_RNN_NODE_FINDS_ADDED_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 3 nodes, got: ', size(res)
                stop 1
            end if
        end subroutine rnnNodeFindsAddedEuclidean
end program Testv030_ADD_NODES_RNN_NODE_FINDS_ADDED_EUCLIDEAN
