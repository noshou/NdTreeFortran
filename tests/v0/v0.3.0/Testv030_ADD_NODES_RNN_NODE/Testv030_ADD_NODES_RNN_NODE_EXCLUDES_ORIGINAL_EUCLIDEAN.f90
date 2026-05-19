program Testv030_ADD_NODES_RNN_NODE_EXCLUDES_ORIGINAL_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnNodeExcludesOriginalEuclidean()
    contains
        !> Build 3 near-origin pts; add 2 far pts.
        !! rNN_Node from (0,0) with small radius must find only original nodes.
        subroutine rnnNodeExcludesOriginalEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [100.0_real64, 100.0_real64, 200.0_real64, 200.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: anchor(:), res(:)
            type(NdNodePtr)              :: target

            call t%build(init_coords)
            anchor = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            target%p => anchor(1)%p
            call t%addNodes(new_coords)
            res = t%rNN_Node(target, 1.5_real64, metric='euclidean')

            if (size(res) .ne. 3) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_RNN_NODE_EXCLUDES_ORIGINAL_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 3 nodes (original only), got: ', size(res)
                stop 1
            end if
        end subroutine rnnNodeExcludesOriginalEuclidean
end program Testv030_ADD_NODES_RNN_NODE_EXCLUDES_ORIGINAL_EUCLIDEAN
