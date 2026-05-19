program Testv030_ADD_NODES_RNN_CENTROID_EXCLUDES_ORIGINAL_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnCentroidExcludesOriginalEuclidean()
    contains
        !> Build 3 near-origin pts; add 2 far pts. rNN_Centroid at origin with small
        !! radius (euclidean) must find only the 3 original nodes.
        subroutine rnnCentroidExcludesOriginalEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [100.0_real64, 100.0_real64, 200.0_real64, 200.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(new_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='euclidean')

            if (size(res) .ne. 3) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_RNN_CENTROID_EXCLUDES_ORIGINAL_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 3 nodes (original only), got: ', size(res)
                stop 1
            end if
        end subroutine rnnCentroidExcludesOriginalEuclidean
end program Testv030_ADD_NODES_RNN_CENTROID_EXCLUDES_ORIGINAL_EUCLIDEAN
