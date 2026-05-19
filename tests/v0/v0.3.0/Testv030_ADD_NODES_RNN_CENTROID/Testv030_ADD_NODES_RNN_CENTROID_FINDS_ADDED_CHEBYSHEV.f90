program Testv030_ADD_NODES_RNN_CENTROID_FINDS_ADDED_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnCentroidFindsAddedChebyshev()
    contains
        !> Build far-away pts; add 3 near origin. rNN_Centroid at origin, r=1.5 (chebyshev)
        !! must find all 3 added nodes: (0,0)[d=0], (1,0)[d=1], (0,1)[d=1].
        subroutine rnnCentroidFindsAddedChebyshev()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [50.0_real64, 50.0_real64, -50.0_real64,  50.0_real64, &
                50.0_real64, -50.0_real64, -50.0_real64, -50.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(new_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='chebyshev')

            if (size(res) .ne. 3) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_RNN_CENTROID_FINDS_ADDED_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 3 nodes, got: ', size(res)
                stop 1
            end if
        end subroutine rnnCentroidFindsAddedChebyshev
end program Testv030_ADD_NODES_RNN_CENTROID_FINDS_ADDED_CHEBYSHEV
