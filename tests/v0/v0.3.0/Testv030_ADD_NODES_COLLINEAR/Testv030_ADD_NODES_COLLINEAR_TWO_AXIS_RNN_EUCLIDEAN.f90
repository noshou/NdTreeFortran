program Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call collinearTwoAxisRnnEuclidean()
    contains
        !> Build 3D tree with 2 pts. Add 5 pts collinear along z-axis (x=0, y=0).
        !! rNN_Centroid at (0,0,2) r=2.5 (euclidean) finds all 5 added pts.
        subroutine collinearTwoAxisRnnEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(3, 2) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, 100.0_real64, 100.0_real64, 100.0_real64], [3, 2])
            real(real64)               :: col_coords(3, 5) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [3, 5])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(col_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64, 2.0_real64], 2.5_real64, metric='euclidean')

            if (size(res) .ne. 5) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 5 collinear nodes, got: ', size(res)
                stop 1
            end if
        end subroutine collinearTwoAxisRnnEuclidean
end program Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_EUCLIDEAN
