program Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call collinearTwoAxisRnnManhattan()
    contains
        subroutine collinearTwoAxisRnnManhattan()
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
            res = t%rNN_Centroid([0.0_real64, 0.0_real64, 2.0_real64], 2.5_real64, metric='manhattan')

            if (size(res) .ne. 5) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 5 collinear nodes, got: ', size(res)
                stop 1
            end if
        end subroutine collinearTwoAxisRnnManhattan
end program Testv030_ADD_NODES_COLLINEAR_TWO_AXIS_RNN_MANHATTAN
