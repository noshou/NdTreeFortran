program Testv071_ADD_NODES_COLLINEAR
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call collinearOneAxisRnnEuclidean()
    contains
        !> Build 2D tree with 2 non-collinear pts. Add 5 pts collinear along x-axis (y=0).
        !! rNN_Centroid at (2,0) r=2.5 (euclidean) must find all 5 added collinear pts.
        subroutine collinearOneAxisRnnEuclidean()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 2) = reshape( &
                [0.0_real64, 100.0_real64, 100.0_real64, 0.0_real64], [2, 2])
            real(real64)               :: col_coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(col_coords)
            res = t%rNN_Centroid([2.0_real64, 0.0_real64], 2.5_real64, metric='euclidean')

            if (size(res) .ne. 5) then
                write(*, '(A)')   '--- Testv071_ADD_NODES_COLLINEAR ---'
                write(*, '(A,I0)') 'expected 5 collinear nodes, got: ', size(res)
                stop 1
            end if
        end subroutine collinearOneAxisRnnEuclidean
end program Testv071_ADD_NODES_COLLINEAR
