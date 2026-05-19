program Testv020_COLLINEAR_TWO_AXIS_II_AXIS_III_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearTwo_AxisIIAxisIII_rNN_Euclidean()
    contains
        !> 3D, y=5 z=4; only x varies. rNN_Centroid at (0,5,4) r=35, euclidean.
        !! x=1: dist=1 in. x=2: dist=2 in. x=-31: dist=31 in.
        !! Others: dist>35. Expect 3.
        subroutine collinearTwo_AxisIIAxisIII_rNN_Euclidean()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape(  &
                [  2.0_real64,    5.0_real64, 4.0_real64, &
                1.0_real64,    5.0_real64, 4.0_real64, &
                -131.0_real64,   5.0_real64, 4.0_real64, &
                31313.0_real64, 5.0_real64, 4.0_real64, &
                -31.0_real64,    5.0_real64, 4.0_real64, &
                432.419_real64, 5.0_real64, 4.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 5.0_real64, 4.0_real64], 35.0_real64)
            if (size(res) .ne. 3) then
                write(*, '(A)') '--- Testv020_COLLINEAR_TWO_AXIS_II_AXIS_III_RNN_EUCLIDEAN ---'
                write(*, *) 'expected 3 nodes, got:', size(res)
                stop 1
            end if
        end subroutine collinearTwo_AxisIIAxisIII_rNN_Euclidean
end program Testv020_COLLINEAR_TWO_AXIS_II_AXIS_III_RNN_EUCLIDEAN
