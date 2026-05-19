program Testv020_COLLINEAR_ONE_AXIS_II_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearOne_AxisII_rNN_Euclidean()
    contains
        !> 3D, all y=4. rNN_Centroid at (13,4,0) r=12.5, euclidean.
        !! P1=(1,4,0.92): dist≈12.04 in. P3=(13,4,0.00003): dist≈0 in.
        !! Others: dist>12.5. Expect 2.
        subroutine collinearOne_AxisII_rNN_Euclidean()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape(           &
                [  1.0_real64, 4.0_real64,  0.92_real64,      &
                52.0_real64, 4.0_real64,  0.42_real64,      &
                13.0_real64, 4.0_real64,  0.00003_real64,   &
                87.0_real64, 4.0_real64,  93291.0_real64,   &
                -98.0_real64, 4.0_real64, -93131913.0_real64,&
                121.0_real64, 4.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([13.0_real64, 4.0_real64, 0.0_real64], 12.5_real64)
            if (size(res) .ne. 2) then
                write(*, '(A)') '--- Testv020_COLLINEAR_ONE_AXIS_II_RNN_EUCLIDEAN ---'
                write(*, *) 'expected 2 nodes, got:', size(res)
                stop 1
            end if
        end subroutine collinearOne_AxisII_rNN_Euclidean
end program Testv020_COLLINEAR_ONE_AXIS_II_RNN_EUCLIDEAN
