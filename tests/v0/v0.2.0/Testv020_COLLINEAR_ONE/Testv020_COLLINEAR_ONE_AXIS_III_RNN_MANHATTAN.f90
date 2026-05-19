program Testv020_COLLINEAR_ONE_AXIS_III_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearOne_AxisIII_rNN_Manhattan()
    contains
        !> 3D, all z=4. rNN_Centroid at (13,0,4) r=12.5, manhattan.
        !! P1=(1,0.92,4): dist=12+0.92+0=12.92 out. P3=(13,0.00003,4): dist≈0 in.
        !! Others: dist>12.5. Expect 1.
        subroutine collinearOne_AxisIII_rNN_Manhattan()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape(             &
                [  1.0_real64,  0.92_real64,       4.0_real64,  &
                52.0_real64,  0.42_real64,       4.0_real64,  &
                13.0_real64,  0.00003_real64,    4.0_real64,  &
                87.0_real64,  93291.0_real64,    4.0_real64,  &
                -98.0_real64, -93131913.0_real64, 4.0_real64,  &
                121.0_real64,  0.0_real64,        4.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([13.0_real64, 0.0_real64, 4.0_real64], 12.5_real64, metric='manhattan')
            if (size(res) .ne. 1) then
                write(*, '(A)') '--- Testv020_COLLINEAR_ONE_AXIS_III_RNN_MANHATTAN ---'
                write(*, *) 'expected 1 node, got:', size(res)
                stop 1
            end if
        end subroutine collinearOne_AxisIII_rNN_Manhattan
end program Testv020_COLLINEAR_ONE_AXIS_III_RNN_MANHATTAN
