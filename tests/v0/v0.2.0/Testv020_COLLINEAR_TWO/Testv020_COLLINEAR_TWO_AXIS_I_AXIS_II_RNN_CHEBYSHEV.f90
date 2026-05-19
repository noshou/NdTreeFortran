program Testv020_COLLINEAR_TWO_AXIS_I_AXIS_II_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearTwo_AxisIAxisII_rNN_Chebyshev()
    contains
        !> 3D, x=5 y=4; only z varies. rNN_Centroid at (5,4,0) r=35, chebyshev.
        !! Only z differs from centroid; L1=L2=L∞ here. Expect 3 (z=1,2,-31).
        subroutine collinearTwo_AxisIAxisII_rNN_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape(  &
                [5.0_real64, 4.0_real64,  2.0_real64,    &
                5.0_real64, 4.0_real64,  1.0_real64,    &
                5.0_real64, 4.0_real64, -131.0_real64,  &
                5.0_real64, 4.0_real64,  31313.0_real64,&
                5.0_real64, 4.0_real64, -31.0_real64,   &
                5.0_real64, 4.0_real64,  432.419_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([5.0_real64, 4.0_real64, 0.0_real64], 35.0_real64, metric='chebyshev')
            if (size(res) .ne. 3) then
                write(*, '(A)') '--- Testv020_COLLINEAR_TWO_AXIS_I_AXIS_II_RNN_CHEBYSHEV ---'
                write(*, *) 'expected 3 nodes, got:', size(res)
                stop 1
            end if
        end subroutine collinearTwo_AxisIAxisII_rNN_Chebyshev
end program Testv020_COLLINEAR_TWO_AXIS_I_AXIS_II_RNN_CHEBYSHEV
