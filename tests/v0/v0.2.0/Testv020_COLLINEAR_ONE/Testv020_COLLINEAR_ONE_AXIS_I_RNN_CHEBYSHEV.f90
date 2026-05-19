program Testv020_COLLINEAR_ONE_AXIS_I_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearOne_AxisI_rNN_Chebyshev()
    contains
        !> 3D, all x=5. rNN_Centroid at (5,2,0) r=1.5, chebyshev.
        !! P1=(5,1,0.92): dist=max(0,1,0.92)=1 in. P2=(5,2,0.42): dist=0.42 in.
        !! P3=(5,3,0.00003): dist=1 in. Others: dist>1.5. Expect 3.
        subroutine collinearOne_AxisI_rNN_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                5.0_real64, 2.0_real64,  0.42_real64,             &
                5.0_real64, 3.0_real64,  0.00003_real64,          &
                5.0_real64, 4.0_real64,  93291.0_real64,          &
                5.0_real64, 5.0_real64, -93131913.0_real64,       &
                5.0_real64, 6.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([5.0_real64, 2.0_real64, 0.0_real64], 1.5_real64, metric='chebyshev')
            if (size(res) .ne. 3) then
                write(*, '(A)') '--- Testv020_COLLINEAR_ONE_AXIS_I_RNN_CHEBYSHEV ---'
                write(*, *) 'expected 3 nodes, got:', size(res)
                stop 1
            end if
        end subroutine collinearOne_AxisI_rNN_Chebyshev
end program Testv020_COLLINEAR_ONE_AXIS_I_RNN_CHEBYSHEV
