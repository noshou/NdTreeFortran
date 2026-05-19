!> Expected-fail: chebyshevDistPoint with a 3D point against a 2D node must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_POINT_QUERY_CHEBYSHEV_AXIS_MISMATCH

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call pointQuery_Chebyshev_AxisMismatch()
    contains

        subroutine pointQuery_Chebyshev_AxisMismatch()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                2.0_real64, 1.0_real64,  &
                3.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            real(real64), allocatable  :: point(:)
            real(real64)               :: dist

            call t%build(coords)
            res  = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            allocate(point, source=[1.0_real64, 1.0_real64, 1.0_real64])

            dist = res(1)%p%chebyshevDistPoint(point)
            write(*, '(A)') '--- pointQuery_Chebyshev_AxisMismatch ---'
            write(*,*) 'expected error stop, but chebyshevDistPoint returned normally'
        end subroutine pointQuery_Chebyshev_AxisMismatch

end program Testv020_POINT_QUERY_CHEBYSHEV_AXIS_MISMATCH
