!> Expected-fail: manhattanDist between a 2D node and a 3D node must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NODE_QUERY_MANHATTAN_AXIS_MISMATCH

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nodeQuery_Manhattan_AxisMismatch()
    contains

        subroutine nodeQuery_Manhattan_AxisMismatch()
            type(KdTree)                 :: t2, t3
            real(real64)               :: coords2(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64], [2, 3])
            real(real64)               :: coords3(3, 3) = reshape( &
                [1.0_real64, 1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64, 1.0_real64], [3, 3])
            type(NdNodePtr), allocatable :: res2(:), res3(:)
            real(real64)               :: dist

            call t2%build(coords2)
            call t3%build(coords3)
            res2 = t2%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            res3 = t3%rNN_Centroid([1.0_real64, 1.0_real64, 1.0_real64], 0.01_real64)

            dist = res2(1)%p%manhattanDist(res3(1)%p)
            write(*, '(A)') '--- Testv020_NODE_QUERY_MANHATTAN_AXIS_MISMATCH ---'
            write(*,*) 'expected error stop, but manhattanDist returned normally'
        end subroutine nodeQuery_Manhattan_AxisMismatch

end program Testv020_NODE_QUERY_MANHATTAN_AXIS_MISMATCH
