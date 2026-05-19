!> node%chebyshevDistPoint on a 10-point 2D grid tree.
!! Node at (1,1), query point (4,2): expected L∞ distance = 3.0.
program Testv020_POINT_QUERY_CHEBYSHEV

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call pointQuery_Chebyshev()
    contains

        subroutine pointQuery_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 10) = reshape( &
                [1.0_real64, 1.0_real64, &
                2.0_real64, 1.0_real64,  &
                3.0_real64, 1.0_real64,  &
                4.0_real64, 1.0_real64,  &
                5.0_real64, 1.0_real64,  &
                1.0_real64, 2.0_real64,  &
                2.0_real64, 2.0_real64,  &
                3.0_real64, 2.0_real64,  &
                4.0_real64, 2.0_real64,  &
                5.0_real64, 2.0_real64], [2, 10])
            type(NdNodePtr), allocatable :: res(:)
            real(real64), allocatable  :: point(:)
            real(real64)               :: dist

            call t%build(coords)
            res = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            allocate(point, source=[4.0_real64, 2.0_real64])

            dist = res(1)%p%chebyshevDistPoint(point)

            if (dist .ne. 3.0_real64) then
                write(*, '(A)') '--- Testv020_POINT_QUERY_CHEBYSHEV ---'
                write(*,*) 'expected 3.0, got:', dist
                stop 1
            end if
        end subroutine pointQuery_Chebyshev

end program Testv020_POINT_QUERY_CHEBYSHEV
