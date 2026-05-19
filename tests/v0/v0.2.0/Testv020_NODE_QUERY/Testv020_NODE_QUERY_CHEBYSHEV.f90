!> node%chebyshevDist between two nodes in a 10-point 2D grid tree.
!! Node at (1,1) to node at (4,2): expected L∞ distance = 3.0.
program Testv020_NODE_QUERY_CHEBYSHEV

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nodeQuery_Chebyshev()
    contains

        subroutine nodeQuery_Chebyshev()
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
            type(NdNodePtr), allocatable :: res1(:), res2(:)
            real(real64)               :: dist

            call t%build(coords)
            res1 = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            res2 = t%rNN_Centroid([4.0_real64, 2.0_real64], 0.01_real64)

            dist = res1(1)%p%chebyshevDist(res2(1)%p)

            if (dist .ne. 3.0_real64) then
                write(*, '(A)') '--- Testv020_NODE_QUERY_CHEBYSHEV ---'
                write(*,*) 'expected 3.0, got:', dist
                stop 1
            end if
        end subroutine nodeQuery_Chebyshev

end program Testv020_NODE_QUERY_CHEBYSHEV
