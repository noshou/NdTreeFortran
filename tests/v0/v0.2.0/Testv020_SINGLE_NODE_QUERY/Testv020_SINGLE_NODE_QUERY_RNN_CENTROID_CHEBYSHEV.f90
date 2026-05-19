!> rNN_Centroid with metric='chebyshev'.
!! Geometry: 4 points in 2D, centroid at origin, radius 1.0.
!! Expected: 3 nodes -> (1,0) L∞=1, (0.6,0.8) L∞=0.8, (0.9,0.9) L∞=0.9;
!! (1.9,0.9) has L∞=1.9 and is excluded.
program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_CHEBYSHEV

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Centroid_Chebyshev()
    contains

        subroutine singleNodeQuery_rNN_Centroid_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 4) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.6_real64, 0.8_real64,  &
                0.9_real64, 0.9_real64,  &
                1.9_real64, 0.9_real64], [2, 4])
            real(real64)               :: centroid(2) = [0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid(centroid, 1.0_real64, metric='chebyshev')

            if (size(res) .ne. 3) then
                write(*, '(A)') '--- Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_CHEBYSHEV ---'
                write(*,*) 'expected 3 nodes, got:', size(res)
                stop 1
            end if
        end subroutine singleNodeQuery_rNN_Centroid_Chebyshev

end program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_CHEBYSHEV
