!> BallTree rNN_Node with metric='chebyshev'.
!! Geometry: 4 points in 2D, target P1=(1,0), radius 1.0.
!! Chebyshev distances from P1: P2=0.8, P3=0.9, P4=0.9.
!! Expected: 4 nodes (all points).
program Testv071_RNN_NODE_CHEBYSHEV

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        subroutine run()
            type(BallTree)               :: t
            real(real64)               :: coords(2, 4) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.6_real64, 0.8_real64,  &
                0.9_real64, 0.9_real64,  &
                1.9_real64, 0.9_real64], [2, 4])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t%setMetric('chebyshev')
            call t%build(coords)

            centroid_res = t%rNN_Centroid([1.0_real64, 0.0_real64], 0.01_real64)

            res = t%rNN_Node(centroid_res(1), 1.0_real64, metric='chebyshev')

            if (size(res) .ne. 4) then
                write(*, '(A)') '--- Testv071_RNN_NODE_CHEBYSHEV ---'
                write(*,*) 'expected 4 nodes, got:', size(res)
                stop 1
            end if
        end subroutine run

end program Testv071_RNN_NODE_CHEBYSHEV
