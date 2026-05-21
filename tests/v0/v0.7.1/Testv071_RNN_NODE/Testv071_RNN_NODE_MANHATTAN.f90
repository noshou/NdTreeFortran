!> BallTree rNN_Node with metric='manhattan'.
!! Geometry: 4 points in 2D, target P1=(1,0), radius 1.0.
!! Manhattan distances from P1: P2=1.2, P3=1.0, P4=1.8.
!! Expected: 2 nodes (P1 itself and P3).
program Testv071_RNN_NODE_MANHATTAN

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

            call t%setMetric('manhattan')
            call t%build(coords)

            centroid_res = t%rNN_Centroid([1.0_real64, 0.0_real64], 0.01_real64)

            res = t%rNN_Node(centroid_res(1), 1.0_real64, metric='manhattan')

            if (size(res) .ne. 2) then
                write(*, '(A)') '--- Testv071_RNN_NODE_MANHATTAN ---'
                write(*,*) 'expected 2 nodes, got:', size(res)
                stop 1
            end if
        end subroutine run

end program Testv071_RNN_NODE_MANHATTAN
