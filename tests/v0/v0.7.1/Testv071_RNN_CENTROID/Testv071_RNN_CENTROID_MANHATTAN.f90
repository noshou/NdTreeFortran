!> BallTree rNN_Centroid with metric='manhattan'.
!! Geometry: 4 points in 2D, centroid at origin, radius 1.0.
!! Expected: 1 node -> only (1,0) has Manhattan distance exactly 1;
!! (0.6,0.8) has L1=1.4, (0.9,0.9) has L1=1.8, (1.9,0.9) has L1=2.8.
program Testv071_RNN_CENTROID_MANHATTAN

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
            real(real64)               :: centroid(2) = [0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)

            call t%setMetric('manhattan')
            call t%build(coords)
            res = t%rNN_Centroid(centroid, 1.0_real64, metric='manhattan')

            if (size(res) .ne. 1) then
                write(*, '(A)') '--- Testv071_RNN_CENTROID_MANHATTAN ---'
                write(*,*) 'expected 1 node, got:', size(res)
                stop 1
            end if
        end subroutine run

end program Testv071_RNN_CENTROID_MANHATTAN
