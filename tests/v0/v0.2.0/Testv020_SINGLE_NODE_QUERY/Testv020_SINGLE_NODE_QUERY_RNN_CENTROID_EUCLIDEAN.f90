!> rNN_Centroid with metric='euclidean'.
!! Geometry: 4 points in 2D, centroid at origin, radius 1.0.
!! Expected: 2 nodes -> (1,0) and (0.6,0.8) both lie on the unit Euclidean sphere.
program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_EUCLIDEAN

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Centroid_Euclidean()
    contains

        subroutine singleNodeQuery_rNN_Centroid_Euclidean()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 4) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.6_real64, 0.8_real64,  &
                0.9_real64, 0.9_real64,  &
                1.9_real64, 0.9_real64], [2, 4])
            real(real64)               :: centroid(2) = [0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid(centroid, 1.0_real64, metric='euclidean')

            if (size(res) .ne. 2) then
                write(*, '(A)') '--- Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_EUCLIDEAN ---'
                write(*,*) 'expected 2 nodes, got:', size(res)
                stop 1
            end if
        end subroutine singleNodeQuery_rNN_Centroid_Euclidean

end program Testv020_SINGLE_NODE_QUERY_RNN_CENTROID_EUCLIDEAN
