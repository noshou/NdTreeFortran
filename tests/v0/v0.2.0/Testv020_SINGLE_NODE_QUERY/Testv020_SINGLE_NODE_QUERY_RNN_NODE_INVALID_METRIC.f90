!> Expected-fail: rNN_Node with an unrecognised metric must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_SINGLE_NODE_QUERY_RNN_NODE_INVALID_METRIC

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Node_InvalidMetric()
    contains

        subroutine singleNodeQuery_rNN_Node_InvalidMetric()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 2) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.0_real64, 1.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t%build(coords)

            centroid_res = t%rNN_Centroid([1.0_real64, 0.0_real64], 0.01_real64)

            res = t%rNN_Node(centroid_res(1), 1.0_real64, metric='invalid')
            write(*, '(A)') '--- Testv020_SINGLE_NODE_QUERY_RNN_NODE_INVALID_METRIC ---'
            write(*,*) 'expected program to fail!'

        end subroutine singleNodeQuery_rNN_Node_InvalidMetric

end program Testv020_SINGLE_NODE_QUERY_RNN_NODE_INVALID_METRIC
