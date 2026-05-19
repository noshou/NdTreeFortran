!> Expected-fail: rNN_Node (default metric) with a node from a different Tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_SINGLE_NODE_QUERY_RNN_NODE_DEFAULT_NON_MEMBER

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call singleNodeQuery_rNN_Node_Default_NonMember()
    contains

        subroutine singleNodeQuery_rNN_Node_Default_NonMember()
            type(KdTree)                 :: t1, t2
            real(real64)               :: coords(2, 4) = reshape( &
                [1.0_real64, 0.0_real64,  &
                0.6_real64, 0.8_real64,  &
                0.9_real64, 0.9_real64,  &
                1.9_real64, 0.9_real64], [2, 4])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t1%build(coords)
            call t2%build(coords)

            centroid_res = t2%rNN_Centroid([1.0_real64, 0.0_real64], 0.01_real64)

            ! target belongs to t2 -> must error stop
            res = t1%rNN_Node(centroid_res(1), 1.0_real64)
            write(*, '(A)') '--- Testv020_SINGLE_NODE_QUERY_RNN_NODE_DEFAULT_NON_MEMBER ---'
            write(*, '(A)') 'expected error stop, but rNN_Node returned normally'
        end subroutine singleNodeQuery_rNN_Node_Default_NonMember

end program Testv020_SINGLE_NODE_QUERY_RNN_NODE_DEFAULT_NON_MEMBER
