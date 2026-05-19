!> Expected-fail: rNN_Node with a negative radius must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_RNN_NODE_NEGATIVE_RADIUS

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call rnnNode_NegativeRadius()
    contains

        subroutine rnnNode_NegativeRadius()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t%build(coords)
            centroid_res = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)

            res = t%rNN_Node(centroid_res(1), -1.0_real64)
            write(*, '(A)') '--- Testv020_RNN_NODE_NEGATIVE_RADIUS ---'
            write(*,*) 'expected error stop, but rNN_Node returned normally'
        end subroutine rnnNode_NegativeRadius

end program Testv020_RNN_NODE_NEGATIVE_RADIUS
