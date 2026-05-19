!> Expected-fail: rNN_Node with a null target pointer must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_RNN_NODE_NULL_TARGET

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call rnnNode_NullTarget()
    contains

        subroutine rnnNode_NullTarget()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                2.0_real64, 1.0_real64,  &
                3.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNodePtr)              :: target

            call t%build(coords)

            res = t%rNN_Node(target, 1.0_real64)
            write(*, '(A)') '--- Testv020_RNN_NODE_NULL_TARGET ---'
            write(*,*) 'expected error stop, but rNN_Node returned normally'
        end subroutine rnnNode_NullTarget

end program Testv020_RNN_NODE_NULL_TARGET
