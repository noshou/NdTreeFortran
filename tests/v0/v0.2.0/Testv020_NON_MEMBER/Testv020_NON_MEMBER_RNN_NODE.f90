!> Expected-fail: rNN_Node with a node from a different Tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NON_MEMBER_RNN_NODE

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nonMemberRnnNode()
    contains

        subroutine nonMemberRnnNode()
            type(KdTree)                 :: t1, t2
            real(real64)               :: coords(2, 2) = reshape([1.0_real64, 2.0_real64, -2.0_real64, -32.3_real64], [2, 2])
            real(real64)               :: centroid(2)  = [0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t1%build(coords)
            call t2%build(coords)

            centroid_res = t2%rNN_Centroid(centroid, 1000.0_real64)

            ! node2 belongs to t2 -> must error stop
            res = t1%rNN_Node(centroid_res(1), 1000.0_real64)
            write(*, '(A)') '--- Testv020_NON_MEMBER_RNN_NODE ---'
            write(*, '(A)') 'expected error stop, but rNN_Node returned normally'

        end subroutine nonMemberRnnNode

end program Testv020_NON_MEMBER_RNN_NODE
