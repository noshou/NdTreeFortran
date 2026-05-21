!> Expected-fail: BallTree rNN_Node on an empty tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv071_RNN_NODE_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        !> rNN_Node on an empty tree must error stop.
        subroutine run()
            type(BallTree)               :: t, tHelper
            real(real64)               :: coords(2, 0)
            real(real64)               :: helperCoords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: r = 0.9
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)

            call t%build(coords)
            call tHelper%build(helperCoords)

            centroid_res = tHelper%rNN_Centroid([0.0_real64, 0.0_real64], 1000.0_real64)

            res = t%rNN_Node(centroid_res(1), r) ! expected to fail here (empty tree check fires first)
            write(*, '(A)') '--- Testv071_RNN_NODE_EMPTY_TREE ---'
            write(*, '(A)') 'expected program to fail, but ran successfully!'
        end subroutine run

end program Testv071_RNN_NODE_EMPTY_TREE
