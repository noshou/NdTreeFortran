program Testv030_ADD_NODES_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesUninitialized()
    contains
        !> addNodes on an uninitialized tree must error stop.
        subroutine addNodesUninitialized()
            type(KdTree)   :: t
            real(real64) :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
            call t%addNodes(coords)
            write(*, '(A)') '--- Testv030_ADD_NODES_UNINITIALIZED ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesUninitialized
end program Testv030_ADD_NODES_UNINITIALIZED
