program Testv050_RMV_NODES_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesUninitialized()
    contains
        !> rmvNodes on an uninitialized tree must error stop.
        subroutine rmvNodesUninitialized()
            type(KdTree) :: t
            real(real64) :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
            integer      :: numRmv
            numRmv = t%rmvNodes(coordsList=coords)
            write(*, '(A)') '--- Testv050_RMV_NODES_UNINITIALIZED ---'
            write(*, '(A)') 'expected error stop before build, but returned normally'
        end subroutine rmvNodesUninitialized
end program Testv050_RMV_NODES_UNINITIALIZED
