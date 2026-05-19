program Testv040_FIND_NODES_COORDS_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesCoordsUninitialized()
    contains
        !> rNN_Coords on a tree that has never been built must stop 1.
        subroutine findNodesCoordsUninitialized()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])

            res = t%rNN_Coords(query)
            write(*, '(A)') '--- Testv040_FIND_NODES_COORDS_UNINITIALIZED ---'
            write(*, '(A)') 'expected stop 1, but rNN_Coords returned normally'
        end subroutine findNodesCoordsUninitialized
end program Testv040_FIND_NODES_COORDS_UNINITIALIZED
