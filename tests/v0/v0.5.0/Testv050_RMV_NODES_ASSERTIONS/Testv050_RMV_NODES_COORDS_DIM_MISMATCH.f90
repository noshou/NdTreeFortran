program Testv050_RMV_NODES_COORDS_DIM_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesCoordsDimMismatch()
    contains
        !> coordsList with wrong dimension must error stop.
        subroutine rmvNodesCoordsDimMismatch()
            type(KdTree) :: t
            real(real64) :: init(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: bad(3, 1) = reshape([0.0_real64, 0.0_real64, 0.0_real64], [3, 1])
            integer      :: numRmv
            call t%build(init)
            numRmv = t%rmvNodes(coordsList=bad)
            write(*, '(A)') '--- Testv050_RMV_NODES_COORDS_DIM_MISMATCH ---'
            write(*, '(A)') 'expected error stop for dimension mismatch'
        end subroutine rmvNodesCoordsDimMismatch
end program Testv050_RMV_NODES_COORDS_DIM_MISMATCH
