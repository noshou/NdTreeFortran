program Testv050_RMV_NODES_RADII_SIZE_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesRadiiSizeMismatch()
    contains
        !> 2 radii but only 1 coord column must error stop.
        subroutine rmvNodesRadiiSizeMismatch()
            type(KdTree) :: t
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64) :: radii(2)    = [1.0_real64, 1.0_real64]
            integer      :: numRmv
            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            write(*, '(A)') '--- Testv050_RMV_NODES_RADII_SIZE_MISMATCH ---'
            write(*, '(A)') 'expected error stop for radii/coords size mismatch'
        end subroutine rmvNodesRadiiSizeMismatch
end program Testv050_RMV_NODES_RADII_SIZE_MISMATCH
