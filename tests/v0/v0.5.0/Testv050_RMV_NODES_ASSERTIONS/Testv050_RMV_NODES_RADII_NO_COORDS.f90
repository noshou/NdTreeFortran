program Testv050_RMV_NODES_RADII_NO_COORDS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesRadiiNoCoords()
    contains
        !> radii without coordsList must error stop.
        subroutine rmvNodesRadiiNoCoords()
            type(KdTree) :: t
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: radii(1) = [1.0_real64]
            integer      :: numRmv
            call t%build(coords)
            numRmv = t%rmvNodes(radii=radii)
            write(*, '(A)') '--- Testv050_RMV_NODES_RADII_NO_COORDS ---'
            write(*, '(A)') 'expected error stop for radii without coordsList'
        end subroutine rmvNodesRadiiNoCoords
end program Testv050_RMV_NODES_RADII_NO_COORDS
