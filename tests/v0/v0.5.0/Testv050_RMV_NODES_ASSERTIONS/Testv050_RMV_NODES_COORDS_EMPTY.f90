program Testv050_RMV_NODES_COORDS_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesCoordsEmpty()
    contains
        !> Zero-column coordsList must error stop.
        subroutine rmvNodesCoordsEmpty()
            type(KdTree) :: t
            real(real64) :: init(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64), allocatable :: bad_coords(:,:)
            integer :: numRmv
            allocate(bad_coords(2, 0))
            call t%build(init)
            numRmv = t%rmvNodes(coordsList=bad_coords)
            write(*, '(A)') '--- Testv050_RMV_NODES_COORDS_EMPTY ---'
            write(*, '(A)') 'expected error stop for empty coordsList'
        end subroutine rmvNodesCoordsEmpty
end program Testv050_RMV_NODES_COORDS_EMPTY
