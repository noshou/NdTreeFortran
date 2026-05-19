program Testv050_RMV_NODES_COORDS_IDS_SIZE_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesCoordsIdsSizeMismatch()
    contains
        !> coordsList with 2 columns and ids with 3 elements (no radii) must error stop.
        subroutine rmvNodesCoordsIdsSizeMismatch()
            type(KdTree)   :: t
            real(real64)   :: init(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 2.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: query(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            type(NodeId)   :: ids(3)
            integer        :: numRmv
            call t%build(init)
            numRmv = t%rmvNodes(coordsList=query, ids=ids)
            write(*, '(A)') '--- Testv050_RMV_NODES_COORDS_IDS_SIZE_MISMATCH ---'
            write(*, '(A)') 'expected error stop for coords/ids size mismatch'
        end subroutine rmvNodesCoordsIdsSizeMismatch
end program Testv050_RMV_NODES_COORDS_IDS_SIZE_MISMATCH
