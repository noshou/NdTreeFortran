program Testv050_RMV_NODES_IDS_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesIdsEmpty()
    contains
        !> Zero-size ids array must error stop.
        subroutine rmvNodesIdsEmpty()
            type(KdTree)               :: t
            real(real64)               :: init(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NodeId),   allocatable :: bad_ids(:)
            integer                    :: numRmv
            allocate(bad_ids(0))
            call t%build(init)
            numRmv = t%rmvNodes(ids=bad_ids)
            write(*, '(A)') '--- Testv050_RMV_NODES_IDS_EMPTY ---'
            write(*, '(A)') 'expected error stop for empty ids'
        end subroutine rmvNodesIdsEmpty
end program Testv050_RMV_NODES_IDS_EMPTY
