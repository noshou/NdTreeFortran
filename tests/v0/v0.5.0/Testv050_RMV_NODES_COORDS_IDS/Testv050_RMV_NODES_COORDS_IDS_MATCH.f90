program Testv050_RMV_NODES_COORDS_IDS_MATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordsIdsMatch()
    contains
        !> coordsList+ids (no radii): coord and id both match -> node is removed.
        !! Find nodeId of (0,0) via rNN_Centroid, then rmvNodes with that coord+id.
        subroutine rmvNodesCoordsIdsMatch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            real(real64)                 :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NodeId)                 :: targetIds(1)
            integer                      :: numRmv
            integer(int64)               :: pop

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)') '--- Testv050_RMV_NODES_COORDS_IDS_MATCH ---'
                write(*, '(A)') 'expected 1 node at origin'
                stop 1
            end if
            targetIds(1) = res(1)%p%getNodeId()

            numRmv = t%rmvNodes(coordsList=query, ids=targetIds)
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_IDS_MATCH ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 2_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_IDS_MATCH ---'
                write(*, '(A,I0)') 'expected pop=2, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordsIdsMatch
end program Testv050_RMV_NODES_COORDS_IDS_MATCH
