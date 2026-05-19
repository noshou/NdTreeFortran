program Testv050_RMV_NODES_IDS_MULTI_IDS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesIdsMultiIds()
    contains
        !> ids-only branch: find nodeIds of 2 nodes via rNN_Centroid, then
        !! rmvNodes with both ids removes exactly 2 nodes; numRmv=2, pop=1.
        subroutine rmvNodesIdsMultiIds()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res1(:), res2(:)
            type(NodeId)                 :: targetIds(2)
            integer                      :: numRmv
            integer(int64)               :: pop

            call t%build(coords)
            res1 = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            res2 = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
            if (size(res1) .ne. 1 .or. size(res2) .ne. 1) then
                write(*, '(A)') '--- Testv050_RMV_NODES_IDS_MULTI_IDS ---'
                write(*, '(A)') 'expected 1 node at each query point'
                stop 1
            end if
            targetIds(1) = res1(1)%p%getNodeId()
            targetIds(2) = res2(1)%p%getNodeId()

            numRmv = t%rmvNodes(ids=targetIds)
            pop    = t%getPop()

            if (numRmv .ne. 2) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IDS_MULTI_IDS ---'
                write(*, '(A,I0)') 'expected numRmv=2, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 1_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IDS_MULTI_IDS ---'
                write(*, '(A,I0)') 'expected pop=1, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesIdsMultiIds
end program Testv050_RMV_NODES_IDS_MULTI_IDS
