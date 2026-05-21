program Testv071_RMV_NODES_IDS_SINGLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesIdsSingleId()
    contains
        !> ids-only branch: find nodeId of one node via rNN_Centroid, then
        !! rmvNodes with that id removes exactly that node; numRmv=1, pop=2.
        subroutine rmvNodesIdsSingleId()
            type(BallTree)               :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            type(NodeId)                 :: targetId(1)
            integer                      :: numRmv
            integer(int64)               :: pop

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_IDS_SINGLE ---'
                write(*, '(A,I0)') 'expected 1 node at origin, got: ', size(res)
                stop 1
            end if
            targetId(1) = res(1)%p%getNodeId()

            numRmv = t%rmvNodes(ids=targetId)
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_IDS_SINGLE ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 2_int64) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_IDS_SINGLE ---'
                write(*, '(A,I0)') 'expected pop=2, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesIdsSingleId
end program Testv071_RMV_NODES_IDS_SINGLE
