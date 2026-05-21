program Testv071_FIND_NODES_IDS_SINGLE_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesIdsSingleHit()
    contains
        !> 1-node tree; query at same coords with id=1 must return exactly one node.
        !! A single-node tree assigns nodeId=1 (currNodeId starts at 0, increments once).
        subroutine findNodesIdsSingleHit()
            type(BallTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64)   :: coords(2, 1) = reshape([3.0_real64, 4.0_real64], [2, 1])
            type(NodeId)   :: ids(1)

            call t%build(coords)
            ids(1)%node_id = 1
            res = t%rNN_Ids(coords, ids, epsilon=1e-10_real64)

            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv071_FIND_NODES_IDS_SINGLE_HIT ---'
                write(*, '(A,I0)') 'expected res size 1, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv071_FIND_NODES_IDS_SINGLE_HIT ---'
                write(*, '(A,I0)') 'expected 1 node in bucket, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesIdsSingleHit
end program Testv071_FIND_NODES_IDS_SINGLE_HIT
