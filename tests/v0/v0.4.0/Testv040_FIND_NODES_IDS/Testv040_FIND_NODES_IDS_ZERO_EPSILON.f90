program Testv040_FIND_NODES_IDS_ZERO_EPSILON
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesIdsZeroEpsilon()
    contains
        !> epsilon=0.0 on an exact coordinate match with id=1 must return one node.
        !! Neighbour at distance 1.0 must not appear even if it happened to have id=1.
        subroutine findNodesIdsZeroEpsilon()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64)   :: coords(2, 1) = reshape([3.0_real64, 4.0_real64], [2, 1])
            type(NodeId)   :: ids(1)

            call t%build(coords)
            ids(1)%node_id = 1
            res = t%rNN_Ids(coords, ids, epsilon=0.0_real64)

            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_ZERO_EPSILON ---'
                write(*, '(A,I0)') 'expected 1 node at zero epsilon, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesIdsZeroEpsilon
end program Testv040_FIND_NODES_IDS_ZERO_EPSILON
