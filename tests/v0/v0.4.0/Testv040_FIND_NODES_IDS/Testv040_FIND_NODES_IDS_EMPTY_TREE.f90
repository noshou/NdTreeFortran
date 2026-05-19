program Testv040_FIND_NODES_IDS_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesIdsEmptyTree()
    contains
        !> rNN_Ids on a zero-node tree must return empty buckets.
        !! Tree state (pop=0, initState=true) must be unchanged after the call.
        subroutine findNodesIdsEmptyTree()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            logical :: isInit
            integer(int64) :: pop
            real(real64)   :: empty_coords(2, 0)
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NodeId)   :: ids(1)

            call t%build(empty_coords)
            res = t%rNN_Ids(query, ids, epsilon=1.0_real64)

            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected empty bucket, got: ', size(res(1)%nodes)
                stop 1
            end if
            call t%getInitState(isInit)
            pop = t%getPop()
            if (.not. isInit .or. pop .ne. 0_int64) then
                write(*, '(A)')       '--- Testv040_FIND_NODES_IDS_EMPTY_TREE ---'
                write(*, '(A,L2,I0)') 'expected initState=T pop=0, got: ', isInit, pop
                stop 1
            end if
        end subroutine findNodesIdsEmptyTree
end program Testv040_FIND_NODES_IDS_EMPTY_TREE
