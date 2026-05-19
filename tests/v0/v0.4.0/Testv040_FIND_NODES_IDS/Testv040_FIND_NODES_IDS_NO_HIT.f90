program Testv040_FIND_NODES_IDS_NO_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesIdsNoHit()
    contains
        !> Query with id=0 must return an empty bucket; no node ever has nodeId=0.
        subroutine findNodesIdsNoHit()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NodeId)   :: ids(1)

            call t%build(coords)
            res = t%rNN_Ids(query, ids, epsilon=0.5_real64)

            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_NO_HIT ---'
                write(*, '(A,I0)') 'expected empty bucket for id=0, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesIdsNoHit
end program Testv040_FIND_NODES_IDS_NO_HIT
