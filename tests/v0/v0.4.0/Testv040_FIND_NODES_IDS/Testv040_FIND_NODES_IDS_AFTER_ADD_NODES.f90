program Testv040_FIND_NODES_IDS_AFTER_ADD_NODES
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesIdsAfterAdd()
    contains
        !> rNN_Ids must find a node inserted via addNodes when given its id.
        !! Build a 1-node tree at (0,0); addNodes inserts (5,5).
        !! Query at (5,5) with the added node's id -> 1 result.
        !! Query at (5,5) with the original node's id -> 0 results.
        subroutine findNodesIdsAfterAdd()
            type(KdTree) :: t
            type(KdNodeBucket), allocatable :: res(:)
            type(KdNodePtr),    allocatable :: pool(:)
            real(real64) :: init_coord(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64) :: new_coord(2, 1)  = reshape([5.0_real64, 5.0_real64], [2, 1])
            type(NodeId)             :: id_new(1), id_orig(1)
            real(real64), allocatable :: tmp_coords(:)
            integer                  :: i

            call t%build(init_coord)
            call t%addNodes(new_coord)

            pool = t%getAllNodes()
            do i = 1, size(pool)
                tmp_coords = pool(i)%p%getCoords()
                if (tmp_coords(1) .gt. 1.0_real64) then
                    id_new(1)  = pool(i)%p%getNodeId()
                else
                    id_orig(1) = pool(i)%p%getNodeId()
                end if
            end do

            res = t%rNN_Ids(new_coord, id_new, epsilon=0.5_real64)
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0)') 'expected 1 node for correct id, got: ', size(res(1)%nodes)
                stop 1
            end if

            res = t%rNN_Ids(new_coord, id_orig, epsilon=0.5_real64)
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_IDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0)') 'expected 0 nodes for wrong id at added coord, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine findNodesIdsAfterAdd
end program Testv040_FIND_NODES_IDS_AFTER_ADD_NODES
