program Testv040_FIND_NODES_IDS_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesIdsUninitialized()
    contains
        !> rNN_Ids on a tree that has never been built must stop 1.
        subroutine findNodesIdsUninitialized()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NodeId)   :: ids(1)

            res = t%rNN_Ids(query, ids)
            write(*, '(A)') '--- Testv040_FIND_NODES_IDS_UNINITIALIZED ---'
            write(*, '(A)') 'expected stop 1, but rNN_Ids returned normally'
        end subroutine findNodesIdsUninitialized
end program Testv040_FIND_NODES_IDS_UNINITIALIZED
