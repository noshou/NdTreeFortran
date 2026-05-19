program Testv040_FIND_NODES_IDS_SIZE_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesIdsSizeMismatch()
    contains
        !> rNN_Ids where size(ids) != size(coords,2) must stop 1.
        subroutine findNodesIdsSizeMismatch()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NodeId)   :: ids(2)

            call t%build(coords)
            res = t%rNN_Ids(query, ids)
            write(*, '(A)') '--- Testv040_FIND_NODES_IDS_SIZE_MISMATCH ---'
            write(*, '(A)') 'expected stop 1, but rNN_Ids returned normally'
        end subroutine findNodesIdsSizeMismatch
end program Testv040_FIND_NODES_IDS_SIZE_MISMATCH
