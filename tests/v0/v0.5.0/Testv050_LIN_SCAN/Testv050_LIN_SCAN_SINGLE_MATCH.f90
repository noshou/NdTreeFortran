program Testv050_LIN_SCAN_SINGLE_MATCH
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanSingleMatch()
    contains
        !> linScan with the id of one node returns exactly that node.
        subroutine linScanSingleMatch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(KdNodePtr), allocatable :: allNodes(:), res(:)
            type(NodeId)                 :: target(1), tmpId

            call t%build(coords)
            allNodes  = t%getAllNodes()
            target(1) = allNodes(1)%p%getNodeId()
            res       = t%linScan(target)

            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_SINGLE_MATCH ---'
                write(*, '(A,I0)') 'expected 1 match, got: ', size(res)
                stop 1
            end if
            tmpId = res(1)%p%getNodeId()
            if (tmpId%node_id .ne. target(1)%node_id) then
                write(*, '(A)') '--- Testv050_LIN_SCAN_SINGLE_MATCH ---'
                write(*, '(A)') 'returned node has wrong id'
                stop 1
            end if
        end subroutine linScanSingleMatch
end program Testv050_LIN_SCAN_SINGLE_MATCH
