program Testv050_LIN_SCAN_MULTI_MATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanMultiMatch()
    contains
        !> linScan with 3 ids from a 5-node tree returns exactly 3 nodes,
        !! all with ids matching the query set.
        subroutine linScanMultiMatch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: allNodes(:), res(:)
            type(NodeId)                 :: targets(3), tmpId
            integer                      :: i
            logical                      :: found

            call t%build(coords)
            allNodes   = t%getAllNodes()
            targets(1) = allNodes(1)%p%getNodeId()
            targets(2) = allNodes(3)%p%getNodeId()
            targets(3) = allNodes(5)%p%getNodeId()

            res = t%linScan(targets)

            if (size(res) .ne. 3) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_MULTI_MATCH ---'
                write(*, '(A,I0)') 'expected 3 matches, got: ', size(res)
                stop 1
            end if

            do i = 1, size(res)
                tmpId = res(i)%p%getNodeId()
                found = (tmpId%node_id == targets(1)%node_id .or. &
                         tmpId%node_id == targets(2)%node_id .or. &
                         tmpId%node_id == targets(3)%node_id)
                if (.not. found) then
                    write(*, '(A)')    '--- Testv050_LIN_SCAN_MULTI_MATCH ---'
                    write(*, '(A,I0)') 'result has unexpected id: ', tmpId%node_id
                    stop 1
                end if
            end do
        end subroutine linScanMultiMatch
end program Testv050_LIN_SCAN_MULTI_MATCH
