program Testv050_LIN_SCAN_LIFECYCLE
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanLifecycle()
    contains
        !> Build → linScan (finds nodes) → rmvNodes → linScan (old id gone) →
        !! addNodes → linScan (new ids found).
        subroutine linScanLifecycle()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                 :: extra(2, 2)  = reshape( &
                [5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 2])
            real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(KdNodePtr), allocatable :: allNodes(:), res(:)
            type(NodeId)                 :: removedId(1), newIds(2)
            integer                      :: numRmv, i

            call t%build(coords)

            allNodes      = t%getAllNodes()
            removedId(1)  = allNodes(1)%p%getNodeId()

            res = t%linScan(removedId)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_LIFECYCLE (before rmv) ---'
                write(*, '(A,I0)') 'expected 1 match, got: ', size(res)
                stop 1
            end if

            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_LIFECYCLE (rmvNodes) ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            res = t%linScan(removedId)
            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_LIFECYCLE (after rmv) ---'
                write(*, '(A,I0)') 'expected size=0 after removal, got: ', size(res)
                stop 1
            end if

            call t%addNodes(extra)
            allNodes = t%getAllNodes()
            do i = 1, 2
                newIds(i) = allNodes(size(allNodes) - 2 + i)%p%getNodeId()
            end do

            res = t%linScan(newIds)
            if (size(res) .ne. 2) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_LIFECYCLE (after add) ---'
                write(*, '(A,I0)') 'expected 2 matches for new ids, got: ', size(res)
                stop 1
            end if
        end subroutine linScanLifecycle
end program Testv050_LIN_SCAN_LIFECYCLE
