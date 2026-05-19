program Testv050_LIN_SCAN_UNINITIALIZED
    use NdTreeFortran
    implicit none
    call linScanUninitialized()
    contains
        !> linScan on an uninitialized tree must error stop.
        subroutine linScanUninitialized()
            type(KdTree)                 :: t
            type(NodeId)                 :: ids(1)
            type(NdNodePtr), allocatable :: res(:)
            res = t%linScan(ids)
        end subroutine linScanUninitialized
end program Testv050_LIN_SCAN_UNINITIALIZED
