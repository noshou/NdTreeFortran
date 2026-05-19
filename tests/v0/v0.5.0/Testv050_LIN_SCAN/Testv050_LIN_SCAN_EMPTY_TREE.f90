program Testv050_LIN_SCAN_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call linScanEmptyTree()
    contains
        !> linScan on a tree drained to pop=0 returns size=0 without error.
        subroutine linScanEmptyTree()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                 :: query(2, 1)  = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                 :: radii(1)     = [100.0_real64]
            type(NodeId)                 :: ids(1)
            type(NdNodePtr), allocatable :: res(:)
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            res    = t%linScan(ids)

            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected size=0 on empty tree, got: ', size(res)
                stop 1
            end if
        end subroutine linScanEmptyTree
end program Testv050_LIN_SCAN_EMPTY_TREE
