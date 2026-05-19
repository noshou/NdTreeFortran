program Testv050_LIN_SCAN_EMPTY_IDS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call linScanEmptyIds()
    contains
        !> linScan with an empty ids array returns size=0 without error.
        subroutine linScanEmptyIds()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NodeId)                 :: ids(0)
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%linScan(ids)

            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv050_LIN_SCAN_EMPTY_IDS ---'
                write(*, '(A,I0)') 'expected size=0 for empty ids, got: ', size(res)
                stop 1
            end if
        end subroutine linScanEmptyIds
end program Testv050_LIN_SCAN_EMPTY_IDS
