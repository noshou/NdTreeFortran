program Testv060_DBSCAN_SINGLE_NODE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanSingleNode()
    contains
        subroutine dbscanSingleNode()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 1) = reshape( &
                [0.0_real64, 0.0_real64], [2, 1])
            type(NdNodeBucket), allocatable :: res(:)

            call t%build(coords)
            res = t%DBSCAN(minPts=2_int64, radius=1.0_real64)

            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_SINGLE_NODE ---'
                write(*, '(A,I0)') 'expected size(res)==1 (noise bucket only), got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv060_DBSCAN_SINGLE_NODE ---'
                write(*, '(A,I0)') 'expected noise bucket size==1, got: ', size(res(1)%nodes)
                stop 1
            end if
        end subroutine dbscanSingleNode
end program Testv060_DBSCAN_SINGLE_NODE
