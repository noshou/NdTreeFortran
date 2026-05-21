program Testv060_DBSCAN_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanEmptyTree()
    contains
        subroutine dbscanEmptyTree()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=coords)
            res = t%DBSCAN(minPts=2_int64, radius=1.0_real64)

            if (size(res) .ne. 0) then
                write(*, '(A)')    '--- Testv060_DBSCAN_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected size(res)==0 for empty tree, got: ', size(res)
                stop 1
            end if
        end subroutine dbscanEmptyTree
end program Testv060_DBSCAN_EMPTY_TREE
