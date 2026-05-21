program Testv071_RMV_NODES_REBUILD_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesRebuildRnnManhattan()
    contains
        !> After rmvNodes removes 2 corner nodes, rNN with manhattan metric
        !! finds exactly 1 survivor within radius 0.5 of (0,0).
        subroutine rmvNodesRebuildRnnManhattan()
            type(BallTree)               :: t
            real(real64)                 :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            real(real64)                 :: rmvQuery(2, 2) = reshape( &
                [2.0_real64, 0.0_real64, 2.0_real64, 2.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:)
            integer                      :: numRmv

            call t%setMetric('manhattan')
            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 2) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_REBUILD_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected numRmv=2, got: ', numRmv
                stop 1
            end if

            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.5_real64, metric='manhattan')
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_REBUILD_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 1 node at origin with manhattan r=0.5, got: ', size(res)
                stop 1
            end if
        end subroutine rmvNodesRebuildRnnManhattan
end program Testv071_RMV_NODES_REBUILD_RNN_MANHATTAN
