program Testv071_RMV_NODES_REBUILD_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesRebuildRnnEuclidean()
    contains
        !> After rmvNodes removes the center node from a 3x3 grid, rNN_Centroid
        !! with euclidean metric finds only the 4 axis-adjacent survivors (r=1.1).
        subroutine rmvNodesRebuildRnnEuclidean()
            type(BallTree)               :: t
            real(real64)                 :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)                 :: rmvQuery(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_REBUILD_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            res = t%rNN_Centroid([1.0_real64, 1.0_real64], 1.1_real64, metric='euclidean')
            if (size(res) .ne. 4) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_REBUILD_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 4 axis-adjacent nodes after removal, got: ', size(res)
                stop 1
            end if
        end subroutine rmvNodesRebuildRnnEuclidean
end program Testv071_RMV_NODES_REBUILD_RNN_EUCLIDEAN
