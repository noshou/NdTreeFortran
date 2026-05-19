program Testv050_RMV_NODES_REBUILD_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesRebuildRnnChebyshev()
    contains
        !> After rmvNodes removes 3 nodes, rNN with chebyshev metric
        !! finds exactly 2 survivors within the chebyshev ball.
        subroutine rmvNodesRebuildRnnChebyshev()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 5])
            real(real64)                 :: rmvQuery(2, 3) = reshape( &
                [2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 3) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REBUILD_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected numRmv=3, got: ', numRmv
                stop 1
            end if

            ! chebyshev r=1.5 from (0,0): nodes within max(|dx|,|dy|)<=1.5 -> (0,0) and (1,0)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='chebyshev')
            if (size(res) .ne. 2) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REBUILD_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 2 nodes within chebyshev r=1.5, got: ', size(res)
                stop 1
            end if
        end subroutine rmvNodesRebuildRnnChebyshev
end program Testv050_RMV_NODES_REBUILD_RNN_CHEBYSHEV
