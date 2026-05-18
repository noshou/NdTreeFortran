program Testv050_RMV_NODES_RAD_IDS_MATCH
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRadIdsMatch()
    contains
        !> coordsList+radii+ids: radius captures multiple nodes but ids filter
        !! restricts removal to only those whose id is in the ids set.
        !! Build 9-node 3x3 grid, r=1.1 from (1,1) captures 5 nodes.
        !! ids set contains only nodeId of (1,1); numRmv=1, pop=8.
        subroutine rmvNodesRadIdsMatch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            type(KdNodePtr), allocatable :: res(:)
            real(real64)                 :: query(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])
            real(real64)                 :: radii(1)    = [1.1_real64]
            type(NodeId)                 :: centerIds(1)
            integer                      :: numRmv
            integer(int64)               :: pop

            call t%build(coords)
            res = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)') '--- Testv050_RMV_NODES_RAD_IDS_MATCH ---'
                write(*, '(A)') 'expected 1 node at center (1,1)'
                stop 1
            end if
            centerIds(1) = res(1)%p%getNodeId()

            numRmv = t%rmvNodes(coordsList=query, radii=radii, ids=centerIds)
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_IDS_MATCH ---'
                write(*, '(A,I0)') 'expected numRmv=1 (id filter restricts to 1), got: ', numRmv
                stop 1
            end if
            if (pop .ne. 8_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_IDS_MATCH ---'
                write(*, '(A,I0)') 'expected pop=8, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRadIdsMatch
end program Testv050_RMV_NODES_RAD_IDS_MATCH
