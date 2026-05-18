program Testv050_RMV_NODES_RAD_IDS_NO_FILTER
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRadIdsNoFilter()
    contains
        !> coordsList+radii+ids: ids set contains none of the nodes within the radius;
        !! numRmv=0, pop unchanged.
        subroutine rmvNodesRadIdsNoFilter()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 5.0_real64, 5.0_real64], [2, 4])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64) :: radii(1)    = [1.5_real64]
            type(NodeId) :: wrongIds(1)
            integer      :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            wrongIds(1)%node_id  = 9999_int64
            wrongIds(1)%pool_idx = 0_int64
            numRmv = t%rmvNodes(coordsList=query, radii=radii, ids=wrongIds)
            pop    = t%getPop()

            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_IDS_NO_FILTER ---'
                write(*, '(A,I0)') 'expected numRmv=0 (ids set misses all nearby nodes), got: ', numRmv
                stop 1
            end if
            if (pop .ne. 4_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_IDS_NO_FILTER ---'
                write(*, '(A,I0)') 'expected pop=4 unchanged, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRadIdsNoFilter
end program Testv050_RMV_NODES_RAD_IDS_NO_FILTER
