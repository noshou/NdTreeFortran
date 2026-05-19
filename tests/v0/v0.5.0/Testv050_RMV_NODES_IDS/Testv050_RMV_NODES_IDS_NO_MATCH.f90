program Testv050_RMV_NODES_IDS_NO_MATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesIdsNoMatch()
    contains
        !> ids-only branch: id not present in pool; numRmv=0, pop unchanged.
        subroutine rmvNodesIdsNoMatch()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            type(NodeId)   :: targetIds(1)
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(ids=targetIds)
            pop    = t%getPop()

            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IDS_NO_MATCH ---'
                write(*, '(A,I0)') 'expected numRmv=0 for non-existent id, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IDS_NO_MATCH ---'
                write(*, '(A,I0)') 'expected pop=3 unchanged, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesIdsNoMatch
end program Testv050_RMV_NODES_IDS_NO_MATCH
