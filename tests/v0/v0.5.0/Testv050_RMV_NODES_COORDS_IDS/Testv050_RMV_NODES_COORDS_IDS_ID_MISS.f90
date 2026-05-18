program Testv050_RMV_NODES_COORDS_IDS_ID_MISS
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordsIdsIdMiss()
    contains
        !> coordsList+ids (no radii): coord matches but id does not -> no removal.
        !! Query at (0,0) with wrong id; numRmv=0, pop unchanged.
        subroutine rmvNodesCoordsIdsIdMiss()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NodeId) :: wrongIds(1)
            integer      :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            wrongIds(1)%node_id  = 9999_int64
            wrongIds(1)%pool_idx = 0_int64
            numRmv = t%rmvNodes(coordsList=query, ids=wrongIds)
            pop    = t%getPop()

            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_IDS_ID_MISS ---'
                write(*, '(A,I0)') 'expected numRmv=0 (id mismatch), got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_IDS_ID_MISS ---'
                write(*, '(A,I0)') 'expected pop=3 unchanged, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordsIdsIdMiss
end program Testv050_RMV_NODES_COORDS_IDS_ID_MISS
