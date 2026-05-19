program Testv050_RNN_RAD_IDS_NO_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rnnRadIdsNoHit()
    contains
        !> Two independent no-hit paths:
        !! a) radius captures nodes but none have the requested id (node_id=0 never exists)
        !! b) id exists but is outside the search radius
        subroutine rnnRadIdsNoHit()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable    :: farNode(:)
            type(NodeId)                    :: wrongId(1), farId(1)
            real(real64)                    :: q(2, 1)   = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r_big(1)  = [10.0_real64]
            real(real64)                    :: r_tiny(1) = [0.01_real64]
            type(NdNodeBucket), allocatable :: res(:)

            call t%build(coords)

            ! a) node_id=0 is never assigned; radius captures all 3 nodes but id won't match
            wrongId(1)%node_id  = 0_int64
            wrongId(1)%pool_idx = 0_int64
            res = t%rNN_RadIds(q, r_big, wrongId)
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_NO_HIT (bad id): expected 0, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! b) id of (0,1) node exists but radius=0.01 only covers (0,0)
            farNode = t%rNN_Centroid([0.0_real64, 1.0_real64], 0.01_real64)
            if (size(farNode) .ne. 1) then
                write(*, '(A)') '--- RNN_RAD_IDS_NO_HIT: setup failed'; stop 1
            end if
            farId(1) = farNode(1)%p%getNodeId()

            res = t%rNN_RadIds(q, r_tiny, farId)
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_NO_HIT (id out of radius): expected 0, got: ', &
                    size(res(1)%nodes); stop 1
            end if
        end subroutine rnnRadIdsNoHit
end program Testv050_RNN_RAD_IDS_NO_HIT
