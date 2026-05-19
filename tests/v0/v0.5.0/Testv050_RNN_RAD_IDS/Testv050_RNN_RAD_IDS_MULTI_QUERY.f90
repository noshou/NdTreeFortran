program Testv050_RNN_RAD_IDS_MULTI_QUERY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadIdsMultiQuery()
    contains
        !> Two queries with different radii and a shared id filter set.
        !! ids set contains ids of both cluster centres; each query finds only
        !! its own centre (the other centre is out of radius).
        subroutine rnnRadIdsMultiQuery()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.5_real64, 0.0_real64, &
                 5.0_real64, 0.0_real64, 5.5_real64, 0.0_real64], [2, 4])
            ! q1 at (0,0) r=0.6 → captures (0,0),(0.5,0); q2 at (5,0) r=0.6 → captures (5,0),(5.5,0)
            real(real64)                    :: q(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: r(2)    = [0.6_real64, 0.6_real64]
            type(NdNodePtr), allocatable    :: c1(:), c2(:)
            type(NodeId)                    :: ids(2)
            type(NdNodeBucket), allocatable :: res(:)
            type(NodeId)                    :: tmp

            call t%build(coords)
            c1 = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            c2 = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
            if (size(c1) .ne. 1 .or. size(c2) .ne. 1) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_MULTI_QUERY: setup failed'; stop 1
            end if
            ids(1) = c1(1)%p%getNodeId()
            ids(2) = c2(1)%p%getNodeId()

            res = t%rNN_RadIds(q, r, ids)

            ! each bucket should contain only its own centre
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_IDS_MULTI_QUERY q1: expected 1, got: ', &
                    size(res(1)%nodes); stop 1
            end if
            tmp = res(1)%nodes(1)%p%getNodeId()
            if (tmp%node_id .ne. ids(1)%node_id) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_MULTI_QUERY q1: wrong node returned'; stop 1
            end if
            if (size(res(2)%nodes) .ne. 1) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_IDS_MULTI_QUERY q2: expected 1, got: ', &
                    size(res(2)%nodes); stop 1
            end if
            tmp = res(2)%nodes(1)%p%getNodeId()
            if (tmp%node_id .ne. ids(2)%node_id) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_MULTI_QUERY q2: wrong node returned'; stop 1
            end if
        end subroutine rnnRadIdsMultiQuery
end program Testv050_RNN_RAD_IDS_MULTI_QUERY
