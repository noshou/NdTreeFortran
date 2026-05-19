program Testv050_RNN_RAD_IDS_LIFECYCLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rnnRadIdsLifecycle()
    contains
        !> search → addNodes → search → rmvNodes → search: id-filtered results
        !! track the changing pool.
        subroutine rnnRadIdsLifecycle()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 3])
            real(real64)                    :: extra(2, 2)  = reshape( &
                [3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: rmvQ(2, 1)   = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: q(2, 1) = reshape([2.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [10.0_real64]
            type(NdNodePtr), allocatable    :: originNode(:)
            type(NodeId)                    :: originId(1)
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coords)
            originNode = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(originNode) .ne. 1) then
                write(*, '(A)') '--- Testv050_RNN_RAD_IDS_LIFECYCLE: setup failed'; stop 1
            end if
            originId(1) = originNode(1)%p%getNodeId()

            res = t%rNN_RadIds(q, r, originId)
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_LIFECYCLE (before rmv): expected 1, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            call t%addNodes(extra)
            res = t%rNN_RadIds(q, r, originId)
            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_LIFECYCLE (after add): expected 1, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            numRmv = t%rmvNodes(coordsList=rmvQ)
            if (numRmv .ne. 1) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_LIFECYCLE: rmvNodes returned ', numRmv; stop 1
            end if

            res = t%rNN_RadIds(q, r, originId)
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_LIFECYCLE (after rmv): expected 0, got: ', &
                    size(res(1)%nodes); stop 1
            end if
        end subroutine rnnRadIdsLifecycle
end program Testv050_RNN_RAD_IDS_LIFECYCLE
