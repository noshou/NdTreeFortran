program Testv050_RNN_RAD_IDS_MULTI_ROUND
    use KdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rnnRadIdsMultiRound()
    contains
        !> Three rounds; ids set updated each round to reflect current pool.
        !!
        !! Round 1: build(A,B) → search with ids=[A,B] → expect 2 total across 1 bucket
        !! Round 2: addNodes(C) → search with ids=[B,C] → expect 2; remove A → search → expect 2
        !! Round 3: addNodes(D) → remove B → search with ids=[C,D] → expect 2; ids=[A,B] → expect 0
        subroutine rnnRadIdsMultiRound()
            type(KdTree)                    :: t
            real(real64)                    :: coordsAB(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: coordsC(2, 1) = reshape( &
                [2.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: coordsD(2, 1) = reshape( &
                [3.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: rmvA(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: rmvB(2, 1) = reshape([1.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: q(2, 1)    = reshape([1.5_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)       = [100.0_real64]
            type(KdNodePtr), allocatable    :: pA(:), pB(:), pC(:), pD(:)
            type(NodeId)                    :: idA, idB, idC, idD
            type(KdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coordsAB)
            pA = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            pB = t%rNN_Centroid([1.0_real64, 0.0_real64], 0.01_real64)
            idA = pA(1)%p%getNodeId(); idB = pB(1)%p%getNodeId()

            ! Round 1
            res = t%rNN_RadIds(q, r, [idA, idB])
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_MULTI_ROUND r1: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 2a: add C
            call t%addNodes(coordsC)
            pC = t%rNN_Centroid([2.0_real64, 0.0_real64], 0.01_real64)
            idC = pC(1)%p%getNodeId()

            res = t%rNN_RadIds(q, r, [idB, idC])
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_MULTI_ROUND r2a: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 2b: remove A
            numRmv = t%rmvNodes(coordsList=rmvA)
            res    = t%rNN_RadIds(q, r, [idB, idC])
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_MULTI_ROUND r2b: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 3: add D, remove B
            call t%addNodes(coordsD)
            pD = t%rNN_Centroid([3.0_real64, 0.0_real64], 0.01_real64)
            idD = pD(1)%p%getNodeId()
            numRmv = t%rmvNodes(coordsList=rmvB)

            res = t%rNN_RadIds(q, r, [idC, idD])
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_MULTI_ROUND r3 C+D: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            res = t%rNN_RadIds(q, r, [idA, idB])
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- RNN_RAD_IDS_MULTI_ROUND r3 A+B: expected 0, got: ', &
                    size(res(1)%nodes); stop 1
            end if
        end subroutine rnnRadIdsMultiRound
end program Testv050_RNN_RAD_IDS_MULTI_ROUND
