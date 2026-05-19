program Testv050_RNN_RAD_MULTI_ROUND
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadMultiRound()
    contains
        !> Three alternating add/remove/search rounds; the radius is large enough to
        !! capture the whole tree each time so counts equal pop exactly.
        !!
        !! Round 1: build(2) → search → expect 2
        !! Round 2: add(2) → search → expect 4; remove 1 → search → expect 3
        !! Round 3: add(1) → search → expect 4; remove 2 → search → expect 2
        subroutine rnnRadMultiRound()
            type(KdTree)                    :: t
            real(real64)                    :: batch1(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: batch2(2, 2) = reshape( &
                [2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: batch3(2, 1) = reshape( &
                [4.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: rmv1(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: rmv2(2, 2) = reshape( &
                [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: q(2, 1) = reshape([2.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [100.0_real64]
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            ! Round 1
            call t%build(batch1)
            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_MULTI_ROUND r1: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 2a: add
            call t%addNodes(batch2)
            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A,I0)') '--- RNN_RAD_MULTI_ROUND r2a: expected 4, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 2b: remove 1
            numRmv = t%rmvNodes(coordsList=rmv1)
            res    = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A,I0)') '--- RNN_RAD_MULTI_ROUND r2b: expected 3, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 3a: add 1
            call t%addNodes(batch3)
            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 4) then
                write(*, '(A,I0)') '--- RNN_RAD_MULTI_ROUND r3a: expected 4, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            ! Round 3b: remove 2
            numRmv = t%rmvNodes(coordsList=rmv2)
            res    = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- RNN_RAD_MULTI_ROUND r3b: expected 2, got: ', &
                    size(res(1)%nodes); stop 1
            end if
        end subroutine rnnRadMultiRound
end program Testv050_RNN_RAD_MULTI_ROUND
