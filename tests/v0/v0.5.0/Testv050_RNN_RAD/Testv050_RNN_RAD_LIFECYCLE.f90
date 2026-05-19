program Testv050_RNN_RAD_LIFECYCLE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadLifecycle()
    contains
        !> search → addNodes → search → rmvNodes → search: results track tree state.
        !!
        !! Build 3-node tree; r=10 captures all 3. Add 2 nodes; r=10 captures 5.
        !! Remove 2; r=10 captures 3.
        subroutine rnnRadLifecycle()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 3])
            real(real64)                    :: extra(2, 2)  = reshape( &
                [3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: rmvQ(2, 2)   = reshape( &
                [3.0_real64, 0.0_real64, 4.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: q(2, 1) = reshape([2.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [10.0_real64]
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coords)

            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_LIFECYCLE (after build): expected 3, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            call t%addNodes(extra)
            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 5) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_LIFECYCLE (after add): expected 5, got: ', &
                    size(res(1)%nodes); stop 1
            end if

            numRmv = t%rmvNodes(coordsList=rmvQ)
            if (numRmv .ne. 2) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_LIFECYCLE (rmvNodes): expected 2, got: ', &
                    numRmv; stop 1
            end if

            res = t%rNN_Rad(q, r)
            if (size(res(1)%nodes) .ne. 3) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_LIFECYCLE (after rmv): expected 3, got: ', &
                    size(res(1)%nodes); stop 1
            end if
        end subroutine rnnRadLifecycle
end program Testv050_RNN_RAD_LIFECYCLE
