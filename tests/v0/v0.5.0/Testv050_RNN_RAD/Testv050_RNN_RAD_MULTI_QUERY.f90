program Testv050_RNN_RAD_MULTI_QUERY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadMultiQuery()
    contains
        !> Multi-query with different radii: each query independently captures
        !! only nodes within its own radius.
        subroutine rnnRadMultiQuery()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 4.0_real64, 0.0_real64, 5.0_real64, 0.0_real64], [2, 4])
            ! q1 at (0.5,0) r=0.6 → should capture (0,0) and (1,0) = 2 nodes
            ! q2 at (4.5,0) r=0.6 → should capture (4,0) and (5,0) = 2 nodes
            real(real64)                    :: q(2, 2) = reshape( &
                [0.5_real64, 0.0_real64, 4.5_real64, 0.0_real64], [2, 2])
            real(real64)                    :: r(2) = [0.6_real64, 0.6_real64]
            type(NdNodeBucket), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Rad(q, r)

            if (size(res(1)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_MULTI_QUERY q1: expected 2, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
            if (size(res(2)%nodes) .ne. 2) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_MULTI_QUERY q2: expected 2, got: ', &
                    size(res(2)%nodes)
                stop 1
            end if
        end subroutine rnnRadMultiQuery
end program Testv050_RNN_RAD_MULTI_QUERY
