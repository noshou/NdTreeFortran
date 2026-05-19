program Testv050_RNN_RAD_SINGLE_QUERY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadSingleQuery()
    contains
        !> Single query: radius captures exactly the expected nodes.
        !! 5-node cross pattern centred at (1,1); radius 1.1 from (1,1) captures 5.
        subroutine rnnRadSingleQuery()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, &
                 2.0_real64, 1.0_real64, 1.0_real64, 2.0_real64], [2, 5])
            real(real64)                    :: q(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.1_real64]
            type(NdNodeBucket), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Rad(q, r)

            if (size(res(1)%nodes) .ne. 5) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_SINGLE_QUERY: expected 5, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine rnnRadSingleQuery
end program Testv050_RNN_RAD_SINGLE_QUERY
