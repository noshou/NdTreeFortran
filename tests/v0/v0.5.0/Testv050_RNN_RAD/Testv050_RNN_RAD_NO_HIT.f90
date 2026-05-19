program Testv050_RNN_RAD_NO_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadNoHit()
    contains
        !> Query with radius too small to capture any node returns empty bucket.
        subroutine rnnRadNoHit()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                    :: q(2, 1) = reshape([10.0_real64, 10.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [0.01_real64]
            type(NdNodeBucket), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Rad(q, r)

            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_NO_HIT: expected 0, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine rnnRadNoHit
end program Testv050_RNN_RAD_NO_HIT
