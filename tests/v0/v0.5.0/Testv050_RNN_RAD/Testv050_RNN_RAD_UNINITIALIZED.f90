program Testv050_RNN_RAD_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadUninitialized()
    contains
        !> rNN_Rad on an uninitialized tree must error stop.
        subroutine rnnRadUninitialized()
            type(KdTree)                    :: t
            real(real64)                    :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.0_real64]
            type(NdNodeBucket), allocatable :: res(:)
            res = t%rNN_Rad(q, r)
        end subroutine rnnRadUninitialized
end program Testv050_RNN_RAD_UNINITIALIZED
