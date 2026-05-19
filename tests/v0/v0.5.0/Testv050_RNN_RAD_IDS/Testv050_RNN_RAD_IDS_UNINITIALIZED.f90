program Testv050_RNN_RAD_IDS_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadIdsUninitialized()
    contains
        !> rNN_RadIds on an uninitialized tree must error stop.
        subroutine rnnRadIdsUninitialized()
            type(KdTree)                    :: t
            real(real64)                    :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.0_real64]
            type(NodeId)                    :: ids(1)
            type(NdNodeBucket), allocatable :: res(:)
            res = t%rNN_RadIds(q, r, ids)
        end subroutine rnnRadIdsUninitialized
end program Testv050_RNN_RAD_IDS_UNINITIALIZED
