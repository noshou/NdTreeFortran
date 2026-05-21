!> Build a multi-point tree; at least one node (an internal/root ball) must have
!! getBallRadius > 0. Leaves have radius 0; a tree with several points must have
!! at least one positive-radius internal node. Fail with stop 1 if none positive.
program Testv071_METRIC_GET_BALL_RADIUS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree)               :: t
        real(real64)                 :: coords(2, 5) = reshape( &
            [0.0_real64, 0.0_real64,  &
             1.0_real64, 0.0_real64,  &
             0.0_real64, 1.0_real64,  &
             4.0_real64, 4.0_real64,  &
             5.0_real64, 4.0_real64], [2, 5])
        type(NdNodePtr), allocatable :: res(:)
        real(real64)                 :: r
        integer                      :: i
        logical                      :: anyPositive

        call t%build(coords)
        res = t%getAllNodes()

        anyPositive = .false.
        do i = 1, size(res)
            r = t%getBallRadius(res(i)%p)
            if (r .gt. 0.0_real64) anyPositive = .true.
        end do

        if (.not. anyPositive) then
            write(*, '(A)') '--- Testv071_METRIC_GET_BALL_RADIUS ---'
            write(*, '(A,I0,A)') 'expected at least one node with radius > 0 among ', &
                size(res), ' nodes, found none'
            stop 1
        end if

        write(*, '(A)') 'Testv071_METRIC_GET_BALL_RADIUS: found positive ball radius'
    end subroutine run
end program Testv071_METRIC_GET_BALL_RADIUS
