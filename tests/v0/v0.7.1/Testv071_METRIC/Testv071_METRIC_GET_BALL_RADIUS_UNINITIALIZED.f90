!> Expected-fail: getBallRadius on an UNINITIALIZED tree must error stop.
!! getBallRadius checks this%getInitState first. We build a temp tree only to
!! obtain a valid NdNode value to pass, then call getBallRadius on a separate
!! tree that was never built -> the uninitialized guard fires.
!! Registered with WILL_FAIL in CTest.
program Testv071_METRIC_GET_BALL_RADIUS_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree)               :: built, fresh
        real(real64)                 :: coords(2, 3) = reshape( &
            [1.0_real64, 1.0_real64,  &
             2.0_real64, 2.0_real64,  &
             3.0_real64, 3.0_real64], [2, 3])
        type(NdNodePtr), allocatable :: res(:)
        real(real64)                 :: r

        call built%build(coords)
        res = built%getAllNodes()

        ! 'fresh' was never built -> getBallRadius must error stop.
        r = fresh%getBallRadius(res(1)%p)

        write(*, '(A)') '--- Testv071_METRIC_GET_BALL_RADIUS_UNINITIALIZED ---'
        write(*, '(A,G0.4)') 'expected error stop, but getBallRadius returned: ', r
    end subroutine run
end program Testv071_METRIC_GET_BALL_RADIUS_UNINITIALIZED
