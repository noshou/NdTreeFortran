!> Expected-fail: getMetric on an uninitialized (un-built) tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv071_METRIC_GET_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree)   :: t
        character(len=9) :: m

        m = t%getMetric()

        write(*, '(A)') '--- Testv071_METRIC_GET_UNINITIALIZED ---'
        write(*, '(A,A,A)') 'expected error stop, but getMetric returned: ', m
    end subroutine run
end program Testv071_METRIC_GET_UNINITIALIZED
