!> Expected-fail: setMetric with an unknown metric must error stop.
!! Registered with WILL_FAIL in CTest. ('badmetric' is 9 chars.)
program Testv071_METRIC_SET_UNKNOWN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree) :: t

        call t%setMetric('badmetric')

        write(*, '(A)') '--- Testv071_METRIC_SET_UNKNOWN ---'
        write(*, '(A)') 'expected error stop, but setMetric returned normally'
    end subroutine run
end program Testv071_METRIC_SET_UNKNOWN
