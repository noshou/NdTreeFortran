!> Expected-fail: setMetric on an already-built tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv071_METRIC_SET_ON_INITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree) :: t
        real(real64)   :: coords(2, 3) = reshape( &
            [1.0_real64, 1.0_real64,  &
             2.0_real64, 2.0_real64,  &
             3.0_real64, 3.0_real64], [2, 3])

        call t%build(coords)
        call t%setMetric('manhattan')

        write(*, '(A)') '--- Testv071_METRIC_SET_ON_INITIALIZED ---'
        write(*, '(A)') 'expected error stop, but setMetric returned normally'
    end subroutine run
end program Testv071_METRIC_SET_ON_INITIALIZED
