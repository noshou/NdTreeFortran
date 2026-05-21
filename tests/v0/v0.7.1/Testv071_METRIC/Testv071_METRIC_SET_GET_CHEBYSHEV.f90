!> setMetric('chebyshev') before build; getMetric must return 'chebyshev'.
program Testv071_METRIC_SET_GET_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree)   :: t
        real(real64)     :: coords(2, 3) = reshape( &
            [1.0_real64, 1.0_real64,  &
             2.0_real64, 2.0_real64,  &
             3.0_real64, 3.0_real64], [2, 3])
        character(len=9) :: m

        call t%setMetric('chebyshev')
        call t%build(coords)
        m = t%getMetric()

        if (m .ne. 'chebyshev') then
            write(*, '(A)') '--- Testv071_METRIC_SET_GET_CHEBYSHEV ---'
            write(*, '(A,A,A)') "expected: 'chebyshev', got: '", m, "'"
            stop 1
        end if

        write(*, '(A)') 'Testv071_METRIC_SET_GET_CHEBYSHEV: metric set to chebyshev'
    end subroutine run
end program Testv071_METRIC_SET_GET_CHEBYSHEV
