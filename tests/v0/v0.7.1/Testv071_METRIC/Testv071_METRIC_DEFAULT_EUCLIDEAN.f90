!> A BallTree built without setMetric must default to 'euclidean'.
program Testv071_METRIC_DEFAULT_EUCLIDEAN
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

        call t%build(coords)
        m = t%getMetric()

        if (m .ne. 'euclidean') then
            write(*, '(A)') '--- Testv071_METRIC_DEFAULT_EUCLIDEAN ---'
            write(*, '(A,A,A)') "expected: 'euclidean', got: '", m, "'"
            stop 1
        end if

        write(*, '(A)') 'Testv071_METRIC_DEFAULT_EUCLIDEAN: default metric is euclidean'
    end subroutine run
end program Testv071_METRIC_DEFAULT_EUCLIDEAN
