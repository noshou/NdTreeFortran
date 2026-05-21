!> Expected-fail: a tree built euclidean (default) queried with metric='manhattan'
!! must stop 1 (metric-match constraint in assertMetric).
!! Registered with WILL_FAIL in CTest.
program Testv071_METRIC_QUERY_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call run()
contains
    subroutine run()
        type(BallTree)               :: t
        real(real64)                 :: coords(2, 3) = reshape( &
            [1.0_real64, 1.0_real64,  &
             2.0_real64, 2.0_real64,  &
             3.0_real64, 3.0_real64], [2, 3])
        type(NdNodePtr), allocatable :: res(:)

        call t%build(coords)
        res = t%rNN_Centroid([1.0_real64, 1.0_real64], 5.0_real64, metric='manhattan')

        write(*, '(A)') '--- Testv071_METRIC_QUERY_MISMATCH ---'
        write(*, '(A,I0,A)') 'expected stop 1, but query returned ', size(res), ' nodes'
    end subroutine run
end program Testv071_METRIC_QUERY_MISMATCH
