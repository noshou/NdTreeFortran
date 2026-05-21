!> A tree built with 'manhattan' queried with metric='manhattan' returns results
!! without error. Verify the expected count and exit 0.
!! Points (1,1),(2,2),(3,3); query centroid (1,1), radius 2 (manhattan).
!! Manhattan dists: (1,1)->0, (2,2)->2, (3,3)->4. Inclusive radius 2 => 2 nodes.
program Testv071_METRIC_QUERY_MATCH
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

        call t%setMetric('manhattan')
        call t%build(coords)
        res = t%rNN_Centroid([1.0_real64, 1.0_real64], 2.0_real64, metric='manhattan')

        if (size(res) .ne. 2) then
            write(*, '(A)') '--- Testv071_METRIC_QUERY_MATCH ---'
            write(*, '(A,I0)') 'expected: 2 nodes, got: ', size(res)
            stop 1
        end if

        write(*, '(A)') 'Testv071_METRIC_QUERY_MATCH: manhattan query returned 2 nodes'
    end subroutine run
end program Testv071_METRIC_QUERY_MATCH
