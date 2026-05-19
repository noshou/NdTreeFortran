program Testv050_RMV_NODES_BAD_METRIC
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesBadMetric()
    contains
        !> Unknown metric string must error stop.
        subroutine rmvNodesBadMetric()
            type(KdTree) :: t
            real(real64) :: init(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer      :: numRmv
            call t%build(init)
            numRmv = t%rmvNodes(coordsList=query, metric='minkowski')
            write(*, '(A)') '--- Testv050_RMV_NODES_BAD_METRIC ---'
            write(*, '(A)') 'expected error stop for unknown metric'
        end subroutine rmvNodesBadMetric
end program Testv050_RMV_NODES_BAD_METRIC
