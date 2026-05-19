program Testv060_DBSCAN_POPULATION_INVARIANT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanPopulationInvariant()
    contains
        !> sum of all bucket sizes must equal tree population.
        subroutine dbscanPopulationInvariant()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 8) = reshape( &
                [0.0_real64, 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.1_real64, 0.1_real64, &
                 10.0_real64, 0.0_real64, 10.1_real64, 0.0_real64, &
                 5.0_real64, 5.0_real64, 20.0_real64, 20.0_real64], [2, 8])
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: pop, total
            integer                         :: i

            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=0.5_real64)
            pop = t%getPop()

            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_POPULATION_INVARIANT ---'
                write(*, '(A,I0,A,I0)') 'expected total==pop==', pop, ', got total: ', total
                stop 1
            end if
        end subroutine dbscanPopulationInvariant
end program Testv060_DBSCAN_POPULATION_INVARIANT
