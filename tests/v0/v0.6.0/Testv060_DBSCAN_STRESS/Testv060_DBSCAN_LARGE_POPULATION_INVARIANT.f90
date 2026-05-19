program Testv060_DBSCAN_LARGE_POPULATION_INVARIANT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call dbscanLargePopulationInvariant()
    contains
        !> 10000 random points, arbitrary params: verify sum(bucket sizes)==pop.
        subroutine dbscanLargePopulationInvariant()
            type(KdTree)                    :: t
            integer,            parameter   :: n = 10000
            real(real64),       allocatable :: coords(:,:)
            type(NdNodeBucket), allocatable :: res(:)
            integer(int64)                  :: total, pop
            integer                         :: i

            allocate(coords(2, n))
            call random_seed()
            call random_number(coords)
            coords = coords * 10.0_real64

            call t%build(coords)
            res = t%DBSCAN(minPts=5, radius=0.3_real64)

            pop   = t%getPop()
            total = 0_int64
            do i = 1, size(res)
                total = total + int(size(res(i)%nodes), int64)
            end do

            if (total .ne. pop) then
                write(*, '(A)')    '--- Testv060_DBSCAN_LARGE_POPULATION_INVARIANT ---'
                write(*, '(A,I0,A,I0)') 'population invariant failed: total=', total, ', pop=', pop
                stop 1
            end if
        end subroutine dbscanLargePopulationInvariant
end program Testv060_DBSCAN_LARGE_POPULATION_INVARIANT
