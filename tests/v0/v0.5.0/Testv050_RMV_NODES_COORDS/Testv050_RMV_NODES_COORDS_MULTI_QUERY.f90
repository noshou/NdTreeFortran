program Testv050_RMV_NODES_COORDS_MULTI_QUERY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordsMultiQuery()
    contains
        !> coordsList with 3 queries removes 3 different nodes; numRmv=3, pop=6.
        subroutine rmvNodesCoordsMultiQuery()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: query(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 1.0_real64, 1.0_real64], [2, 3])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            pop    = t%getPop()

            if (numRmv .ne. 3) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_MULTI_QUERY ---'
                write(*, '(A,I0)') 'expected numRmv=3, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 6_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_MULTI_QUERY ---'
                write(*, '(A,I0)') 'expected pop=6, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordsMultiQuery
end program Testv050_RMV_NODES_COORDS_MULTI_QUERY
