program Testv050_RMV_NODES_COORDS_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordssManhattan()
    contains
        !> coordsList with manhattan metric removes exactly the matching node.
        !! The eps match uses manhattan distance; query exactly at (1,0).
        subroutine rmvNodesCoordssManhattan()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 2.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: query(2, 1) = reshape([1.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, metric='manhattan')
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_MANHATTAN ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_MANHATTAN ---'
                write(*, '(A,I0)') 'expected pop=3, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordssManhattan
end program Testv050_RMV_NODES_COORDS_MANHATTAN
