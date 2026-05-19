program Testv050_RMV_NODES_RAD_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRadManhattan()
    contains
        !> coordsList+radii with manhattan metric: from (0,0) radius=1.5
        !! captures (0,0) and (1,0) and (0,1) (manhattan dist 0, 1, 1).
        !! numRmv=3, pop=1.
        subroutine rmvNodesRadManhattan()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 5.0_real64, 5.0_real64], [2, 4])
            real(real64)   :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)   :: radii(1)    = [1.5_real64]
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii, metric='manhattan')
            pop    = t%getPop()

            if (numRmv .ne. 3) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_MANHATTAN ---'
                write(*, '(A,I0)') 'expected numRmv=3 with manhattan metric, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 1_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_MANHATTAN ---'
                write(*, '(A,I0)') 'expected pop=1, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRadManhattan
end program Testv050_RMV_NODES_RAD_MANHATTAN
