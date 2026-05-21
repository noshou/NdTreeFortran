program Testv071_RMV_NODES_RAD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRadMultiple()
    contains
        !> coordsList+radii: radius=1.1 from center (1,1) captures 5 nodes
        !! (center + 4 axis-adjacent); numRmv=5, pop=4.
        subroutine rmvNodesRadMultiple()
            type(BallTree) :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: query(2, 1)  = reshape([1.0_real64, 1.0_real64], [2, 1])
            real(real64)   :: radii(1)     = [1.1_real64]
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            pop    = t%getPop()

            if (numRmv .ne. 5) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_RAD ---'
                write(*, '(A,I0)') 'expected numRmv=5 (center+4 adjacent), got: ', numRmv
                stop 1
            end if
            if (pop .ne. 4_int64) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_RAD ---'
                write(*, '(A,I0)') 'expected pop=4, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRadMultiple
end program Testv071_RMV_NODES_RAD
