program Testv050_RMV_NODES_RAD_NO_HIT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRadNoHit()
    contains
        !> coordsList+radii: radius too small to capture any node; numRmv=0, pop unchanged.
        subroutine rmvNodesRadNoHit()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)   :: query(2, 1) = reshape([10.0_real64, 10.0_real64], [2, 1])
            real(real64)   :: radii(1)    = [0.5_real64]
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            pop    = t%getPop()

            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_NO_HIT ---'
                write(*, '(A,I0)') 'expected numRmv=0 (no nodes in radius), got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_RAD_NO_HIT ---'
                write(*, '(A,I0)') 'expected pop=3 unchanged, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRadNoHit
end program Testv050_RMV_NODES_RAD_NO_HIT
