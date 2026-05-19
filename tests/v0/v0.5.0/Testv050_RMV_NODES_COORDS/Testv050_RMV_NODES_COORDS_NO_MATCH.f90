program Testv050_RMV_NODES_COORDS_NO_MATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordsNoMatch()
    contains
        !> coordsList query at a point not in the tree; numRmv=0, pop unchanged.
        subroutine rmvNodesCoordsNoMatch()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)   :: query(2, 1) = reshape([99.0_real64, 99.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            pop    = t%getPop()

            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_NO_MATCH ---'
                write(*, '(A,I0)') 'expected numRmv=0, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_NO_MATCH ---'
                write(*, '(A,I0)') 'expected pop=3 unchanged, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordsNoMatch
end program Testv050_RMV_NODES_COORDS_NO_MATCH
