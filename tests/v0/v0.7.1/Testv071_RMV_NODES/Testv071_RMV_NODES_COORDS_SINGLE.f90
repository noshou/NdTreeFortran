program Testv071_RMV_NODES_COORDS_SINGLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordssSingleHit()
    contains
        !> coordsList only: query exactly matches one node; numRmv=1, pop=2.
        subroutine rmvNodesCoordssSingleHit()
            type(BallTree) :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)   :: query(2, 1) = reshape([1.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_COORDS_SINGLE ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 2_int64) then
                write(*, '(A)')    '--- Testv071_RMV_NODES_COORDS_SINGLE ---'
                write(*, '(A,I0)') 'expected pop=2, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordssSingleHit
end program Testv071_RMV_NODES_COORDS_SINGLE
