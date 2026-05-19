program Testv050_RMV_NODES_COORDS_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesCoordssChebyshev()
    contains
        !> coordsList with chebyshev metric removes exactly the matching node.
        subroutine rmvNodesCoordssChebyshev()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 2.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: query(2, 1) = reshape([2.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, metric='chebyshev')
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 3_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_COORDS_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected pop=3, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesCoordssChebyshev
end program Testv050_RMV_NODES_COORDS_CHEBYSHEV
