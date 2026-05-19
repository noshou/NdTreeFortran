program Testv050_GET_ALL_COORDS_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getAllCoordsEmptyTree()
    contains
        !> After rmvNodes drains the tree (pop=0), getAllCoords returns a
        !! (dim, 0) array without error.
        subroutine getAllCoordsEmptyTree()
            type(KdTree)              :: t
            real(real64)              :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)              :: query(2, 1)  = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)              :: radii(1)     = [100.0_real64]
            real(real64), allocatable :: got(:,:)
            integer                   :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            got = t%getAllCoords()

            if (size(got) .ne. 0) then
                write(*, '(A)')    '--- Testv050_GET_ALL_COORDS_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected size=0 on empty tree, got: ', size(got)
                stop 1
            end if
        end subroutine getAllCoordsEmptyTree
end program Testv050_GET_ALL_COORDS_EMPTY_TREE
