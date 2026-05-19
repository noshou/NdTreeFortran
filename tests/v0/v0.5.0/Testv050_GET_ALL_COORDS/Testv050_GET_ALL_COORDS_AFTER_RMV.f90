program Testv050_GET_ALL_COORDS_AFTER_RMV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getAllCoordsAfterRmv()
    contains
        !> After removing 2 nodes from a 5-node tree, getAllCoords returns
        !! exactly 3 columns (the survivors).
        subroutine getAllCoordsAfterRmv()
            type(KdTree)              :: t
            real(real64)              :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            real(real64)              :: rmvQuery(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 2])
            real(real64), allocatable :: got(:,:)
            integer                   :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 2) then
                write(*, '(A)')    '--- Testv050_GET_ALL_COORDS_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected numRmv=2, got: ', numRmv
                stop 1
            end if

            got = t%getAllCoords()

            if (size(got, 1) .ne. 2 .or. size(got, 2) .ne. 3) then
                write(*, '(A)')    '--- Testv050_GET_ALL_COORDS_AFTER_RMV ---'
                write(*, '(A,I0,A,I0,A)') 'expected shape [2,3], got [', &
                    size(got,1), ',', size(got,2), ']'
                stop 1
            end if
        end subroutine getAllCoordsAfterRmv
end program Testv050_GET_ALL_COORDS_AFTER_RMV
