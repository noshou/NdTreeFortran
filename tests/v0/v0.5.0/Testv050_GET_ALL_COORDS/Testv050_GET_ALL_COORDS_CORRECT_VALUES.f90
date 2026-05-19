program Testv050_GET_ALL_COORDS_CORRECT_VALUES
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getAllCoordsCorrectValues()
    contains
        !> Each coordinate column returned by getAllCoords must belong to the
        !! original input set.  Verified by querying rNN_Coords with epsilon=0
        !! for each column; each bucket must contain at least one node (meaning
        !! the coordinate exists in the tree).
        subroutine getAllCoordsCorrectValues()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, &
                 0.0_real64, 3.0_real64, 3.0_real64, 3.0_real64], [2, 4])
            real(real64), allocatable    :: got(:,:)
            type(NdNodeBucket), allocatable :: buckets(:)
            integer                      :: i

            call t%build(coords)
            got     = t%getAllCoords()
            buckets = t%rNN_Coords(got, epsilon=0.0_real64)

            do i = 1, size(buckets)
                if (size(buckets(i)%nodes) .lt. 1) then
                    write(*, '(A)')    '--- Testv050_GET_ALL_COORDS_CORRECT_VALUES ---'
                    write(*, '(A,I0,A)') 'column ', i, ' from getAllCoords not found in tree'
                    stop 1
                end if
            end do
        end subroutine getAllCoordsCorrectValues
end program Testv050_GET_ALL_COORDS_CORRECT_VALUES
