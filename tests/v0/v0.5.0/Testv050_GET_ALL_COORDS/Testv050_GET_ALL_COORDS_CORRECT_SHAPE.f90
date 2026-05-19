program Testv050_GET_ALL_COORDS_CORRECT_SHAPE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getAllCoordsCorrectShape()
    contains
        !> getAllCoords returns shape [dim, pop].
        subroutine getAllCoordsCorrectShape()
            type(KdTree)              :: t
            real(real64)              :: coords(3, 4) = reshape( &
                [1.0_real64, 0.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 0.0_real64, 1.0_real64, &
                 1.0_real64, 1.0_real64, 1.0_real64], [3, 4])
            real(real64), allocatable :: got(:,:)
            integer(int64)            :: pop

            call t%build(coords)
            got = t%getAllCoords()
            pop = t%getPop()

            if (size(got, 1) .ne. 3 .or. size(got, 2) .ne. int(pop)) then
                write(*, '(A)')    '--- Testv050_GET_ALL_COORDS_CORRECT_SHAPE ---'
                write(*, '(A,I0,A,I0)') 'expected shape [3,', pop, '], got [', size(got,1)
                write(*, '(A,I0,A)')    '  ', size(got,2), ']'
                stop 1
            end if
        end subroutine getAllCoordsCorrectShape
end program Testv050_GET_ALL_COORDS_CORRECT_SHAPE
