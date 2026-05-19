program Testv010_COLLINEAR_TWO_AXIS_II_AXIS_III
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearTwo_AxisIIAxisIII()
    contains 
        !> Builds a 3D tree where all points share both axis-2 and axis-3
        !! values, leaving axis 1 as the only discriminating dimension.
        !! Set comparison absorbs sibling-order ambiguity.
        subroutine collinearTwo_AxisIIAxisIII()
            type(KdTree)   :: t
            real(real64) :: coords(3, 6) = reshape(  &
                [  2.0_real64,    5.0_real64, 4.0_real64, &
                1.0_real64,    5.0_real64, 4.0_real64, &
                -131.0_real64,   5.0_real64, 4.0_real64, &
                31313.0_real64, 5.0_real64, 4.0_real64, &
                -31.0_real64,    5.0_real64, 4.0_real64, &
                432.419_real64, 5.0_real64, 4.0_real64], [3, 6])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (1.000, 5.000, 4.000)',               &
                '  [axis=2] (-131.0, 5.000, 4.000)',            &
                '    [axis=3] (-31.00, 5.000, 4.000)',          &
                '  [axis=2] (432.4, 5.000, 4.000)',             &
                '    [axis=3] (2.000, 5.000, 4.000)',           &
                '    [axis=3] (0.3131E+5, 5.000, 4.000)']

            call t%build(coords)
            call t%assert('Testv010_COLLINEAR_TWO_AXIS_II_AXIS_III', expected)
        end subroutine collinearTwo_AxisIIAxisIII
end program Testv010_COLLINEAR_TWO_AXIS_II_AXIS_III