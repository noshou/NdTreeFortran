program Testv071_BUILD_COLLINEAR_TWO_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearTwo_AxisIAxisII()
    contains
        !> Builds a 3D tree where all points share both axis-1 and axis-2
        !! values, leaving axis 3 as the only discriminating dimension.
        !! Set comparison absorbs sibling-order ambiguity.
        subroutine collinearTwo_AxisIAxisII()
            type(BallTree) :: t
            real(real64) :: coords(3, 6) = reshape(  &
                [5.0_real64, 4.0_real64,  2.0_real64,    &
                5.0_real64, 4.0_real64,  1.0_real64,    &
                5.0_real64, 4.0_real64, -131.0_real64,  &
                5.0_real64, 4.0_real64,  31313.0_real64,&
                5.0_real64, 4.0_real64, -31.0_real64,   &
                5.0_real64, 4.0_real64,  432.419_real64], [3, 6])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000, 4.000, -131.0)',              &
                '  [axis=2] (5.000, 4.000, 2.000)',             &
                '    [axis=3] (5.000, 4.000, 1.000)',           &
                '  [axis=2] (5.000, 4.000, -31.00)',            &
                '    [axis=3] (5.000, 4.000, 0.3131E+5)',       &
                '    [axis=3] (5.000, 4.000, 432.4)']

            call t%build(coords)
            call t%assert('Testv071_BUILD_COLLINEAR_TWO_AXIS', expected)
        end subroutine collinearTwo_AxisIAxisII
end program Testv071_BUILD_COLLINEAR_TWO_AXIS
