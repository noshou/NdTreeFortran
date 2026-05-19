program Testv010_COLLINEAR_ONE_AXIS_III 
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearOne_AxisIII()
    contains 
        !> Builds a 3D tree where all points share the same axis-3 value,
        !! so the depth-2 split degenerates. Set comparison absorbs
        !! sibling-order ambiguity.
        subroutine collinearOne_AxisIII()
            type(KdTree)   :: t
            real(real64) :: coords(3, 6) = reshape(             &
                [  1.0_real64,  0.92_real64,       4.0_real64,  &
                52.0_real64,  0.42_real64,       4.0_real64,  &
                13.0_real64,  0.00003_real64,    4.0_real64,  &
                87.0_real64,  93291.0_real64,    4.0_real64,  &
                -98.0_real64, -93131913.0_real64, 4.0_real64,  &
                121.0_real64,  0.0_real64,        4.0_real64], [3, 6])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (13.00, 0.3000E-4, 4.000)',           &
                '  [axis=2] (-98.00, -0.9313E+8, 4.000)',       &
                '    [axis=3] (1.000, 0.9200, 4.000)',          &
                '  [axis=2] (52.00, 0.4200, 4.000)',            &
                '    [axis=3] (121.0, 0.000, 4.000)',           &
                '    [axis=3] (87.00, 0.9329E+5, 4.000)']

            call t%build(coords)
            call t%assert('Testv010_COLLINEAR_ONE_AXIS_III', expected)
        end subroutine collinearOne_AxisIII

end program Testv010_COLLINEAR_ONE_AXIS_III