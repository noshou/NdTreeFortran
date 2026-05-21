program Testv071_BUILD_COLLINEAR_ONE_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call collinearOne_AxisI()
    contains
        !> 3D, all points share axis 1.
        subroutine collinearOne_AxisI()
            type(BallTree) :: t
            real(real64) :: coords(3, 6) = reshape(             &
                [5.0_real64, 1.0_real64,  0.92_real64,          &
                5.0_real64, 2.0_real64,  0.42_real64,          &
                5.0_real64, 3.0_real64,  0.00003_real64,       &
                5.0_real64, 4.0_real64,  93291.0_real64,       &
                5.0_real64, 5.0_real64, -93131913.0_real64,    &
                5.0_real64, 6.0_real64,  0.0_real64], [3, 6])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000, 3.000, 0.3000E-4)',           &
                '  [axis=2] (5.000, 1.000, 0.9200)',            &
                '    [axis=3] (5.000, 2.000, 0.4200)',          &
                '  [axis=2] (5.000, 5.000, -0.9313E+8)',        &
                '    [axis=3] (5.000, 4.000, 0.9329E+5)',       &
                '    [axis=3] (5.000, 6.000, 0.000)']

            call t%build(coords)
            call t%assert('Testv071_BUILD_COLLINEAR_ONE_AXIS', expected)
        end subroutine collinearOne_AxisI
end program Testv071_BUILD_COLLINEAR_ONE_AXIS
