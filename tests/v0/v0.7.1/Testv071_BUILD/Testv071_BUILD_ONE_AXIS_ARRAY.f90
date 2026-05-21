program Testv071_BUILD_ONE_AXIS_ARRAY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call oneAxisArray()
    contains

        !> 1D, 7 distinct points.
        subroutine oneAxisArray()
            type(BallTree) :: t
            real(real64) :: coords(1, 7) = reshape( &
                [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, &
                5.0_real64, 9.0_real64, 2.0_real64], [1, 7])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (3.000)',         &
                '  [axis=1] (1.000)',       &
                '    [axis=1] (1.000)',     &
                '    [axis=1] (2.000)',     &
                '  [axis=1] (5.000)',       &
                '    [axis=1] (4.000)',     &
                '    [axis=1] (9.000)']

            call t%build(coords)
            call t%assert('Testv071_BUILD_ONE_AXIS_ARRAY', expected)
        end subroutine oneAxisArray
end program Testv071_BUILD_ONE_AXIS_ARRAY
