program Testv010_TWO_AXIS_ARRAY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call twoAxisArray()
    contains
        !> 2D, 7 distinct points.
        subroutine twoAxisArray()
            type(KdTree)   :: t
            real(real64) :: coords(2, 7) = reshape( &
                [3.0_real64, 1.0_real64, &
                1.0_real64, 4.0_real64, &
                4.0_real64, 1.0_real64, &
                1.0_real64, 5.0_real64, &
                5.0_real64, 9.0_real64, &
                9.0_real64, 2.0_real64, &
                2.0_real64, 6.0_real64], [2, 7])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (3.000, 1.000)',         &
                '  [axis=2] (1.000, 5.000)',       &
                '    [axis=1] (1.000, 4.000)',     &
                '    [axis=1] (2.000, 6.000)',     &
                '  [axis=2] (9.000, 2.000)',       &
                '    [axis=1] (4.000, 1.000)',     &
                '    [axis=1] (5.000, 9.000)']

            call t%build(coords)
            call t%assert('Testv010_TWO_AXIS_ARRAY', expected)
        end subroutine twoAxisArray

end program Testv010_TWO_AXIS_ARRAY 