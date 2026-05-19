program Testv010_THREE_AXIS_ARRAY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call threeAxisArray()
    contains
                !> 3D, 7 distinct points.
        subroutine threeAxisArray()
            type(KdTree)   :: t
            real(real64) :: coords(3, 7) = reshape(         &
                [3.0_real64, 1.0_real64, -13.31_real64,     &
                1.0_real64, 4.0_real64,  34.14_real64,     &
                4.0_real64, 1.0_real64, -1093.3139_real64, &
                1.0_real64, 5.0_real64,  0.013_real64,     &
                5.0_real64, 9.0_real64,  33.13_real64,     &
                9.0_real64, 2.0_real64,  734.348_real64,   &
                2.0_real64, 6.0_real64, -93.131_real64], [3, 7])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (3.000, 1.000, -13.31)',           &
                '  [axis=2] (1.000, 5.000, 0.1300E-1)',      &
                '    [axis=3] (1.000, 4.000, 34.14)',        &
                '    [axis=3] (2.000, 6.000, -93.13)',       &
                '  [axis=2] (9.000, 2.000, 734.3)',          &
                '    [axis=3] (4.000, 1.000, -1093.)',       &
                '    [axis=3] (5.000, 9.000, 33.13)']

            call t%build(coords)
            call t%assert('Testv010_THREE_AXIS_ARRAY', expected)
        end subroutine threeAxisArray

end program Testv010_THREE_AXIS_ARRAY