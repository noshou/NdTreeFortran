program Testv021_LIFECYCLE_BUILD_DOUBLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call lifecycleBuildDouble()
    contains 
        
        !> checks if "double build" correctly throws an error stop
        subroutine lifecycleBuildDouble()
            type(KdTree)      :: t
            real(real64)    :: coords(4, 7) = reshape(                                &
                [3.0_real64, 1.0_real64, -13.31_real64,        0.92_real64,         &
                1.0_real64, 4.0_real64,  34.14_real64,       92.0_real64,          &
                4.0_real64, 1.0_real64, -1093.3139_real64,  312.0_real64,          &
                1.0_real64, 5.0_real64,  0.013_real64,    13112.0_real64,          &
                5.0_real64, 9.0_real64,  33.13_real64,       -5.13192_real64,      &
                9.0_real64, 2.0_real64,  734.348_real64,      0.00000092_real64,   &
                2.0_real64, 6.0_real64, -93.131_real64,       0.9412412_real64], [4, 7])

            call t%build(coords)
            call t%build(coords)

            write(*, '(A)')    '--- Testv021_LIFECYCLE_BUILD_DOUBLE ---'
            write(*, '(A)')    'expected program to fail, but executed successfully!'

        end subroutine lifecycleBuildDouble
end program Testv021_LIFECYCLE_BUILD_DOUBLE