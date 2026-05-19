program Testv021_DATA_INPUT_SIZE_MISMATCH_UNPOPULATED_DATA
    use NdTreeFortran 
    use iso_fortran_env, only: real64
    implicit none

    call dataInputSizeMismatchUnpopulatedData()
    contains 

        !> expected to run; passes precondition:
        !!
        !!  (# coords == # data points) || (# data points == 0)
        subroutine dataInputSizeMismatchUnpopulatedData()
            type(KdTree)          :: t
            real(real64)        :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                4.0_real64, 2.0_real64,  0.42_real64,             &
                3.0_real64, 3.0_real64,  0.00003_real64,          &
                0.0_real64, 0.0_real64,  0.00000031_real64,       &
                1.0_real64, 5.0_real64, -93131913.0_real64,       &
                0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            character(len=1)    :: data(0)
            
            call t%build(coords, data)

        end subroutine dataInputSizeMismatchUnpopulatedData

end program Testv021_DATA_INPUT_SIZE_MISMATCH_UNPOPULATED_DATA