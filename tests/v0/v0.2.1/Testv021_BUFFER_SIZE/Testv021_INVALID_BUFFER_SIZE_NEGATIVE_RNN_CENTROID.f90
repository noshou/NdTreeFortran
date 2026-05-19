!> Expected-fail: rNN_Centroid with bufferSize=-1 must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv021_INVALID_BUFFER_SIZE_NEGATIVE_RNN_CENTROID
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call invalidBufferSizeNegative_rNN_Centroid()
    contains

        subroutine invalidBufferSizeNegative_rNN_Centroid()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                 4.0_real64, 2.0_real64,  0.42_real64,             &
                 3.0_real64, 3.0_real64,  0.00003_real64,          &
                 0.0_real64, 0.0_real64,  0.00000031_real64,       &
                 1.0_real64, 5.0_real64, -93131913.0_real64,       &
                 0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([2.5_real64, 2.5_real64, 0.0_real64], 4.0_real64, bufferSize=-1)
            write(*, '(A)') '--- invalidBufferSizeNegative_rNN_Centroid ---'
            write(*, *) 'expected error stop, but rNN_Centroid returned normally'

        end subroutine invalidBufferSizeNegative_rNN_Centroid

end program Testv021_INVALID_BUFFER_SIZE_NEGATIVE_RNN_CENTROID
