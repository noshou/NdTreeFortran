!> Expected-fail: rNN_Centroid with a negative radius must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_RNN_CENTROID_NEGATIVE_RADIUS

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call rnnCentroid_NegativeRadius()
    contains

        subroutine rnnCentroid_NegativeRadius()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)

            res = t%rNN_Centroid([0.0_real64, 0.0_real64], -1.0_real64)
            write(*, '(A)') '--- Testv020_RNN_CENTROID_NEGATIVE_RADIUS ---'
            write(*,*) 'expected error stop, but rNN_Centroid returned normally'
        end subroutine rnnCentroid_NegativeRadius

end program Testv020_RNN_CENTROID_NEGATIVE_RADIUS
