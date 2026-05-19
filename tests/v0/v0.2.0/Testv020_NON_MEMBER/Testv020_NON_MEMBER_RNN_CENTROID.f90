!> Expected-fail: rNN_Centroid with a centroid whose dimension mismatches
!! the tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NON_MEMBER_RNN_CENTROID

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call dimMismatchCentroid()
    contains

        subroutine dimMismatchCentroid()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 2) = reshape([1.0_real64, 2.0_real64, -2.0_real64, -32.3_real64], [2, 2])
            real(real64)               :: centroid3d(3) = [0.0_real64, 0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)

            ! centroid is 3D but tree is 2D -> must error stop
            res = t%rNN_Centroid(centroid3d, 1.0_real64)
            write(*, '(A)') '--- Testv020_NON_MEMBER_RNN_CENTROID ---'
            write(*, '(A)') 'expected error stop, but rNN_Centroid returned normally'

        end subroutine dimMismatchCentroid

end program Testv020_NON_MEMBER_RNN_CENTROID
