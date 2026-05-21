!> Expected-fail: BallTree rNN_Centroid on an empty tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv071_RNN_CENTROID_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        !> rNN_Centroid on an empty tree must error stop.
        subroutine run()
            type(BallTree)               :: t
            real(real64)               :: coords(2, 0),  centroid(2) = [0.0_real64, 0.0_real64], r=0.9
            type(NdNodePtr), allocatable :: res(:)
            call t%build(coords)
            res = t%rNN_Centroid(centroid, r) ! expected to fail here
            write(*, '(A)') '--- Testv071_RNN_CENTROID_EMPTY_TREE ---'
            write(*, '(A)') 'expected program to fail, but ran successfully!'
        end subroutine run

end program Testv071_RNN_CENTROID_EMPTY_TREE
