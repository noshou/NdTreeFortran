!> Expected-fail: rNN_Centroid on an empty tree must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_EMPTY_TREE_RNN_CENTROID
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call emptyTree()
    contains

        !> rNN_Centroid on an empty tree must error stop.
        subroutine emptyTree()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 0),  centroid(2) = [0.0_real64, 0.0_real64], r=0.9
            type(NdNodePtr), allocatable :: res(:)
            call t%build(coords)
            res = t%rNN_Centroid(centroid, r) ! expected to fail here
            write(*, '(A)') '--- Testv020_EMPTY_TREE_RNN_CENTROID ---'
            write(*, '(A)') 'expected program to fail, but ran successfully!'
        end subroutine emptyTree

end program Testv020_EMPTY_TREE_RNN_CENTROID