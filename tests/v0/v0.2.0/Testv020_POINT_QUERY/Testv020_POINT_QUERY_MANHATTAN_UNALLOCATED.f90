!> Expected-fail: manhattanDistPoint with an unallocated point must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_POINT_QUERY_MANHATTAN_UNALLOCATED

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call pointQuery_Manhattan_Unallocated()
    contains

        subroutine pointQuery_Manhattan_Unallocated()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 1.0_real64,  &
                 2.0_real64, 1.0_real64,  &
                 3.0_real64, 1.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            real(real64), allocatable  :: point(:)
            real(real64)               :: dist

            call t%build(coords)
            res = t%rNN_Centroid([1.0_real64, 1.0_real64], 0.01_real64)

            dist = res(1)%p%manhattanDistPoint(point)
            write(*, '(A)') '--- Testv020_POINT_QUERY_MANHATTAN_UNALLOCATED ---'
            write(*,*) 'expected error stop, but manhattanDistPoint returned normally'
        end subroutine pointQuery_Manhattan_Unallocated

end program Testv020_POINT_QUERY_MANHATTAN_UNALLOCATED
