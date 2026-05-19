program Testv050_GET_ALL_COORDS_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getAllCoordsUninitialized()
    contains
        !> getAllCoords on an uninitialized tree must error stop.
        subroutine getAllCoordsUninitialized()
            type(KdTree)              :: t
            real(real64), allocatable :: coords(:,:)
            coords = t%getAllCoords()
        end subroutine getAllCoordsUninitialized
end program Testv050_GET_ALL_COORDS_UNINITIALIZED
