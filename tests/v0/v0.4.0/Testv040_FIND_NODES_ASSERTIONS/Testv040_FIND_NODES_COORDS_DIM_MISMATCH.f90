program Testv040_FIND_NODES_COORDS_DIM_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call findNodesDimMismatch()
    contains
        !> rNN_Coords with wrong dimension must stop 1.
        subroutine findNodesDimMismatch()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64) :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: bad_query(3, 1) = reshape([0.0_real64, 0.0_real64, 0.0_real64], [3, 1])

            call t%build(init_coords)
            res = t%rNN_Coords(bad_query)
            write(*, '(A)') '--- Testv040_FIND_NODES_COORDS_DIM_MISMATCH ---'
            write(*, '(A)') 'expected stop 1, but rNN_Coords returned normally'
        end subroutine findNodesDimMismatch
end program Testv040_FIND_NODES_COORDS_DIM_MISMATCH
