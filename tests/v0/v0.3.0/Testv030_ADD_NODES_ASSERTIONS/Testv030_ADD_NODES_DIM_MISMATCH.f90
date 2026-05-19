program Testv030_ADD_NODES_DIM_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesDimMismatch()
    contains
        !> addNodes with wrong dimension must error stop.
        subroutine addNodesDimMismatch()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [2, 3])
            real(real64) :: bad_coords(3, 2) = reshape( &
                [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64], [3, 2])
            call t%build(init_coords)
            call t%addNodes(bad_coords)
            write(*, '(A)') '--- Testv030_ADD_NODES_DIM_MISMATCH ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesDimMismatch
end program Testv030_ADD_NODES_DIM_MISMATCH
