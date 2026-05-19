program Testv030_ADD_NODES_DATA_COUNT_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesDataCountMismatch()
    contains
        !> addNodes with data list size != number of coords must error stop.
        subroutine addNodesDataCountMismatch()
            type(KdTree)       :: t
            real(real64)     :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            character(len=1) :: init_data(3) = ['A', 'B', 'C']
            real(real64)     :: new_coords(2, 2) = reshape( &
                [2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 2])
            character(len=1) :: bad_data(3) = ['D', 'E', 'F']
            call t%build(init_coords, init_data)
            call t%addNodes(new_coords, bad_data)
            write(*, '(A)') '--- Testv030_ADD_NODES_DATA_COUNT_MISMATCH ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesDataCountMismatch
end program Testv030_ADD_NODES_DATA_COUNT_MISMATCH
