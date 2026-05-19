program Testv030_ADD_NODES_TREE_NO_DATA_DATA_GIVEN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesTreeNoDataDataGiven()
    contains
        !> Tree built without data but addNodes called with data must error stop.
        subroutine addNodesTreeNoDataDataGiven()
            type(KdTree)       :: t
            real(real64)     :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)     :: new_coords(2, 2) = reshape( &
                [2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 2])
            character(len=1) :: extra_data(2) = ['D', 'E']
            call t%build(init_coords)
            call t%addNodes(new_coords, extra_data)
            write(*, '(A)') '--- Testv030_ADD_NODES_TREE_NO_DATA_DATA_GIVEN ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesTreeNoDataDataGiven
end program Testv030_ADD_NODES_TREE_NO_DATA_DATA_GIVEN
