program Testv030_ADD_NODES_TREE_HAS_DATA_NO_DATA_GIVEN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesTreeHasDataNoDataGiven()
    contains
        !> Tree built with data but addNodes called without data must error stop.
        subroutine addNodesTreeHasDataNoDataGiven()
            type(KdTree)       :: t
            real(real64)     :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            character(len=1) :: init_data(3) = ['A', 'B', 'C']
            real(real64)     :: new_coords(2, 2) = reshape( &
                [2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 2])
            call t%build(init_coords, init_data)
            call t%addNodes(new_coords)
            write(*, '(A)') '--- Testv030_ADD_NODES_TREE_HAS_DATA_NO_DATA_GIVEN ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesTreeHasDataNoDataGiven
end program Testv030_ADD_NODES_TREE_HAS_DATA_NO_DATA_GIVEN
