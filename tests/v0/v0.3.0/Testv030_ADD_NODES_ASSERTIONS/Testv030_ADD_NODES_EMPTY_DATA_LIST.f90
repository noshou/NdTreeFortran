program Testv030_ADD_NODES_EMPTY_DATA_LIST
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesEmptyDataList()
    contains
        !> addNodes with a zero-size data list must error stop.
        subroutine addNodesEmptyDataList()
            type(KdTree)       :: t
            real(real64)     :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            character(len=1) :: init_data(3) = ['A', 'B', 'C']
            real(real64)     :: new_coords(2, 2) = reshape( &
                [2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 2])
            character(len=1) :: empty_data(0)
            call t%build(init_coords, init_data)
            call t%addNodes(new_coords, empty_data)
            write(*, '(A)') '--- Testv030_ADD_NODES_EMPTY_DATA_LIST ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesEmptyDataList
end program Testv030_ADD_NODES_EMPTY_DATA_LIST
