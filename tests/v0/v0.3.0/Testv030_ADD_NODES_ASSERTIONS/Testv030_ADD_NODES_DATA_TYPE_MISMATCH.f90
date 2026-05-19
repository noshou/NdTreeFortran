program Testv030_ADD_NODES_DATA_TYPE_MISMATCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesDataTypeMismatch()
    contains
        !> addNodes with data of different type than tree data must error stop.
        subroutine addNodesDataTypeMismatch()
            type(KdTree)       :: t
            real(real64)     :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            character(len=1) :: init_data(3) = ['A', 'B', 'C']
            real(real64)     :: new_coords(2, 2) = reshape( &
                [2.0_real64, 2.0_real64, 3.0_real64, 3.0_real64], [2, 2])
            integer          :: bad_data(2) = [1, 2]
            call t%build(init_coords, init_data)
            call t%addNodes(new_coords, bad_data)
            write(*, '(A)') '--- Testv030_ADD_NODES_DATA_TYPE_MISMATCH ---'
            write(*, '(A)') 'expected error stop, but addNodes returned normally'
        end subroutine addNodesDataTypeMismatch
end program Testv030_ADD_NODES_DATA_TYPE_MISMATCH
