program Testv021_DATA_INPUT_EMPTY_TREE_EMPTY_DATA
    use NdTreeFortran
    use iso_fortran_env, only: real64
    call dataInputEmptyTreeEmptyData()
    contains 

        !> Pass empty empty data array to an empty tree
        subroutine dataInputEmptyTreeEmptyData()
            type(KdTree)               :: t
            real(real64)               :: coords(2, 0)
            character(len=1)           :: data(0)
            call t%build(coords, data)
        end subroutine dataInputEmptyTreeEmptyData
end program Testv021_DATA_INPUT_EMPTY_TREE_EMPTY_DATA