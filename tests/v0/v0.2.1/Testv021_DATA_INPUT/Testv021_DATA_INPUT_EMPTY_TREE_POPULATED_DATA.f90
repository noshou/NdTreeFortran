program Testv021_DATA_INPUT_EMPTY_TREE_POPULATED_DATA
    use NdTreeFortran
    use iso_fortran_env, only: real64

    call dataInputEmptyTreePopulatedData()
    contains 
        
        !> expected to fail (size mismatch on data and coords)
        subroutine dataInputEmptyTreePopulatedData()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 0)
            character(len=1)           :: data(6) = ['1', '2', '3', '4', '5', '6']
            call t%build(coords, data)
            write(*, '(A)') '--- dataInputEmptyTreePopulatedData ---'
            write(*,*) 'expected program to fail, but program executed successfully!'
        end subroutine dataInputEmptyTreePopulatedData

end program Testv021_DATA_INPUT_EMPTY_TREE_POPULATED_DATA