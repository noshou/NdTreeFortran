!> Expected-fail: manhattanDist on nodes with unallocated coords must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NODE_QUERY_MANHATTAN_UNALLOCATED

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nodeQuery_Manhattan_Unallocated()
    contains

        subroutine nodeQuery_Manhattan_Unallocated()
            type(NdNode)   :: n1, n2
            real(real64) :: dist

            dist = n1%manhattanDist(n2)
            write(*, '(A)') '--- Testv020_NODE_QUERY_MANHATTAN_UNALLOCATED ---'
            write(*,*) 'expected error stop, but manhattanDist returned normally'
        end subroutine nodeQuery_Manhattan_Unallocated

end program Testv020_NODE_QUERY_MANHATTAN_UNALLOCATED
