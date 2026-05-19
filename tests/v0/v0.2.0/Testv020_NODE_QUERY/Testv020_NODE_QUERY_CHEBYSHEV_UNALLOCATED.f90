!> Expected-fail: chebyshevDist on nodes with unallocated coords must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NODE_QUERY_CHEBYSHEV_UNALLOCATED

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nodeQuery_Chebyshev_Unallocated()
    contains

        subroutine nodeQuery_Chebyshev_Unallocated()
            type(NdNode)   :: n1, n2
            real(real64) :: dist

            dist = n1%chebyshevDist(n2)
            write(*, '(A)') '--- Testv020_NODE_QUERY_CHEBYSHEV_UNALLOCATED ---'
            write(*,*) 'expected error stop, but chebyshevDist returned normally'
        end subroutine nodeQuery_Chebyshev_Unallocated

end program Testv020_NODE_QUERY_CHEBYSHEV_UNALLOCATED
