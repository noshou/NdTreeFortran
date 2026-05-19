!> Expected-fail: euclideanDist on nodes with unallocated coords must error stop.
!! Registered with WILL_FAIL in CTest.
program Testv020_NODE_QUERY_EUCLIDEAN_UNALLOCATED

    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call nodeQuery_Euclidean_Unallocated()
    contains

        subroutine nodeQuery_Euclidean_Unallocated()
            type(NdNode)   :: n1, n2
            real(real64) :: dist

            dist = n1%euclideanDist(n2)
            write(*, '(A)') '--- Testv020_NODE_QUERY_EUCLIDEAN_UNALLOCATED ---'
            write(*,*) 'expected error stop, but euclideanDist returned normally'
        end subroutine nodeQuery_Euclidean_Unallocated

end program Testv020_NODE_QUERY_EUCLIDEAN_UNALLOCATED
