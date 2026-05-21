program Testv060_DBSCAN_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanUninitialized()
    contains
        subroutine dbscanUninitialized()
            type(KdTree)                    :: t
            type(NdNodeBucket), allocatable :: res(:)
            res = t%DBSCAN(minPts=2_int64, radius=1.0_real64)
            write(*, '(A)') '--- Testv060_DBSCAN_UNINITIALIZED ---'
            write(*, '(A)') 'expected error stop before build, but returned normally'
        end subroutine dbscanUninitialized
end program Testv060_DBSCAN_UNINITIALIZED
