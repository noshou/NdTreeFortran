program Testv060_DBSCAN_NEG_RADIUS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanNegRadius()
    contains
        subroutine dbscanNegRadius()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            call t%build(coords)
            res = t%DBSCAN(minPts=2, radius=-1.0_real64)
            write(*, '(A)') '--- Testv060_DBSCAN_NEG_RADIUS ---'
            write(*, '(A)') 'expected error stop for radius=-1.0, but returned normally'
        end subroutine dbscanNegRadius
end program Testv060_DBSCAN_NEG_RADIUS
