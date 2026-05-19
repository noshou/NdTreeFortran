program Testv060_DBSCAN_NEG_MINPTS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call dbscanNegMinPts()
    contains
        subroutine dbscanNegMinPts()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            type(NdNodeBucket), allocatable :: res(:)
            call t%build(coords)
            res = t%DBSCAN(minPts=-1, radius=1.0_real64)
            write(*, '(A)') '--- Testv060_DBSCAN_NEG_MINPTS ---'
            write(*, '(A)') 'expected error stop for minPts=-1, but returned normally'
        end subroutine dbscanNegMinPts
end program Testv060_DBSCAN_NEG_MINPTS
