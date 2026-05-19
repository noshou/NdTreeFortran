program Testv020_DUPLICATES_ONE_AXIS_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call duplicatesOneAxis_rNN_Chebyshev()
    contains
        !> 1D tree, all 9 points at (5). rNN_Centroid at (5) r=0.01, chebyshev:
        !! all 9 at distance 0, expect 9 results.
        subroutine duplicatesOneAxis_rNN_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(1, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64], [1, 9])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([5.0_real64], 0.01_real64, metric='chebyshev')
            if (size(res) .ne. 9) then
                write(*, '(A)') '--- Testv020_DUPLICATES_ONE_AXIS_RNN_CHEBYSHEV ---'
                write(*, *) 'expected 9 nodes, got:', size(res)
                stop 1
            end if
        end subroutine duplicatesOneAxis_rNN_Chebyshev
end program Testv020_DUPLICATES_ONE_AXIS_RNN_CHEBYSHEV
