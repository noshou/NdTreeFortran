program Testv020_DUPLICATES_THREE_AXIS_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call duplicatesThreeAxis_rNN_Euclidean()
    contains
        !> 3D tree, all 9 points at (5,5,5). rNN_Centroid at (5,5,5) r=0.01, euclidean:
        !! all 9 at distance 0, expect 9 results.
        subroutine duplicatesThreeAxis_rNN_Euclidean()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64, &
                 5.0_real64, 5.0_real64, 5.0_real64], [3, 9])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([5.0_real64, 5.0_real64, 5.0_real64], 0.01_real64)
            if (size(res) .ne. 9) then
                write(*, '(A)') '--- Testv020_DUPLICATES_THREE_AXIS_RNN_EUCLIDEAN ---'
                write(*, *) 'expected 9 nodes, got:', size(res)
                stop 1
            end if
        end subroutine duplicatesThreeAxis_rNN_Euclidean
end program Testv020_DUPLICATES_THREE_AXIS_RNN_EUCLIDEAN
