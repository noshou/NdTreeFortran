program Testv030_ADD_NODES_DUPLICATES_FOUR_AXIS_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call duplicatesFourAxisRnnEuclidean()
    contains
        !> Build 4D tree with 2 distinct pts. Add 5 duplicate pts at (5,5,5,5).
        subroutine duplicatesFourAxisRnnEuclidean()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(4, 2) = reshape( &
                [1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, &
                9.0_real64, 9.0_real64, 9.0_real64, 9.0_real64], [4, 2])
            real(real64)               :: dup_coords(4, 5) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [4, 5])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(dup_coords)
            res = t%rNN_Centroid([5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], 0.01_real64, metric='euclidean')

            if (size(res) .ne. 5) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_DUPLICATES_FOUR_AXIS_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 5 duplicate nodes, got: ', size(res)
                stop 1
            end if
        end subroutine duplicatesFourAxisRnnEuclidean
end program Testv030_ADD_NODES_DUPLICATES_FOUR_AXIS_RNN_EUCLIDEAN
