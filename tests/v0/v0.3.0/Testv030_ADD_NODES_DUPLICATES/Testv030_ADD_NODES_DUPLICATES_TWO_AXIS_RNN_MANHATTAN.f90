program Testv030_ADD_NODES_DUPLICATES_TWO_AXIS_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call duplicatesTwoAxisRnnManhattan()
    contains
        subroutine duplicatesTwoAxisRnnManhattan()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 2) = reshape( &
                [1.0_real64, 1.0_real64, 9.0_real64, 9.0_real64], [2, 2])
            real(real64)               :: dup_coords(2, 5) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(dup_coords)
            res = t%rNN_Centroid([5.0_real64, 5.0_real64], 0.01_real64, metric='manhattan')

            if (size(res) .ne. 5) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_DUPLICATES_TWO_AXIS_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 5 duplicate nodes, got: ', size(res)
                stop 1
            end if
        end subroutine duplicatesTwoAxisRnnManhattan
end program Testv030_ADD_NODES_DUPLICATES_TWO_AXIS_RNN_MANHATTAN
