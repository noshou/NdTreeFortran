program Testv030_ADD_NODES_REBUILD_LEAF_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildLeafRnnChebyshev()
    contains
        !> Build with 8 nodes; add 1 node (leaf insert). Verify rNN_Centroid (chebyshev) finds it.
        subroutine rebuildLeafRnnChebyshev()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64)               :: new_coords(2, 1) = reshape([50.0_real64, 50.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: numMods

            call t%build(init_coords)
            call t%addNodes(new_coords)
            numMods = t%getNumMods() 
            res = t%rNN_Centroid([50.0_real64, 50.0_real64], 0.1_real64, metric='chebyshev')

            if (numMods .ne. 1_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected numMods=1 (leaf insert), got: ', numMods
                stop 1
            end if
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 1 node at leaf, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildLeafRnnChebyshev
end program Testv030_ADD_NODES_REBUILD_LEAF_RNN_CHEBYSHEV
