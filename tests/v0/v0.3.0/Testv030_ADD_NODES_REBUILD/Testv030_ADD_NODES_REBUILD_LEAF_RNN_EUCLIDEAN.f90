program Testv030_ADD_NODES_REBUILD_LEAF_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildLeafRnnEuclidean()
    contains
        !> Build with 8 nodes; add 1 node. modifications(0)+1 > 0.25*(9-1)=2.0 is FALSE -> leaf insert.
        !! getNumMods returns 1. rNN_Centroid finds only the added leaf node.
        subroutine rebuildLeafRnnEuclidean()
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
            res = t%rNN_Centroid([50.0_real64, 50.0_real64], 0.1_real64, metric='euclidean')

            if (numMods .ne. 1_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected numMods=1 (leaf insert), got: ', numMods
                stop 1
            end if
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_LEAF_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)') 'expected 1 node at leaf, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildLeafRnnEuclidean
end program Testv030_ADD_NODES_REBUILD_LEAF_RNN_EUCLIDEAN
