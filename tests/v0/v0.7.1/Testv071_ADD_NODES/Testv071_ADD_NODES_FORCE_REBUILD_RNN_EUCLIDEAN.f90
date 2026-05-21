program Testv071_ADD_NODES_FORCE_REBUILD_RNN_EUCLIDEAN
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildForceRnnEuclidean()
    contains
        !> Build with 1 node; add 4 nodes. modifications(0)+4 > 0.25*(5-4)=0.25 -> TRUE -> rebuild.
        !! After rebuild, getNumMods returns 0. rNN_Centroid finds all 4 nodes in region.
        subroutine rebuildForceRnnEuclidean()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 1) = reshape([100.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: new_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, &
                0.0_real64, 4.0_real64, 3.0_real64, 4.0_real64], [2, 4])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: numMods

            call t%build(init_coords)
            call t%addNodes(new_coords)
            numMods = t%getNumMods()
            res = t%rNN_Centroid([1.5_real64, 2.0_real64], 5.0_real64, metric='euclidean')

            if (numMods .ne. 0_int64) then
                write(*, '(A)')         '--- Testv071_ADD_NODES_FORCE_REBUILD_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)')      'expected numMods=0 after rebuild, got: ', numMods
                stop 1
            end if
            if (size(res) .ne. 4) then
                write(*, '(A)')         '--- Testv071_ADD_NODES_FORCE_REBUILD_RNN_EUCLIDEAN ---'
                write(*, '(A,I0)')      'expected 4 nodes in search region, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildForceRnnEuclidean
end program Testv071_ADD_NODES_FORCE_REBUILD_RNN_EUCLIDEAN
