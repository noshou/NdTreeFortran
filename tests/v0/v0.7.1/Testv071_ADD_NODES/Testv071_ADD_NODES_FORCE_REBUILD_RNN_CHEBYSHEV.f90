program Testv071_ADD_NODES_FORCE_REBUILD_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildForceRnnChebyshev()
    contains
        !> Build with 1 node; add 4 nodes to trigger rebuild.
        !! After rebuild, rNN_Centroid (chebyshev) finds expected nodes.
        subroutine rebuildForceRnnChebyshev()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 1) = reshape([100.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: new_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 3.0_real64, 0.0_real64, &
                0.0_real64, 4.0_real64, 3.0_real64, 4.0_real64], [2, 4])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: numMods

            call t%setMetric('chebyshev')
            call t%build(init_coords)
            call t%addNodes(new_coords)
            numMods = t%getNumMods()
            ! Chebyshev: from (0,0) r=4: (0,0)[0],(3,0)[3],(0,4)[4],(3,4)[4] all inside
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 4.0_real64, metric='chebyshev')

            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_FORCE_REBUILD_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected numMods=0 after rebuild, got: ', numMods
                stop 1
            end if
            if (size(res) .ne. 4) then
                write(*, '(A)')    '--- Testv071_ADD_NODES_FORCE_REBUILD_RNN_CHEBYSHEV ---'
                write(*, '(A,I0)') 'expected 4 nodes, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildForceRnnChebyshev
end program Testv071_ADD_NODES_FORCE_REBUILD_RNN_CHEBYSHEV
