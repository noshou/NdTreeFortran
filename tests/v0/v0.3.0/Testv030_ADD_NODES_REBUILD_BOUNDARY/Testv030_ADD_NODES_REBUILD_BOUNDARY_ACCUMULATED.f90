program Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rebuildBoundaryAccumulated()
    contains
        !> Accumulated mods cross the rebuild threshold over three addNodes calls.
        !! Build 4, ratio=0.5. Sequence:
        !!   add 2: 0+2 > 0.5*4=2.0 -> FALSE, leaf, mods=2, pop=6
        !!   add 1: 2+1 > 0.5*6=3.0 -> FALSE, leaf, mods=3, pop=7
        !!   add 1: 3+1 > 0.5*7=3.5 -> TRUE,  rebuild, mods=0, pop=8
        !! All 8 nodes findable after final rebuild.
        subroutine rebuildBoundaryAccumulated()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                20.0_real64, 0.0_real64, 30.0_real64, 0.0_real64], [2, 4])
            real(real64)               :: batch1(2, 2) = reshape( &
                [40.0_real64, 0.0_real64, 50.0_real64, 0.0_real64], [2, 2])
            real(real64)               :: batch2(2, 1) = reshape([60.0_real64, 0.0_real64], [2, 1])
            real(real64)               :: batch3(2, 1) = reshape([70.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: numMods, pop

            call t%build(init_coords)
            call t%setRebuildRatio(0.5_real64)

            call t%addNodes(batch1)
            numMods = t%getNumMods()
            pop = t%getPop()
            if (numMods .ne. 2_int64 .or. pop .ne. 6_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED ---'
                write(*, '(A)')    'after batch1:'
                write(*, '(A,I0)') 'expected numMods=2, got: ', numMods
                write(*, '(A,I0)') 'expected pop=6,     got: ', pop
                stop 1
            end if

            call t%addNodes(batch2)
            numMods = t%getNumMods() 
            pop = t%getPop()
            if (numMods .ne. 3_int64 .or. pop .ne. 7_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED ---'
                write(*, '(A)')    'after batch2:'
                write(*, '(A,I0)') 'expected numMods=3, got: ', numMods
                write(*, '(A,I0)') 'expected pop=7,     got: ', pop
                stop 1
            end if

            call t%addNodes(batch3)
            numMods = t%getNumMods() 
            pop = t%getPop()
            if (numMods .ne. 0_int64 .or. pop .ne. 8_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED ---'
                write(*, '(A)')    'after batch3 (rebuild expected):'
                write(*, '(A,I0)') 'expected numMods=0, got: ', numMods
                write(*, '(A,I0)') 'expected pop=8,     got: ', pop
                stop 1
            end if

            res = t%rNN_Centroid([35.0_real64, 0.0_real64], 40.0_real64, metric='euclidean')
            if (size(res) .ne. 8) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED ---'
                write(*, '(A,I0)') 'expected all 8 nodes findable, got: ', size(res)
                stop 1
            end if
        end subroutine rebuildBoundaryAccumulated
end program Testv030_ADD_NODES_REBUILD_BOUNDARY_ACCUMULATED
