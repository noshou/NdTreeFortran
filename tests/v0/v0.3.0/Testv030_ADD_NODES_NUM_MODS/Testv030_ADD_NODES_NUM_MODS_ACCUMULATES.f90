program Testv030_ADD_NODES_NUM_MODS_ACCUMULATES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call numModsAccumulates()
    contains
        !> With ratio=0.9 all adds are leaf inserts; getNumMods accumulates by batch size each call.
        !! Build 8 nodes (ratio=0.9). Three addNodes calls:
        !!   add 1: 0+1 > 0.9*8=7.2 -> FALSE, leaf, mods=1, pop=9
        !!   add 2: 1+2 > 0.9*9=8.1 -> FALSE, leaf, mods=3, pop=11
        !!   add 1: 3+1 > 0.9*11=9.9 -> FALSE, leaf, mods=4, pop=12
        subroutine numModsAccumulates()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64) :: batch1(2, 1) = reshape([100.0_real64, 0.0_real64], [2, 1])
            real(real64) :: batch2(2, 2) = reshape( &
                [200.0_real64, 0.0_real64, 300.0_real64, 0.0_real64], [2, 2])
            real(real64) :: batch3(2, 1) = reshape([400.0_real64, 0.0_real64], [2, 1])
            integer(int64) :: numMods, pop

            call t%build(init_coords)
            call t%setRebuildRatio(0.9_real64)

            call t%addNodes(batch1)
            numMods = t%getNumMods() 
            pop     = t%getPop()
            if (numMods .ne. 1_int64 .or. pop .ne. 9_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ACCUMULATES ---'
                write(*, '(A)')    'after batch1 (add 1):'
                write(*, '(A,I0)') 'expected numMods=1, got: ', numMods
                write(*, '(A,I0)') 'expected pop=9,     got: ', pop
                stop 1
            end if

            call t%addNodes(batch2)
            numMods = t%getNumMods() 
            pop     = t%getPop()
            if (numMods .ne. 3_int64 .or. pop .ne. 11_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ACCUMULATES ---'
                write(*, '(A)')    'after batch2 (add 2):'
                write(*, '(A,I0)') 'expected numMods=3,  got: ', numMods
                write(*, '(A,I0)') 'expected pop=11,     got: ', pop
                stop 1
            end if

            call t%addNodes(batch3)
            numMods = t%getNumMods() 
            pop     = t%getPop()
            if (numMods .ne. 4_int64 .or. pop .ne. 12_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_NUM_MODS_ACCUMULATES ---'
                write(*, '(A)')    'after batch3 (add 1):'
                write(*, '(A,I0)') 'expected numMods=4,  got: ', numMods
                write(*, '(A,I0)') 'expected pop=12,     got: ', pop
                stop 1
            end if
        end subroutine numModsAccumulates
end program Testv030_ADD_NODES_NUM_MODS_ACCUMULATES
