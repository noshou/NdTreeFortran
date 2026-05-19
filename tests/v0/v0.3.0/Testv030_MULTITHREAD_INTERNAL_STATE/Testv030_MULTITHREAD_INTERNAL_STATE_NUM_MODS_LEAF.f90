program Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_LEAF
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call numModsLeaf()
    contains
        !> Large shared tree (40 nodes); 4 threads each add 1 node concurrently.
        !! 0.25*40=10.0 >> 4, so all adds are guaranteed leaf inserts regardless
        !! of CRITICAL-section ordering. Final numMods=4, pop=44.
        subroutine numModsLeaf()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 40)
            real(real64) :: per_thread(2, 1, 4)
            integer      :: i
            integer(int64) :: numMods, pop

            do i = 1, 40
                init_coords(1, i) = real(i, real64)
                init_coords(2, i) = 0.0_real64
            end do
            do i = 1, 4
                per_thread(1, 1, i) = real(40 + i, real64)
                per_thread(2, 1, i) = 0.0_real64
            end do

            call t%build(init_coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1)
            do i = 1, 4
                call t%addNodes(per_thread(:,:,i))
            end do
            !$OMP END PARALLEL DO

            numMods = t%getNumMods()
            pop = t%getPop()

            if (numMods .ne. 4_int64) then
                write(*, '(A)')    '--- Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_LEAF ---'
                write(*, '(A,I0)') 'expected numMods=4 (all leaf inserts), got: ', numMods
                stop 1
            end if
            if (pop .ne. 44_int64) then
                write(*, '(A)')    '--- Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_LEAF ---'
                write(*, '(A,I0)') 'expected pop=44, got: ', pop
                stop 1
            end if
        end subroutine numModsLeaf
end program Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_LEAF
