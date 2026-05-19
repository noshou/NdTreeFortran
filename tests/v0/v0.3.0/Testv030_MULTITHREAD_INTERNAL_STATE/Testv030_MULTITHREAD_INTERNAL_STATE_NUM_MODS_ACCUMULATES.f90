program Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_ACCUMULATES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call mtNumModsAccumulates()
    contains
        !> 4 threads independently accumulate numMods via three leaf-insert addNodes calls.
        !! Each thread: build 8, ratio=0.9. add 1 (mods=1), add 2 (mods=3), add 1 (mods=4).
        subroutine mtNumModsAccumulates()
            real(real64) :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64) :: batch1(2, 1) = reshape([100.0_real64, 0.0_real64], [2, 1])
            real(real64) :: batch2(2, 2) = reshape( &
                [200.0_real64, 0.0_real64, 300.0_real64, 0.0_real64], [2, 2])
            real(real64) :: batch3(2, 1) = reshape([400.0_real64, 0.0_real64], [2, 1])
            integer      :: i
            logical      :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)     :: t
                    integer(int64) :: numMods, pop

                    call t%build(init_coords)
                    call t%setRebuildRatio(0.9_real64)

                    call t%addNodes(batch1)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 1_int64 .or. pop .ne. 9_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%addNodes(batch2)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 3_int64 .or. pop .ne. 11_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%addNodes(batch3)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 4_int64 .or. pop .ne. 12_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_ACCUMULATES ---'
                write(*, '(A)') 'one or more threads had wrong numMods or pop during accumulation'
                stop 1
            end if
        end subroutine mtNumModsAccumulates
end program Testv030_MULTITHREAD_INTERNAL_STATE_NUM_MODS_ACCUMULATES
