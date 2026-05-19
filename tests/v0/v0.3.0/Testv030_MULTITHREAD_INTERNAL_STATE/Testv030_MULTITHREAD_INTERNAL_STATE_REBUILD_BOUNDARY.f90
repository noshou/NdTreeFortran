program Testv030_MULTITHREAD_INTERNAL_STATE_REBUILD_BOUNDARY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call mtRebuildBoundary()
    contains
        !> 4 threads each run the full accumulated-boundary scenario independently.
        !! Build 4, ratio=0.5:
        !!   add 2: 0+2>2=F -> leaf (mods=2, pop=6)
        !!   add 1: 2+1>3=F -> leaf (mods=3, pop=7)
        !!   add 1: 3+1>3=T -> rebuild (mods=0, pop=8)
        !! rNN after rebuild finds all 8 nodes.
        subroutine mtRebuildBoundary()
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, &
                20.0_real64, 0.0_real64, 30.0_real64, 0.0_real64], [2, 4])
            real(real64) :: batch1(2, 2) = reshape( &
                [40.0_real64, 0.0_real64, 50.0_real64, 0.0_real64], [2, 2])
            real(real64) :: batch2(2, 1) = reshape([60.0_real64, 0.0_real64], [2, 1])
            real(real64) :: batch3(2, 1) = reshape([70.0_real64, 0.0_real64], [2, 1])
            integer      :: i
            logical      :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    type(NdNodePtr), allocatable :: res(:)
                    integer(int64)             :: numMods, pop

                    call t%build(init_coords)
                    call t%setRebuildRatio(0.5_real64)

                    call t%addNodes(batch1)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 2_int64 .or. pop .ne. 6_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%addNodes(batch2)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 3_int64 .or. pop .ne. 7_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    call t%addNodes(batch3)
                    numMods = t%getNumMods()
                    pop = t%getPop()
                    if (numMods .ne. 0_int64 .or. pop .ne. 8_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    res = t%rNN_Centroid([35.0_real64, 0.0_real64], 40.0_real64, metric='euclidean')
                    if (size(res) .ne. 8) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_REBUILD_BOUNDARY ---'
                write(*, '(A)') 'one or more threads failed the accumulated rebuild boundary scenario'
                stop 1
            end if
        end subroutine mtRebuildBoundary
end program Testv030_MULTITHREAD_INTERNAL_STATE_REBUILD_BOUNDARY
