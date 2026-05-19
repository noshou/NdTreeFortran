program Testv030_MULTITHREAD_INTERNAL_STATE_SET_REBUILD_RATIO
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call mtSetRebuildRatio()
    contains
        !> 4 threads each build their own tree and call setRebuildRatio(0.6).
        !! After addNodes each thread verifies getRebuildRatio=0.6, numMods=1 (leaf insert),
        !! and full state is valid. destroy resets ratio to 0.25; rebuild without param confirms.
        subroutine mtSetRebuildRatio()
            real(real64)   :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64)   :: new_coord(2, 1) = reshape([50.0_real64, 0.0_real64], [2, 1])
            integer        :: i
            logical        :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)     :: t
                    integer(int64) :: numMods, pop
                    real(real64)   :: ratio
                    logical        :: isInit, nodePoolAssoc, rootAssoc

                    call t%build(init_coords)
                    call t%setRebuildRatio(0.6_real64)
                    
                    ! 0+1 > 0.6*8=4.8 -> FALSE -> leaf insert, numMods=1
                    call t%addNodes(new_coord)

                    numMods = t%getNumMods()
                    ratio = t%getRebuildRatio()
                    pop = t%getPop()
                    call t%getInitState(isInit)
                    call t%associatedNodePool(nodePoolAssoc)
                    call t%associatedRoot(rootAssoc)

                    if (ratio .ne. 0.6_real64 .or. numMods .ne. 1_int64 .or. pop .ne. 9_int64 .or. &
                        .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                    end if

                    ! destroy resets ratio; rebuild without param keeps 0.25
                    call t%destroy()
                    call t%build(init_coords)
                    ratio = t%getRebuildRatio()
                    if (ratio .ne. 0.25_real64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_SET_REBUILD_RATIO ---'
                write(*, '(A)') 'one or more threads had wrong state for setRebuildRatio scenario'
                stop 1
            end if
        end subroutine mtSetRebuildRatio
end program Testv030_MULTITHREAD_INTERNAL_STATE_SET_REBUILD_RATIO
