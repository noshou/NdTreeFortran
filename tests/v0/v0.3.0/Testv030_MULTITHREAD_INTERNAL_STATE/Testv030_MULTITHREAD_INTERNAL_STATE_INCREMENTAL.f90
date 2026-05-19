program Testv030_MULTITHREAD_INTERNAL_STATE_INCREMENTAL
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call mtIncremental()
    contains
        !> 4 threads each add one node at a time (5 steps) to their own tree.
        !! ratio=0.9 ensures all are leaf inserts. After each add: pop=4+k, numMods=k.
        !! After all adds: rNN at each added point finds exactly 1 node (euclidean).
        subroutine mtIncremental()
            real(real64)   :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            integer        :: i
            logical        :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    real(real64)               :: step(2, 1)
                    type(NdNodePtr), allocatable :: res(:)
                    integer(int64)             :: numMods, pop, k

                    call t%build(init_coords)
                    call t%setRebuildRatio(0.9_real64)

                    do k = 1, 5
                        step(1, 1) = real(k, real64) * 100.0_real64
                        step(2, 1) = 0.0_real64
                        call t%addNodes(step)

                        numMods = t%getNumMods()
                        pop     = t%getPop()
                        if (numMods .ne. k .or. pop .ne. 4_int64 + k) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                            exit
                        end if

                        res = t%rNN_Centroid([step(1,1), 0.0_real64], 0.5_real64, metric='euclidean')
                        if (size(res) .ne. 1) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                            exit
                        end if
                    end do
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_INCREMENTAL ---'
                write(*, '(A)') 'one or more threads had wrong state or rNN result during incremental adds'
                stop 1
            end if
        end subroutine mtIncremental
end program Testv030_MULTITHREAD_INTERNAL_STATE_INCREMENTAL
