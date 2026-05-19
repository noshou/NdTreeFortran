program Testv050_MULTITHREAD_INTERNAL_STATE_MODS_RESET
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call multithreadInternalStateModsReset()
    contains
        !> After 4 concurrent rmvNodes calls each triggering a rebuild,
        !! getNumMods must be 0 (rebuild always resets modifications).
        subroutine multithreadInternalStateModsReset()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: queries(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            integer(int64) :: numMods
            integer        :: i

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, queries)
            do i = 1, 4
                block
                    real(real64) :: q(2, 1)
                    integer      :: numRmv
                    q(:, 1) = queries(:, i)
                    numRmv  = t%rmvNodes(coordsList=q)
                end block
            end do
            !$OMP END PARALLEL DO

            numMods = t%getNumMods()
            if (numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_INTERNAL_STATE_MODS_RESET ---'
                write(*, '(A,I0)') 'expected numMods=0 after concurrent rmvNodes (rebuild resets mods), got: ', numMods
                stop 1
            end if
        end subroutine multithreadInternalStateModsReset
end program Testv050_MULTITHREAD_INTERNAL_STATE_MODS_RESET
