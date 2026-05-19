program Testv030_MULTITHREAD_INTERNAL_STATE_ZERO_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call mtZeroAdd()
    contains
        !> 4 threads each build their own tree and call addNodes with a zero-column array.
        !! All tree state (pop, numMods, ratio, initState, nodePool, root) must be unchanged.
        subroutine mtZeroAdd()
            real(real64)   :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            integer        :: i
            logical        :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)     :: t
                    real(real64)   :: zero_coords(2, 0)
                    integer(int64) :: numMods, pop
                    real(real64)   :: ratio
                    logical        :: isInit, nodePoolAssoc, rootAssoc

                    call t%build(init_coords)
                    call t%addNodes(zero_coords)

                    numMods = t%getNumMods()
                    ratio   = t%getRebuildRatio()
                    pop     = t%getPop()
                    call t%getInitState(isInit)
                    call t%associatedNodePool(nodePoolAssoc)
                    call t%associatedRoot(rootAssoc)

                    if (numMods .ne. 0_int64 .or. pop .ne. 4_int64 .or. &
                        ratio .ne. 0.25_real64 .or. .not. isInit .or. &
                        .not. nodePoolAssoc .or. .not. rootAssoc) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_ZERO_ADD ---'
                write(*, '(A)') 'one or more threads had wrong state after zero-node addNodes'
                stop 1
            end if
        end subroutine mtZeroAdd
end program Testv030_MULTITHREAD_INTERNAL_STATE_ZERO_ADD
