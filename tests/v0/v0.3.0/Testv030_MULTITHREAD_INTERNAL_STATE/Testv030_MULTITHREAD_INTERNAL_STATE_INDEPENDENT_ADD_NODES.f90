program Testv030_MULTITHREAD_INTERNAL_STATE_INDEPENDENT_ADD_NODES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call independentAddNodesState()
    contains
        !> 4 threads each build their own tree (4 nodes), setRebuildRatio(0.9), addNodes(2).
        !! 0+2 > 0.9*4=3.6 -> FALSE -> leaf insert. Each thread verifies:
        !!   pop=6, numMods=2, getRebuildRatio=0.9, getInitState=T, nodePool=T, root=T.
        !! After parallel region: all 4 treeIds are distinct.
        subroutine independentAddNodesState()
            real(real64)   :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: new_coords(2, 2) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 60.0_real64], [2, 2])
            integer        :: i
            logical        :: failed = .false.
            integer(int64) :: treeIds(4)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed, treeIds)
            do i = 1, 4
                block
                    type(KdTree)     :: t
                    integer(int64) :: numMods, pop
                    real(real64)   :: ratio
                    logical        :: isInit, nodePoolAssoc, rootAssoc

                    call t%build(init_coords)
                    call t%setRebuildRatio(0.9_real64)
                    call t%addNodes(new_coords)
                    treeIds(i) = t%getTreeId()

                    numMods = t%getNumMods()
                    ratio   = t%getRebuildRatio()
                    pop     = t%getPop()
                    call t%getInitState(isInit)
                    call t%associatedNodePool(nodePoolAssoc)
                    call t%associatedRoot(rootAssoc)

                    if (numMods .ne. 2_int64 .or. pop .ne. 6_int64 .or. &
                        ratio .ne. 0.9_real64 .or. .not. isInit .or. &
                        .not. nodePoolAssoc .or. .not. rootAssoc) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_INDEPENDENT_ADD_NODES ---'
                write(*, '(A)') 'one or more threads had wrong internal state after addNodes'
                stop 1
            end if

            if (treeIds(1) .eq. treeIds(2) .or. treeIds(1) .eq. treeIds(3) .or. &
                treeIds(1) .eq. treeIds(4) .or. treeIds(2) .eq. treeIds(3) .or. &
                treeIds(2) .eq. treeIds(4) .or. treeIds(3) .eq. treeIds(4)) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INTERNAL_STATE_INDEPENDENT_ADD_NODES ---'
                write(*, '(A)') 'concurrent builds with addNodes produced duplicate treeIds'
                stop 1
            end if
        end subroutine independentAddNodesState
end program Testv030_MULTITHREAD_INTERNAL_STATE_INDEPENDENT_ADD_NODES
