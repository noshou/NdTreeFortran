program Testv030_ADD_NODES_INCREMENTAL_STATE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call incrementalState()
    contains
        !> Add one node at a time (5 times). After each add verify pop, numMods,
        !! getRebuildRatio, getInitState, associatedNodePool, associatedRoot.
        !! ratio=0.9 ensures all are leaf inserts:
        !!   add 1: 0+1 > 0.9*4=3.6 -> FALSE (mods=1)
        !!   add 1: 1+1 > 0.9*5=4.5 -> FALSE (mods=2)  ... all 5 adds stay as leaf inserts.
        subroutine incrementalState()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            real(real64)   :: step(2, 1)
            integer(int64) :: numMods, pop, expMods, expPop, k
            real(real64)   :: ratio
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(init_coords)
            call t%setRebuildRatio(0.9_real64)

            do k = 1, 5
                step(1, 1) = real(k, real64) * 100.0_real64
                step(2, 1) = 0.0_real64
                call t%addNodes(step)

                expMods = k
                expPop  = 4_int64 + k
                numMods = t%getNumMods() 
                ratio   = t%getRebuildRatio()
                pop     = t%getPop()
                call t%getInitState(isInit)
                call t%associatedNodePool(nodePoolAssoc)
                call t%associatedRoot(rootAssoc)

                if (numMods .ne. expMods) then
                    write(*, '(A)')      '--- Testv030_ADD_NODES_INCREMENTAL_STATE ---'
                    write(*, '(A,I0,A,I0,A,I0)') 'step ', k, ': expected numMods=', expMods, ', got: ', numMods
                    stop 1
                end if
                if (pop .ne. expPop) then
                    write(*, '(A)')      '--- Testv030_ADD_NODES_INCREMENTAL_STATE ---'
                    write(*, '(A,I0,A,I0,A,I0)') 'step ', k, ': expected pop=', expPop, ', got: ', pop
                    stop 1
                end if
                if (ratio .ne. 0.9_real64 .or. .not. isInit .or. .not. nodePoolAssoc .or. .not. rootAssoc) then
                    write(*, '(A)')      '--- Testv030_ADD_NODES_INCREMENTAL_STATE ---'
                    write(*, '(A,I0)')   'step: ', k
                    write(*, '(A,F8.4)') 'ratio:       ', ratio
                    write(*, '(A,L2)')   'initialized: ', isInit
                    write(*, '(A,L2)')   'nodePool:    ', nodePoolAssoc
                    write(*, '(A,L2)')   'root:        ', rootAssoc
                    stop 1
                end if
            end do
        end subroutine incrementalState
end program Testv030_ADD_NODES_INCREMENTAL_STATE
