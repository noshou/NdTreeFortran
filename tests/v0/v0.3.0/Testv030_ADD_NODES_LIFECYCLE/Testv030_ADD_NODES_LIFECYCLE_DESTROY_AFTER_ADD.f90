program Testv030_ADD_NODES_LIFECYCLE_DESTROY_AFTER_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesLifecycleDestroyAfterAdd()
    contains
        !> After addNodes then destroy, all state must be cleared.
        subroutine addNodesLifecycleDestroyAfterAdd()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64) :: new_coords(2, 2) = reshape( &
                [3.0_real64, 3.0_real64, 7.0_real64, 7.0_real64], [2, 2])
            logical :: assertNodePool, assertRoot, assertInitState

            call t%build(init_coords)
            call t%addNodes(new_coords)
            call t%destroy()

            call t%associatedNodePool(assertNodePool)
            call t%associatedRoot(assertRoot)
            call t%getInitState(assertInitState)

            if (assertNodePool .or. assertRoot .or. assertInitState) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_LIFECYCLE_DESTROY_AFTER_ADD ---'
                write(*, '(A,L2)') 'expected all false; associated(nodePool) = ', assertNodePool
                write(*, '(A,L2)') 'associated(root)     = ', assertRoot
                write(*, '(A,L2)') 'initialized          = ', assertInitState
                stop 1
            end if
        end subroutine addNodesLifecycleDestroyAfterAdd
end program Testv030_ADD_NODES_LIFECYCLE_DESTROY_AFTER_ADD
