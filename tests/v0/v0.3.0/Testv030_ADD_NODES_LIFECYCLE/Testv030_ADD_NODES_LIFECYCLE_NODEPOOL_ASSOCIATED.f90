program Testv030_ADD_NODES_LIFECYCLE_NODEPOOL_ASSOCIATED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesLifecycleNodepoolAssociated()
    contains
        !> nodePool and root must still be associated after addNodes.
        subroutine addNodesLifecycleNodepoolAssociated()
            type(KdTree)   :: t
            real(real64) :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64) :: new_coords(2, 1) = reshape([5.0_real64, 5.0_real64], [2, 1])
            logical :: assertNodePool, assertRoot, assertInitState

            call t%build(init_coords)
            call t%addNodes(new_coords)

            call t%associatedNodePool(assertNodePool)
            call t%associatedRoot(assertRoot)
            call t%getInitState(assertInitState)

            if (.not. (assertNodePool .and. assertRoot .and. assertInitState)) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_LIFECYCLE_NODEPOOL_ASSOCIATED ---'
                write(*, '(A,L2)') 'associated(nodePool) = ', assertNodePool
                write(*, '(A,L2)') 'associated(root)     = ', assertRoot
                write(*, '(A,L2)') 'initialized          = ', assertInitState
                stop 1
            end if
        end subroutine addNodesLifecycleNodepoolAssociated
end program Testv030_ADD_NODES_LIFECYCLE_NODEPOOL_ASSOCIATED
