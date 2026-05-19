program Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesEmptyTree()
    contains
        !> After removing all nodes: pop=0, root not associated, nodePool not associated,
        !! tree still initialized.
        subroutine rmvNodesEmptyTree()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)   :: radii(1)   = [100.0_real64]
            real(real64)   :: centre(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop
            logical        :: isInit, nodePoolAssoc, rootAssoc

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=centre, radii=radii)
            pop    = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (pop .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected pop=0, got: ', pop
                stop 1
            end if
            if (.not. isInit) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE ---'
                write(*, '(A)') 'expected isInit=T even after removing all nodes'
                stop 1
            end if
            if (nodePoolAssoc) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE ---'
                write(*, '(A)') 'expected nodePool not associated after removing all nodes'
                stop 1
            end if
            if (rootAssoc) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE ---'
                write(*, '(A)') 'expected root not associated after removing all nodes'
                stop 1
            end if
        end subroutine rmvNodesEmptyTree
end program Testv050_RMV_NODES_INTERNAL_STATE_EMPTY_TREE
