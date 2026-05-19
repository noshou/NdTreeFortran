program Testv050_RMV_NODES_INTERNAL_STATE_ASSOC
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesAssoc()
    contains
        !> nodePool and root remain associated after partial removal (pop > 0).
        subroutine rmvNodesAssoc()
            type(KdTree) :: t
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer      :: numRmv
            logical      :: nodePoolAssoc, rootAssoc

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (.not. nodePoolAssoc) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_ASSOC ---'
                write(*, '(A)') 'expected nodePool still associated after partial removal'
                stop 1
            end if
            if (.not. rootAssoc) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_ASSOC ---'
                write(*, '(A)') 'expected root still associated after partial removal'
                stop 1
            end if
        end subroutine rmvNodesAssoc
end program Testv050_RMV_NODES_INTERNAL_STATE_ASSOC
