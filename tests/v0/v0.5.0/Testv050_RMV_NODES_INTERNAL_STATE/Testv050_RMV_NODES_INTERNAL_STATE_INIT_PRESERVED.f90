program Testv050_RMV_NODES_INTERNAL_STATE_INIT_PRESERVED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesInitPreserved()
    contains
        !> Tree remains initialized (isInit=T) after rmvNodes.
        subroutine rmvNodesInitPreserved()
            type(KdTree) :: t
            real(real64) :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 2.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 4])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer      :: numRmv
            logical      :: isInit

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            call t%getInitState(isInit)

            if (.not. isInit) then
                write(*, '(A)') '--- Testv050_RMV_NODES_INTERNAL_STATE_INIT_PRESERVED ---'
                write(*, '(A)') 'expected isInit=T after rmvNodes'
                stop 1
            end if
        end subroutine rmvNodesInitPreserved
end program Testv050_RMV_NODES_INTERNAL_STATE_INIT_PRESERVED
