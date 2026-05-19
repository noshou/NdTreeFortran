program Testv050_RMV_NODES_NO_ARGS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesNoArgs()
    contains
        !> rmvNodes with no optional args must error stop: must supply ids or coordsList.
        subroutine rmvNodesNoArgs()
            type(KdTree) :: t
            real(real64) :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            integer      :: numRmv
            call t%build(coords)
            numRmv = t%rmvNodes()
            write(*, '(A)') '--- Testv050_RMV_NODES_NO_ARGS ---'
            write(*, '(A)') 'expected error stop for no args, but returned normally'
        end subroutine rmvNodesNoArgs
end program Testv050_RMV_NODES_NO_ARGS
