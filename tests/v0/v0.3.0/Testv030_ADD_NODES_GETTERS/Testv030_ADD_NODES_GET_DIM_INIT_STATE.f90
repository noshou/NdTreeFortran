program Testv030_ADD_NODES_GET_DIM_INIT_STATE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call addNodesGetDimInitState()
    contains
        !> getDim and getInitState must remain unchanged after addNodes.
        subroutine addNodesGetDimInitState()
            type(KdTree)     :: t
            real(real64)   :: init_coords(3, 4) = reshape( &
                [0.0_real64, 0.0_real64, 0.0_real64, &
                1.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 1.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 1.0_real64], [3, 4])
            real(real64)   :: new_coords(3, 2) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, &
                6.0_real64, 6.0_real64, 6.0_real64], [3, 2])
            integer(int64) :: dim
            logical        :: isInit

            call t%build(init_coords)
            call t%addNodes(new_coords)
            dim = t%getDim()
            call t%getInitState(isInit)

            if (dim .ne. 3_int64 .or. .not. isInit) then
                write(*, '(A)')         '--- Testv030_ADD_NODES_GET_DIM_INIT_STATE ---'
                write(*, '(A,I0)')      'expected dim = 3, got: ', dim
                write(*, '(A,L2)')      'expected initialized = T, got: ', isInit
                stop 1
            end if
        end subroutine addNodesGetDimInitState
end program Testv030_ADD_NODES_GET_DIM_INIT_STATE
