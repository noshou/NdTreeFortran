program Testv030_ADD_NODES_LIFECYCLE_DESTROY_REBUILD_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call addNodesLifecycleDestroyRebuildAdd()
    contains
        !> destroy then rebuild then addNodes must succeed and have correct pop.
        subroutine addNodesLifecycleDestroyRebuildAdd()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)   :: new_coords(2, 2) = reshape( &
                [1.0_real64, 1.0_real64, 4.0_real64, 4.0_real64], [2, 2])
            integer(int64) :: pop
            logical        :: isInit

            call t%build(init_coords)
            call t%destroy()
            call t%build(init_coords)
            call t%addNodes(new_coords)

            pop = t%getPop()
            call t%getInitState(isInit)

            if (pop .ne. 5_int64 .or. .not. isInit) then
                write(*, '(A)')         '--- Testv030_ADD_NODES_LIFECYCLE_DESTROY_REBUILD_ADD ---'
                write(*, '(A,I0)')      'expected pop = 5, got: ', pop
                write(*, '(A,L2)')      'expected initialized = T, got: ', isInit
                stop 1
            end if
        end subroutine addNodesLifecycleDestroyRebuildAdd
end program Testv030_ADD_NODES_LIFECYCLE_DESTROY_REBUILD_ADD
