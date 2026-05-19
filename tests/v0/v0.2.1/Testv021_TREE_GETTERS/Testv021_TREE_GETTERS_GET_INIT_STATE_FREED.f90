program Testv021_TREE_GETTERS_GET_INIT_STATE_FREED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getInitStateFreed()
    contains

        !> getInitState must return .false. after build then destroy.
        subroutine getInitStateFreed()
            type(KdTree)   :: t
            real(real64) :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64,             &
                 3.0_real64, 4.0_real64,             &
                 5.0_real64, 6.0_real64], [2, 3])
            logical :: isInit

            call t%build(coords)
            call t%destroy()
            call t%getInitState(isInit)

            if (isInit) then
                write(*, '(A)') '--- Testv021_TREE_GETTERS_GET_INIT_STATE_FREED ---'
                write(*, '(A)') 'expected: isInit = F'
                write(*, '(A)') 'got:      isInit = T'
                stop 1
            end if

        end subroutine getInitStateFreed

end program Testv021_TREE_GETTERS_GET_INIT_STATE_FREED
