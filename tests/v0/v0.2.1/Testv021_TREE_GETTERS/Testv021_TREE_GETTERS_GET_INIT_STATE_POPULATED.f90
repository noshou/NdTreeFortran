program Testv021_TREE_GETTERS_GET_INIT_STATE_POPULATED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getInitStatePopulated()
    contains

        !> getInitState must return .true. after a populated build.
        subroutine getInitStatePopulated()
            type(KdTree)   :: t
            real(real64) :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64,             &
                 3.0_real64, 4.0_real64,             &
                 5.0_real64, 6.0_real64], [2, 3])
            logical :: isInit

            call t%build(coords)
            call t%getInitState(isInit)

            if (.not. isInit) then
                write(*, '(A)') '--- Testv021_TREE_GETTERS_GET_INIT_STATE_POPULATED ---'
                write(*, '(A)') 'expected: isInit = T'
                write(*, '(A)') 'got:      isInit = F'
                stop 1
            end if

        end subroutine getInitStatePopulated

end program Testv021_TREE_GETTERS_GET_INIT_STATE_POPULATED
