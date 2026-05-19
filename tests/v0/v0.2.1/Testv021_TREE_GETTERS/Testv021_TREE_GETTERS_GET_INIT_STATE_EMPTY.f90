program Testv021_TREE_GETTERS_GET_INIT_STATE_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getInitStateEmpty()
    contains

        !> getInitState must return .true. after build with zero points.
        subroutine getInitStateEmpty()
            type(KdTree)   :: t
            real(real64) :: coords(3, 0)
            logical      :: isInit

            call t%build(coords)
            call t%getInitState(isInit)

            if (.not. isInit) then
                write(*, '(A)') '--- Testv021_TREE_GETTERS_GET_INIT_STATE_EMPTY ---'
                write(*, '(A)') 'expected: isInit = T'
                write(*, '(A)') 'got:      isInit = F'
                stop 1
            end if

        end subroutine getInitStateEmpty

end program Testv021_TREE_GETTERS_GET_INIT_STATE_EMPTY
