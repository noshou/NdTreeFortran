program Testv021_LIFECYCLE_UNINITIALIZED
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call lifecycleUninitialized()
    contains 
        
        !> checks if uninitialized state is correct 
        subroutine lifecycleUninitialized()
            type(KdTree)      :: t
            logical         :: assertNodePool, assertRoot, assertInitState

            call t%associatedNodePool(assertNodePool)
            call t%associatedRoot(assertRoot)
            call t%getInitState(assertInitState)
            
            if (assertNodePool .or. assertRoot .or. assertInitState) then 
                write(*, '(A)')    '--- Testv021_LIFECYCLE_UNINITIALIZED ---'
                write(*, '(A)')    'expected: '
                write(*, '(A)')    '           associated(t%nodePool) = F'
                write(*, '(A)')    '           associated(t%root)     = F'
                write(*, '(A)')    '           t%initialized          = F'
                write(*, '(A)')    'got: '
                write(*, '(A,L2)') '           associated(t%nodePool) = ', assertNodePool
                write(*, '(A,L2)') '           associated(t%root)     = ', assertRoot
                write(*, '(A,L2)') '           t%initialized          = ', assertInitState
            end if

        end subroutine lifecycleUninitialized
end program Testv021_LIFECYCLE_UNINITIALIZED