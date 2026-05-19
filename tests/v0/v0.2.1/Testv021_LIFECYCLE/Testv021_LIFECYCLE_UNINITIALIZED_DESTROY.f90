program Testv021_LIFECYCLE_UNINITIALIZED_DESTROY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call lifecycleUninitializedDestroy()
    contains 
        
        !> checks if calling destroy on an uninitialized tree
        !! causes no errors state is correct 
        subroutine lifecycleUninitializedDestroy()
            type(KdTree)      :: t
            logical         :: assertNodePool, assertRoot, assertInitState

            call t%destroy()
            call t%associatedNodePool(assertNodePool)
            call t%associatedRoot(assertRoot)
            call t%getInitState(assertInitState)
            
            if (assertNodePool .or. assertRoot .or. assertInitState) then 
                write(*, '(A)')    '--- Testv021_LIFECYCLE_UNINITIALIZED_DESTROY ---'
                write(*, '(A)')    'expected: '
                write(*, '(A)')    '           associated(t%nodePool) = F'
                write(*, '(A)')    '           associated(t%root)     = F'
                write(*, '(A)')    '           t%initialized          = F'
                write(*, '(A)')    'got: '
                write(*, '(A,L2)') '           associated(t%nodePool) = ', assertNodePool
                write(*, '(A,L2)') '           associated(t%root)     = ', assertRoot
                write(*, '(A,L2)') '           t%initialized          = ', assertInitState
            end if

        end subroutine lifecycleUninitializedDestroy
end program Testv021_LIFECYCLE_UNINITIALIZED_DESTROY