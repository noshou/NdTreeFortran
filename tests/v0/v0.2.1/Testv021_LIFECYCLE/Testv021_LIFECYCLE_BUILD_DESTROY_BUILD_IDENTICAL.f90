program Testv021_LIFECYCLE_BUILD_DESTROY_BUILD_IDENTICAL
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call lifecycleBuildDestroyBuildIdentical()
    contains 
        
        !> checks internal state of tree after it is built with 
        !! one set of coords, destroyed, then rebuilt with identical set
        subroutine lifecycleBuildDestroyBuildIdentical()
            type(KdTree)      :: t
            real(real64)    :: coords(4, 7) = reshape(                                &
                [3.0_real64, 1.0_real64, -13.31_real64,        0.92_real64,         &
                1.0_real64, 4.0_real64,  34.14_real64,       92.0_real64,          &
                4.0_real64, 1.0_real64, -1093.3139_real64,  312.0_real64,          &
                1.0_real64, 5.0_real64,  0.013_real64,    13112.0_real64,          &
                5.0_real64, 9.0_real64,  33.13_real64,       -5.13192_real64,      &
                9.0_real64, 2.0_real64,  734.348_real64,      0.00000092_real64,   &
                2.0_real64, 6.0_real64, -93.131_real64,       0.9412412_real64], [4, 7])
            logical         :: assertNodePool, assertRoot, assertInitState

            call t%build(coords)
            call t%destroy()
            call t%build(coords)

            call t%associatedNodePool(assertNodePool)
            call t%associatedRoot(assertRoot)
            call t%getInitState(assertInitState)

            if (.not.(assertNodePool .and. assertRoot .and. assertInitState)) then 
                write(*, '(A)')    '--- Testv021_LIFECYCLE_BUILD_DESTROY_BUILD_IDENTICAL ---'
                write(*, '(A)')    'expected: '
                write(*, '(A)')    '           associated(t%nodePool) = T'
                write(*, '(A)')    '           associated(t%root)     = T'
                write(*, '(A)')    '           t%initialized          = T'
                write(*, '(A)')    'got: '
                write(*, '(A,L2)') '           associated(t%nodePool) = ', assertNodePool
                write(*, '(A,L2)') '           associated(t%root)     = ', assertRoot
                write(*, '(A,L2)') '           t%initialized          = ', assertInitState
            end if

        end subroutine lifecycleBuildDestroyBuildIdentical
end program Testv021_LIFECYCLE_BUILD_DESTROY_BUILD_IDENTICAL