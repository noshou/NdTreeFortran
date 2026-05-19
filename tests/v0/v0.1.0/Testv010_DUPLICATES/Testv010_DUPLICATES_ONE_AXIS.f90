program Testv010_DUPLICATES_ONE_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call duplicatesOneAxis()
    contains 
        !> Builds a 1D tree where all 9 input points are identical, the
        !! pathological all-ties case. Verifies the build handles it without
        !! infinite recursion and emits the expected node-multiset.
        subroutine duplicatesOneAxis()
            type(KdTree)   :: t
            real(real64) :: coords(1, 9) = reshape( &
                [5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64, &
                5.0_real64], [1, 9])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000)',           &
                '  [axis=1] (5.000)',         &
                '    [axis=1] (5.000)',       &
                '    [axis=1] (5.000)',       &
                '      [axis=1] (5.000)',     &
                '  [axis=1] (5.000)',         &
                '    [axis=1] (5.000)',       &
                '    [axis=1] (5.000)',       &
                '      [axis=1] (5.000)']

            call t%build(coords)
            call t%assert('Testv010_DUPLICATES_ONE_AXIS', expected)
        end subroutine duplicatesOneAxis
end program Testv010_DUPLICATES_ONE_AXIS