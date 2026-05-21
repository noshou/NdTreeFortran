program Testv071_BUILD_DUPLICATES_FOUR_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call duplicatesFourAxis()
    contains
        !> Builds a 4D tree where all 9 input points are identical. Verifies
        !! the build handles full duplicates without infinite recursion and
        !! emits the expected node-multiset.
        subroutine duplicatesFourAxis()
            type(BallTree) :: t
            real(real64) :: coords(4, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, 5.0_real64], [4, 9])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000, 5.000, 5.000, 5.000)',           &
                '  [axis=2] (5.000, 5.000, 5.000, 5.000)',         &
                '    [axis=3] (5.000, 5.000, 5.000, 5.000)',       &
                '    [axis=3] (5.000, 5.000, 5.000, 5.000)',       &
                '      [axis=4] (5.000, 5.000, 5.000, 5.000)',     &
                '  [axis=2] (5.000, 5.000, 5.000, 5.000)',         &
                '    [axis=3] (5.000, 5.000, 5.000, 5.000)',       &
                '    [axis=3] (5.000, 5.000, 5.000, 5.000)',       &
                '      [axis=4] (5.000, 5.000, 5.000, 5.000)']

            call t%build(coords)
            call t%assert('Testv071_BUILD_DUPLICATES_FOUR_AXIS', expected)
        end subroutine duplicatesFourAxis
end program Testv071_BUILD_DUPLICATES_FOUR_AXIS
