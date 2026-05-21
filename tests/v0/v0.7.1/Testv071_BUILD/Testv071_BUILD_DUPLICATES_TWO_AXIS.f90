program Testv071_BUILD_DUPLICATES_TWO_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call duplicatesTwoAxis()

    contains

        !> Builds a 2D tree where all 9 input points are identical. Verifies
        !! the build handles full duplicates without infinite recursion and
        !! emits the expected node-multiset.
        subroutine duplicatesTwoAxis()
            type(BallTree) :: t
            real(real64) :: coords(2, 9) = reshape( &
                [5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64], [2, 9])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000, 5.000)',         &
                '  [axis=2] (5.000, 5.000)',       &
                '    [axis=1] (5.000, 5.000)',     &
                '    [axis=1] (5.000, 5.000)',     &
                '      [axis=2] (5.000, 5.000)',   &
                '  [axis=2] (5.000, 5.000)',       &
                '    [axis=1] (5.000, 5.000)',     &
                '    [axis=1] (5.000, 5.000)',     &
                '      [axis=2] (5.000, 5.000)']

            call t%build(coords)
            call t%assert('Testv071_BUILD_DUPLICATES_TWO_AXIS', expected)
        end subroutine duplicatesTwoAxis
end program Testv071_BUILD_DUPLICATES_TWO_AXIS
