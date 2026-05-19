program Testv010_DUPLICATES_THREE_AXIS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call duplicatesThreeAxis()
    contains 
        !> Builds a 3D tree where all 9 input points are identical. Verifies
        !! the build handles full duplicates without infinite recursion and
        !! emits the expected node-multiset.
        subroutine duplicatesThreeAxis()
            type(KdTree)   :: t
            real(real64) :: coords(3, 9) = reshape( &
                [5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64, &
                5.0_real64, 5.0_real64, 5.0_real64], [3, 9])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (5.000, 5.000, 5.000)',          &
                '  [axis=2] (5.000, 5.000, 5.000)',        &
                '    [axis=3] (5.000, 5.000, 5.000)',      &
                '    [axis=3] (5.000, 5.000, 5.000)',      &
                '      [axis=1] (5.000, 5.000, 5.000)',    &
                '  [axis=2] (5.000, 5.000, 5.000)',        &
                '    [axis=3] (5.000, 5.000, 5.000)',      &
                '    [axis=3] (5.000, 5.000, 5.000)',      &
                '      [axis=1] (5.000, 5.000, 5.000)']

            call t%build(coords)
            call t%assert('Testv010_DUPLICATES_THREE_AXIS', expected)
        end subroutine duplicatesThreeAxis
end program Testv010_DUPLICATES_THREE_AXIS