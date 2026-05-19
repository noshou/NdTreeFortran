program Testv010_TWO_POINTS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call twoPoints()
    contains

        !> Two points → root + one child.
        subroutine twoPoints()
            type(KdTree)   :: t
            real(real64) :: coords(2, 2) = reshape( &
                [1.0_real64, 5.0_real64,    &
                3.0_real64, 2.0_real64], [2, 2])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (1.000, 5.000)',   &
                '  [axis=2] (3.000, 2.000)']

            call t%build(coords)
            call t%assert('Testv010_TWO_POINTS', expected)
        end subroutine twoPoints
end program Testv010_TWO_POINTS