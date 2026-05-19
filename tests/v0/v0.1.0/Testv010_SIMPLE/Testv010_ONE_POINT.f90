program Testv010_ONE_POINT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call onePoint()

    contains

        !> Single point → root only.
        subroutine onePoint()
            type(KdTree)   :: t
            real(real64) :: coords(2, 1) = reshape([1.0_real64, 2.0_real64], [2, 1])
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '[axis=1] (1.000, 2.000)']

            call t%build(coords)
            call t%assert('Testv010_ONE_POINT', expected)
        end subroutine onePoint
end program Testv010_ONE_POINT