program Testv071_BUILD_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call empty()

    contains

        !> Empty input → empty-tree marker.
        subroutine empty()
            type(BallTree) :: t
            real(real64) :: coords(2, 0)
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '**empty tree**']

            call t%build(coords)
            call t%assert('Testv071_BUILD_EMPTY', expected)
        end subroutine empty
end program Testv071_BUILD_EMPTY
