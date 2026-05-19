program Testv010_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call empty()

    contains

        !> Empty input → empty-tree marker.
        subroutine empty()
            type(KdTree)   :: t
            real(real64) :: coords(2, 0)
            character(len=*), parameter :: expected(*) = [character(len=64) :: &
                '**empty tree**']

            call t%build(coords)
            call t%assert('Testv010_EMPTY', expected)
        end subroutine empty
end program Testv010_EMPTY