!> BallTree rNN_Centroid with the default result buffer size, repeated 1000 times.
program Testv071_RNN_BUFFER_DEFAULT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        subroutine run()
            type(BallTree)               :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                 4.0_real64, 2.0_real64,  0.42_real64,             &
                 3.0_real64, 3.0_real64,  0.00003_real64,          &
                 0.0_real64, 0.0_real64,  0.00000031_real64,       &
                 1.0_real64, 5.0_real64, -93131913.0_real64,       &
                 0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            real(real64)               :: centroid(3) = [2.5_real64, 2.5_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)
            integer                    :: i

            call t%build(coords)

            do i = 1, 1000
                res = t%rNN_Centroid(centroid, 4.0_real64)
                if (size(res) .ne. 5) then
                    write(*, '(A)') '--- Testv071_RNN_BUFFER_DEFAULT ---'
                    write(*, *) 'expected 5 nodes, got:', size(res)
                    stop 1
                end if
            end do

        end subroutine run

end program Testv071_RNN_BUFFER_DEFAULT
