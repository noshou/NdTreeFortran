!> BallTree rNN_Centroid: 1000 random 3-D points; 1000 queries with random centroids.
!! Result count is verified against a brute-force euclidean scan each iteration.
program Testv071_RNN_BUFFER_LARGE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        !> 1000 random 3-D points; 1000 queries with random centroids.
        !! Result count is verified against a brute-force euclidean scan each iteration.
        subroutine run()
            integer, parameter         :: N = 1000, NDIM = 3, NITER = 1000
            type(BallTree)               :: t
            real(real64)               :: coords(NDIM, N), centroid(NDIM)
            type(NdNodePtr), allocatable :: res(:)
            real(real64)               :: d, r
            integer                    :: i, j, expected

            call random_number(coords)
            coords = coords * 100.0_real64
            call t%build(coords)

            r = 20.0_real64

            do i = 1, NITER
                call random_number(centroid)
                centroid = centroid * 100.0_real64

                res = t%rNN_Centroid(centroid, r, bufferSize=1000000_int64)

                expected = 0
                do j = 1, N
                    d = sqrt(sum((centroid - coords(:, j))**2))
                    if (d .le. r) expected = expected + 1
                end do

                if (size(res) .ne. expected) then
                    write(*, '(A)') '--- Testv071_RNN_BUFFER_LARGE ---'
                    write(*, *) 'iteration:', i, '  expected:', expected, '  got:', size(res)
                    stop 1
                end if
            end do

        end subroutine run

end program Testv071_RNN_BUFFER_LARGE
