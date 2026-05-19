program Testv021_LARGE_BUFFER_SIZE_RNN_NODE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call largeBufferSize_rNN_Node()
    contains

        !> 1000 random 3-D points; 1000 queries with random radii against the first node.
        !! Result count is verified against a brute-force euclidean scan each iteration.
        subroutine largeBufferSize_rNN_Node()
            integer, parameter         :: N = 1000, NDIM = 3, NITER = 1000
            type(KdTree)                 :: t
            real(real64)               :: coords(NDIM, N), target_coords(NDIM)
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)
            real(real64)               :: d, r
            integer                    :: i, j, expected

            call random_number(coords)
            coords = coords * 100.0_real64
            call t%build(coords)

            target_coords = coords(:, 1)
            centroid_res = t%rNN_Centroid(target_coords, 0.0_real64)

            do i = 1, NITER
                call random_number(r)
                r = r * 40.0_real64

                res = t%rNN_Node(centroid_res(1), r, bufferSize=1000000)

                expected = 0
                do j = 1, N
                    d = sqrt(sum((target_coords - coords(:, j))**2))
                    if (d .le. r) expected = expected + 1
                end do

                if (size(res) .ne. expected) then
                    write(*, '(A)') '--- largeBufferSize_rNN_Node ---'
                    write(*, *) 'iteration:', i, '  r:', r, '  expected:', expected, '  got:', size(res)
                    stop 1
                end if
            end do

        end subroutine largeBufferSize_rNN_Node

end program Testv021_LARGE_BUFFER_SIZE_RNN_NODE
