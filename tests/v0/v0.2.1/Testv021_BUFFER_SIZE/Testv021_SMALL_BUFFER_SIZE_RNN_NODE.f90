program Testv021_SMALL_BUFFER_SIZE_RNN_NODE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call smallBufferSize_rNN_Node()
    contains

        !> bufferSize=1 forces the internal array to double four times for 10 results.
        subroutine smallBufferSize_rNN_Node()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 10) = reshape( &
                [0.1_real64, 0.0_real64, 0.0_real64, &
                 0.2_real64, 0.0_real64, 0.0_real64, &
                 0.3_real64, 0.0_real64, 0.0_real64, &
                 0.4_real64, 0.0_real64, 0.0_real64, &
                 0.5_real64, 0.0_real64, 0.0_real64, &
                 0.0_real64, 0.1_real64, 0.0_real64, &
                 0.0_real64, 0.2_real64, 0.0_real64, &
                 0.0_real64, 0.3_real64, 0.0_real64, &
                 0.0_real64, 0.4_real64, 0.0_real64, &
                 0.0_real64, 0.5_real64, 0.0_real64], [3, 10])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)
            integer                    :: i

            call t%build(coords)
            centroid_res = t%rNN_Centroid([0.1_real64, 0.0_real64, 0.0_real64], 0.0_real64)

            do i = 1, 1000
                res = t%rNN_Node(centroid_res(1), 1.0_real64, bufferSize=1_int64)
                if (size(res) .ne. 10) then
                    write(*, '(A)') '--- smallBufferSize_rNN_Node ---'
                    write(*, *) 'expected 10 nodes, got:', size(res)
                    stop 1
                end if
            end do

        end subroutine smallBufferSize_rNN_Node

end program Testv021_SMALL_BUFFER_SIZE_RNN_NODE
