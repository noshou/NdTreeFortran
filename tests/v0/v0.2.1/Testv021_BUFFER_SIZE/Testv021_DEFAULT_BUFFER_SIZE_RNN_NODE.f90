program Testv021_DEFAULT_BUFFER_SIZE_RNN_NODE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call defaultBufferSize_rNN_Node()
    contains

        subroutine defaultBufferSize_rNN_Node()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                 4.0_real64, 2.0_real64,  0.42_real64,             &
                 3.0_real64, 3.0_real64,  0.00003_real64,          &
                 0.0_real64, 0.0_real64,  0.00000031_real64,       &
                 1.0_real64, 5.0_real64, -93131913.0_real64,       &
                 0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:), centroid_res(:)
            integer                    :: i

            call t%build(coords)
            centroid_res = t%rNN_Centroid([5.0_real64, 1.0_real64, 0.92_real64], 0.0_real64)

            do i = 1, 1000
                res = t%rNN_Node(centroid_res(1), 4.0_real64)
                if (size(res) .ne. 3) then
                    write(*, '(A)') '--- defaultBufferSize_rNN_Node ---'
                    write(*, *) 'expected 3 nodes, got:', size(res)
                    stop 1
                end if
            end do

        end subroutine defaultBufferSize_rNN_Node

end program Testv021_DEFAULT_BUFFER_SIZE_RNN_NODE
