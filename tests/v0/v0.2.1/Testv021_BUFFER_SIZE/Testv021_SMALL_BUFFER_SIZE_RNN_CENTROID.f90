program Testv021_SMALL_BUFFER_SIZE_RNN_CENTROID
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call smallBufferSize_rNN_Centroid()
    contains

        !> bufferSize=1 forces the internal array to double four times for 10 results.
        subroutine smallBufferSize_rNN_Centroid()
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
            real(real64)               :: centroid(3) = [0.0_real64, 0.0_real64, 0.0_real64]
            type(NdNodePtr), allocatable :: res(:)
            integer                    :: i

            call t%build(coords)

            do i = 1, 1000
                res = t%rNN_Centroid(centroid, 1.0_real64, bufferSize=1_int64)
                if (size(res) .ne. 10) then
                    write(*, '(A)') '--- smallBufferSize_rNN_Centroid ---'
                    write(*, *) 'expected 10 nodes, got:', size(res)
                    stop 1
                end if
            end do

        end subroutine smallBufferSize_rNN_Centroid

end program Testv021_SMALL_BUFFER_SIZE_RNN_CENTROID
