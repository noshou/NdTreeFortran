program Testv030_ADD_NODES_ZERO_ADD_RNN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call zeroAddRnn()
    contains
        !> After a zero-column addNodes, rNN_Centroid still finds exactly the original nodes.
        subroutine zeroAddRnn()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)               :: zero_coords(2, 0)
            type(NdNodePtr), allocatable :: res(:)

            call t%build(init_coords)
            call t%addNodes(zero_coords)

            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='euclidean')
            if (size(res) .ne. 3) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_ZERO_ADD_RNN ---'
                write(*, '(A,I0)') 'expected 3 original nodes findable after zero-add, got: ', size(res)
                stop 1
            end if
        end subroutine zeroAddRnn
end program Testv030_ADD_NODES_ZERO_ADD_RNN
