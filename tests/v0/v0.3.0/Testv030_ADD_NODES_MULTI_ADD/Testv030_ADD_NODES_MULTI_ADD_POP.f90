program Testv030_ADD_NODES_MULTI_ADD_POP
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call multiAddPop()
    contains
        !> Three consecutive addNodes calls; final pop must equal initial + sum of added.
        subroutine multiAddPop()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)   :: batch1(2, 3) = reshape( &
                [1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, 3.0_real64, 0.0_real64], [2, 3])
            real(real64)   :: batch2(2, 3) = reshape( &
                [4.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 6.0_real64, 0.0_real64], [2, 3])
            real(real64)   :: batch3(2, 3) = reshape( &
                [7.0_real64, 0.0_real64, 8.0_real64, 0.0_real64, 9.0_real64, 0.0_real64], [2, 3])
            integer(int64) :: pop

            call t%build(init_coords)
            call t%addNodes(batch1)
            call t%addNodes(batch2)
            call t%addNodes(batch3)
            pop = t%getPop()

            if (pop .ne. 10_int64) then
                write(*, '(A)')         '--- Testv030_ADD_NODES_MULTI_ADD_POP ---'
                write(*, '(A,I0,A,I0)') 'expected pop = 10, got: ', pop
                stop 1
            end if
        end subroutine multiAddPop
end program Testv030_ADD_NODES_MULTI_ADD_POP
