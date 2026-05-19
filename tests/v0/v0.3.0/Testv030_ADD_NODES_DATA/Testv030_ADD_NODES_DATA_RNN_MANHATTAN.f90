program Testv030_ADD_NODES_DATA_RNN_MANHATTAN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesDataRnnManhattan()
    contains
        !> Build 3 pts with data far from origin. addNodes 2 near-origin pts with data ['D','E'].
        !! rNN_Centroid at origin r=1.5 (manhattan) returns the 2 added nodes with correct data.
        subroutine addNodesDataRnnManhattan()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 3) = reshape( &
                [50.0_real64, 50.0_real64, -50.0_real64, 50.0_real64, 50.0_real64, -50.0_real64], [2, 3])
            character(len=1)           :: init_data(3) = ['A', 'B', 'C']
            real(real64)               :: new_coords(2, 2) = reshape( &
                [1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 2])
            character(len=1)           :: new_data(2) = ['D', 'E']
            type(NdNodePtr), allocatable :: res(:)
            integer                    :: i
            logical                    :: foundD = .false., foundE = .false.

            call t%build(init_coords, init_data)
            call t%addNodes(new_coords, new_data)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 1.5_real64, metric='manhattan')

            if (size(res) .ne. 2) then
                write(*, '(A)')   '--- Testv030_ADD_NODES_DATA_RNN_MANHATTAN ---'
                write(*, '(A,I0)') 'expected 2 nodes near origin, got: ', size(res)
                stop 1
            end if
            do i = 1, size(res)
                select type (d => res(i)%p%getData())
                    type is (character(*))
                        if (d .eq. 'D')      foundD = .true.
                        if (d .eq. 'E')      foundE = .true.
                        if (d .ne. 'D' .and. d .ne. 'E') then
                            write(*, '(A)')  '--- Testv030_ADD_NODES_DATA_RNN_MANHATTAN ---'
                            write(*, '(A,A)') 'unexpected data: ', d
                            stop 1
                        end if
                end select
            end do
            if (.not. (foundD .and. foundE)) then
                write(*, '(A)')  '--- Testv030_ADD_NODES_DATA_RNN_MANHATTAN ---'
                write(*, '(A,L2,A,L2)') 'foundD=', foundD, ', foundE=', foundE
                stop 1
            end if
        end subroutine addNodesDataRnnManhattan
end program Testv030_ADD_NODES_DATA_RNN_MANHATTAN
