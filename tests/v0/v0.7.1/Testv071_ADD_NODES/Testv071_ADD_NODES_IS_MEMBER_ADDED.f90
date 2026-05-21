program Testv071_ADD_NODES_IS_MEMBER_ADDED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesIsMemberAdded()
    contains
        !> A node returned by rNN after addNodes must be a member.
        subroutine addNodesIsMemberAdded()
            type(BallTree)               :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 50.0_real64, &
                50.0_real64, 60.0_real64, 60.0_real64, 60.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n

            call t%build(init_coords)
            call t%addNodes(new_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)

            if (size(res) .ne. 1) then
                write(*, '(A)')   '--- Testv071_ADD_NODES_IS_MEMBER_ADDED ---'
                write(*, '(A,I0)') 'expected 1 node near origin, got: ', size(res)
                stop 1
            end if
            n => res(1)%p
            if (.not. t%isMember(n)) then
                write(*, '(A)') '--- Testv071_ADD_NODES_IS_MEMBER_ADDED ---'
                write(*, '(A)') 'added node should be a member'
                stop 1
            end if
        end subroutine addNodesIsMemberAdded
end program Testv071_ADD_NODES_IS_MEMBER_ADDED
