program Testv050_GET_ALL_NODES_IS_MEMBER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call getAllNodesIsMember()
    contains
        !> Every node returned by getAllNodes must be a member of the tree.
        subroutine getAllNodesIsMember()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 4])
            type(NdNodePtr), allocatable :: nodes(:)
            integer                      :: i

            call t%build(coords)
            nodes = t%getAllNodes()

            do i = 1, size(nodes)
                if (.not. t%isMember(nodes(i)%p)) then
                    write(*, '(A)')    '--- Testv050_GET_ALL_NODES_IS_MEMBER ---'
                    write(*, '(A,I0)') 'node ', i, ' returned by getAllNodes is not a member'
                    stop 1
                end if
            end do
        end subroutine getAllNodesIsMember
end program Testv050_GET_ALL_NODES_IS_MEMBER
