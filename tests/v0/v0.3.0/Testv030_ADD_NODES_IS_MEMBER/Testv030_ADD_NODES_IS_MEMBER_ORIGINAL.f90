program Testv030_ADD_NODES_IS_MEMBER_ORIGINAL
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesIsMemberOriginal()
    contains
        !> A node from the original tree must still be a member after addNodes.
        subroutine addNodesIsMemberOriginal()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [5.0_real64, 5.0_real64, 3.0_real64, 7.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n

            call t%build(init_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            n => res(1)%p
            call t%addNodes(new_coords)

            if (.not. t%isMember(n)) then
                write(*, '(A)') '--- Testv030_ADD_NODES_IS_MEMBER_ORIGINAL ---'
                write(*, '(A)') 'original node should still be member after addNodes'
                stop 1
            end if
        end subroutine addNodesIsMemberOriginal
end program Testv030_ADD_NODES_IS_MEMBER_ORIGINAL
