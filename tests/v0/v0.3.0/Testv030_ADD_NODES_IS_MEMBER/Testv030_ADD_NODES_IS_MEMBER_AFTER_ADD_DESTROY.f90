program Testv030_ADD_NODES_IS_MEMBER_AFTER_ADD_DESTROY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesIsMemberAfterAddDestroy()
    contains
        !> A node from after addNodes must not be a member once the tree is destroyed.
        subroutine addNodesIsMemberAfterAddDestroy()
            type(KdTree)                 :: t
            real(real64)               :: init_coords(2, 4) = reshape( &
                [50.0_real64, 50.0_real64, 60.0_real64, 50.0_real64, &
                50.0_real64, 60.0_real64, 60.0_real64, 60.0_real64], [2, 4])
            real(real64)               :: new_coords(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n

            call t%build(init_coords)
            call t%addNodes(new_coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            n => res(1)%p
            call t%destroy()

            if (t%isMember(n)) then
                write(*, '(A)') '--- Testv030_ADD_NODES_IS_MEMBER_AFTER_ADD_DESTROY ---'
                write(*, '(A)') 'node should not be a member after tree destroyed'
                stop 1
            end if
        end subroutine addNodesIsMemberAfterAddDestroy
end program Testv030_ADD_NODES_IS_MEMBER_AFTER_ADD_DESTROY
