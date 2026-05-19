program Testv030_ADD_NODES_IS_MEMBER_ORIGINAL_AFTER_REBUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call addNodesIsMemberOriginalAfterRebuild()
    contains
        !> A node from the old tree is not a member after destroy + rebuild.
        subroutine addNodesIsMemberOriginalAfterRebuild()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)               :: new_coords(2, 2) = reshape( &
                [1.0_real64, 1.0_real64, 4.0_real64, 4.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            n => res(1)%p
            call t%destroy()
            call t%build(coords)
            call t%addNodes(new_coords)

            if (t%isMember(n)) then
                write(*, '(A)') '--- Testv030_ADD_NODES_IS_MEMBER_ORIGINAL_AFTER_REBUILD ---'
                write(*, '(A)') 'old node should not be a member after destroy and rebuild'
                stop 1
            end if
        end subroutine addNodesIsMemberOriginalAfterRebuild
end program Testv030_ADD_NODES_IS_MEMBER_ORIGINAL_AFTER_REBUILD
