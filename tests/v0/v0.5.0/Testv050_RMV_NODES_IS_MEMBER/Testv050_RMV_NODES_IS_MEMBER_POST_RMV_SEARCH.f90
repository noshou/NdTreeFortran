program Testv050_RMV_NODES_IS_MEMBER_POST_RMV_SEARCH
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesIsMemberPostRmvSearch()
    contains
        !> After rmvNodes removes one node, find a surviving node via rNN_Centroid.
        !! The returned NdNodePtr has pool_idx pointing to the current pool position,
        !! so isMember takes the O(1) fast path and returns true.
        !!
        !! This is distinct from IS_MEMBER_SURVIVING which finds the node BEFORE
        !! removal and exercises the O(n) fallback.
        subroutine rmvNodesIsMemberPostRmvSearch()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_POST_RMV_SEARCH ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            ! find a survivor AFTER the removal -> numRemovesSnapshot will equal
            ! current numRemoves, so isMember fast path fires
            res = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_POST_RMV_SEARCH ---'
                write(*, '(A,I0)') 'expected 1 node at (5,0) after removal, got: ', size(res)
                stop 1
            end if
            n => res(1)%p

            if (.not. t%isMember(n)) then
                write(*, '(A)') '--- Testv050_RMV_NODES_IS_MEMBER_POST_RMV_SEARCH ---'
                write(*, '(A)') 'surviving node found after removal must be a member (fast path)'
                stop 1
            end if
        end subroutine rmvNodesIsMemberPostRmvSearch
end program Testv050_RMV_NODES_IS_MEMBER_POST_RMV_SEARCH
