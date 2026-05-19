program Testv050_RMV_NODES_IS_MEMBER_AFTER_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesIsMemberAfterAdd()
    contains
        !> After rmvNodes and then addNodes, a node found by rNN must be a member.
        !! Verifies that the tree remains usable for isMember after a removal cycle.
        subroutine rmvNodesIsMemberAfterAdd()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                 :: newCoords(2, 1) = reshape([9.0_real64, 9.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_AFTER_ADD ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            call t%addNodes(newCoords)
            res = t%rNN_Centroid([9.0_real64, 9.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_AFTER_ADD ---'
                write(*, '(A,I0)') 'expected 1 node near (9,9) after addNodes, got: ', size(res)
                stop 1
            end if
            n => res(1)%p

            if (.not. t%isMember(n)) then
                write(*, '(A)') '--- Testv050_RMV_NODES_IS_MEMBER_AFTER_ADD ---'
                write(*, '(A)') 'newly added node should be a member after rmvNodes+addNodes cycle'
                stop 1
            end if
        end subroutine rmvNodesIsMemberAfterAdd
end program Testv050_RMV_NODES_IS_MEMBER_AFTER_ADD
