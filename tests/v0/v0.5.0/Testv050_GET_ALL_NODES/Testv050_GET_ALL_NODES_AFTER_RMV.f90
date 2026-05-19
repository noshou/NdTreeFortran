program Testv050_GET_ALL_NODES_AFTER_RMV
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getAllNodesAfterRmv()
    contains
        !> After rmvNodes removes 2 nodes from a 5-node tree,
        !! getAllNodes returns exactly 3 nodes, all members.
        subroutine getAllNodesAfterRmv()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            real(real64)                 :: rmvQuery(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64], [2, 2])
            type(NdNodePtr), allocatable :: nodes(:)
            integer                      :: numRmv, i

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 2) then
                write(*, '(A)')    '--- Testv050_GET_ALL_NODES_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected numRmv=2, got: ', numRmv
                stop 1
            end if

            nodes = t%getAllNodes()

            if (size(nodes) .ne. 3) then
                write(*, '(A)')    '--- Testv050_GET_ALL_NODES_AFTER_RMV ---'
                write(*, '(A,I0)') 'expected 3 nodes after removal, got: ', size(nodes)
                stop 1
            end if

            do i = 1, size(nodes)
                if (.not. t%isMember(nodes(i)%p)) then
                    write(*, '(A)')    '--- Testv050_GET_ALL_NODES_AFTER_RMV ---'
                    write(*, '(A,I0)') 'node ', i, ' not a member after rmvNodes'
                    stop 1
                end if
            end do
        end subroutine getAllNodesAfterRmv
end program Testv050_GET_ALL_NODES_AFTER_RMV
