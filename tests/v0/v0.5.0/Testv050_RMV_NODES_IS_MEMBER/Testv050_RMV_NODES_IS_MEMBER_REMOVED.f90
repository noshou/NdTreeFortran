program Testv050_RMV_NODES_IS_MEMBER_REMOVED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesIsMemberRemoved()
    contains
        !> A node obtained via rNN_Centroid before rmvNodes removes it
        !! must return isMember=false after removal.
        subroutine rmvNodesIsMemberRemoved()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                 :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: n
            integer                      :: numRmv

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_REMOVED ---'
                write(*, '(A,I0)') 'expected 1 node at origin before removal, got: ', size(res)
                stop 1
            end if
            n => res(1)%p

            numRmv = t%rmvNodes(coordsList=query)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_REMOVED ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            if (t%isMember(n)) then
                write(*, '(A)') '--- Testv050_RMV_NODES_IS_MEMBER_REMOVED ---'
                write(*, '(A)') 'removed node should not be a member'
                stop 1
            end if
        end subroutine rmvNodesIsMemberRemoved
end program Testv050_RMV_NODES_IS_MEMBER_REMOVED
