program Testv050_RMV_NODES_IS_MEMBER_SURVIVING
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesIsMemberSurviving()
    contains
        !> After rmvNodes removes one node, a reference to a surviving node
        !! obtained before the removal must still return isMember=true.
        subroutine rmvNodesIsMemberSurviving()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            type(NdNodePtr), allocatable :: res(:)
            type(NdNode),    pointer     :: surviving
            integer                      :: numRmv

            call t%build(coords)
            res = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
            if (size(res) .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_SURVIVING ---'
                write(*, '(A,I0)') 'expected 1 node at (5,0) before removal, got: ', size(res)
                stop 1
            end if
            surviving => res(1)%p

            numRmv = t%rmvNodes(coordsList=rmvQuery)
            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_IS_MEMBER_SURVIVING ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if

            if (.not. t%isMember(surviving)) then
                write(*, '(A)') '--- Testv050_RMV_NODES_IS_MEMBER_SURVIVING ---'
                write(*, '(A)') 'surviving node should still be a member after partial removal'
                stop 1
            end if
        end subroutine rmvNodesIsMemberSurviving
end program Testv050_RMV_NODES_IS_MEMBER_SURVIVING
