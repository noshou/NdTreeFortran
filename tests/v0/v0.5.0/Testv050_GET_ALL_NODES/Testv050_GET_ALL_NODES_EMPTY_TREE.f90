program Testv050_GET_ALL_NODES_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getAllNodesEmptyTree()
    contains
        !> After rmvNodes empties the tree (pop=0, initialized=T),
        !! getAllNodes returns a zero-length array without error.
        subroutine getAllNodesEmptyTree()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                 :: query(2, 1)  = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                 :: radii(1)     = [100.0_real64]
            type(NdNodePtr), allocatable :: nodes(:)
            integer                      :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            nodes  = t%getAllNodes()

            if (size(nodes) .ne. 0) then
                write(*, '(A)')    '--- Testv050_GET_ALL_NODES_EMPTY_TREE ---'
                write(*, '(A,I0)') 'expected size=0 on empty tree, got: ', size(nodes)
                stop 1
            end if
        end subroutine getAllNodesEmptyTree
end program Testv050_GET_ALL_NODES_EMPTY_TREE
