program Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesOnEmptyTree()
    contains
        !> After removing all nodes (pop=0, initialized=T), all 5 rmvNodes search
        !! branches must return numRmv=0 and not crash:
        !!   1. coords only          (rNN_Coords branch)
        !!   2. ids only             (linScan branch)
        !!   3. coords + ids         (rNN_Ids branch)
        !!   4. coords + radii       (rNN_Rad branch)
        !!   5. coords + radii + ids (rNN_RadIds branch)
        subroutine rmvNodesOnEmptyTree()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64)                 :: query(2, 1)  = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                 :: radii(1)     = [100.0_real64]
            type(NodeId)                 :: ids(1)
            type(NdNodePtr), allocatable :: res(:)
            integer                      :: numRmv
            integer(int64)               :: pop

            ! drain the tree
            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            if (numRmv .ne. 3 .or. t%getPop() .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0,A,I0)') 'setup: expected numRmv=3,pop=0; got numRmv=', numRmv, ',pop=', t%getPop()
                stop 1
            end if

            ! branch 1: coords only
            numRmv = t%rmvNodes(coordsList=query)
            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'branch1 coords-only: expected numRmv=0 on empty tree, got: ', numRmv
                stop 1
            end if

            ! branch 2: ids only
            numRmv = t%rmvNodes(ids=ids)
            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'branch2 ids-only: expected numRmv=0 on empty tree, got: ', numRmv
                stop 1
            end if

            ! branch 3: coords + ids (no radii; must have same size)
            numRmv = t%rmvNodes(coordsList=query, ids=ids)
            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'branch3 coords+ids: expected numRmv=0 on empty tree, got: ', numRmv
                stop 1
            end if

            ! branch 4: coords + radii
            numRmv = t%rmvNodes(coordsList=query, radii=radii)
            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'branch4 coords+radii: expected numRmv=0 on empty tree, got: ', numRmv
                stop 1
            end if

            ! branch 5: coords + radii + ids
            numRmv = t%rmvNodes(coordsList=query, radii=radii, ids=ids)
            if (numRmv .ne. 0) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'branch5 coords+radii+ids: expected numRmv=0 on empty tree, got: ', numRmv
                stop 1
            end if

            pop = t%getPop()
            if (pop .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY ---'
                write(*, '(A,I0)') 'expected pop=0 after all empty-tree rmvNodes calls, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesOnEmptyTree
end program Testv050_RMV_NODES_LIFECYCLE_RMV_ON_EMPTY
