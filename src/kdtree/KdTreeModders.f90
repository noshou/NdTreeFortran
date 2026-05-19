submodule(NdTreeFortran) KdTreeModders
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure addNodesKDT

            logical               :: hasData
            integer(int64)        :: i, dim, pop, numNodeToAdd, tid, currIdx, currAxis
            type(NdNode), pointer :: nodePoolTmp(:)

            hasData      = present(dataList)
            numNodeToAdd = size(coordsList, 2)
            dim          = size(coordsList, 1)

            ! realloc nodePool ; serialized: nodePool pointer, pop,
            ! and currNodeId are all shared state
            !$OMP CRITICAL (tree_mutate)
            pop = numNodeToAdd + this%pop
            tid = this%getTreeId()
            allocate(nodePoolTmp(pop))
            if (this%pop .gt. 0_int64) nodePoolTmp(1:this%pop) = this%nodePool(1:this%pop)
            do i = this%pop + 1, pop
                allocate(nodePoolTmp(i)%coords(dim))
                !$OMP ATOMIC CAPTURE
                this%currNodeId                   = this%currNodeId + 1_int64
                nodePoolTmp(i)%nodeId%node_id     = this%currNodeId
                !$OMP END ATOMIC
                nodePoolTmp(i)%nodeId%pool_idx    = i
                nodePoolTmp(i)%coords(:)          =  coordsList(:, i-this%pop)
                nodePoolTmp(i)%hasData            =  hasData
                nodePoolTmp(i)%treeId             = tid
                if (nodePoolTmp(i)%hasData) then
                    allocate(nodePoolTmp(i)%data, source=dataList(i-this%pop))
                end if
            end do
            if (associated(this%nodePool)) deallocate(this%nodePool)
            this%nodePool => nodePoolTmp
            this%pop = pop
            !$OMP END CRITICAL (tree_mutate)

            ! rebuild decision and tree mutation
            ! serialized: modifications, rootIdx, and children are shared state
            !$OMP CRITICAL (tree_mutate)
            if (this%modifications + numNodeToAdd                     &
                .gt.                                                  &
                this%rebuildRatio * (this%pop - numNodeToAdd)         &
            ) then
                call rebuild(this)

            else

                ! no need for rebuild; insert new nodes at leaves.
                ! rootIdx=0 (empty tree) can never reach this branch: the rebuild condition
                ! simplifies to numNodeToAdd > 0, which is always true when pop was zero.
                do i = this%pop-numNodeToAdd + 1, this%pop
                    currIdx = this%rootIdx
                    do

                        currAxis = int(this%nodePool(currIdx)%nodeParams(1), int64)
                        if (this%nodePool(i)%coords(currAxis) .le. this%nodePool(currIdx)%coords(currAxis)) then
                            if (this%nodePool(currIdx)%children(1) .eq. 0_int64) then
                                this%nodePool(currIdx)%children(1) = i
                                allocate(this%nodePool(i)%nodeParams(1))
                                allocate(this%nodePool(i)%children(2))
                                this%nodePool(i)%nodeParams(1) = real(this%saxs(currAxis, dim), real64)
                                this%nodePool(i)%children(:)   = 0_int64
                                exit
                            else
                                currIdx = this%nodePool(currIdx)%children(1)
                            end if
                        else
                            if (this%nodePool(currIdx)%children(2) .eq. 0_int64) then
                                this%nodePool(currIdx)%children(2) = i
                                allocate(this%nodePool(i)%nodeParams(1))
                                allocate(this%nodePool(i)%children(2))
                                this%nodePool(i)%nodeParams(1) = real(this%saxs(currAxis, dim), real64)
                                this%nodePool(i)%children(:)   = 0_int64
                                exit
                            else
                                currIdx = this%nodePool(currIdx)%children(2)
                            end if
                        end if
                    end do
                end do

                this%modifications = this%modifications + numNodeToAdd
            end if
            !$OMP END CRITICAL (tree_mutate)

        end procedure addNodesKDT

        module procedure rmvNodesKDT
            logical                         :: hasIds, hasRad, hasCrd, resIsPtr
            type(NdNodePtr), allocatable    :: foundNodes(:)
            type(NdNodeBucket), allocatable :: foundNodesBucket(:)

            ! compaction variables
            integer(int64), allocatable     :: rmvIds(:)
            logical, allocatable            :: keepMask(:)
            type(NdNode), pointer           :: newPool(:)
            integer(int64)                  :: i, j, k, numRmvIds, newPop

            hasIds = present(ids)
            hasCrd = present(coordsList)
            hasRad = present(radii)

            ! search + compaction serialized together: the search reads this%nodePool,
            ! which another thread's critical section can deallocate and replace.
            ! keeping the search inside the same critical avoids that use-after-free.
            !$OMP CRITICAL (tree_mutate)

            if (hasIds .and. (.not. hasCrd)) then
                foundNodes = this%linScan(ids)
                resIsPtr = .true.

            else if (hasIds .and. hasCrd .and. (.not. hasRad)) then
                foundNodesBucket = this%rNN_Ids(coordsList, ids, metric, epsilon, bufferSize)
                resIsPtr = .false.

            else if ((.not. hasIds) .and. (.not. hasRad) .and. hasCrd) then
                foundNodesBucket = this%rNN_Coords(coordsList, metric, epsilon, bufferSize)
                resIsPtr = .false.

            else if ((.not. hasIds) .and. hasRad .and. hasCrd) then
                foundNodesBucket = this%rNN_Rad(coordsList, radii, metric, bufferSize)
                resIsPtr = .false.

            else
                foundNodesBucket = this%rNN_RadIds(coordsList, radii, ids, metric, bufferSize)
                resIsPtr = .false.
            end if

            ! collect nodeIds of all candidates found by the search
            if (resIsPtr) then
                numRmvIds = int(size(foundNodes), int64)
                allocate(rmvIds(numRmvIds))
                do j = 1_int64, numRmvIds
                    rmvIds(j) = foundNodes(j)%p%nodeId%node_id
                end do
            else
                numRmvIds = 0_int64
                do i = 1_int64, int(size(foundNodesBucket), int64)
                    numRmvIds = numRmvIds + int(size(foundNodesBucket(i)%nodes), int64)
                end do
                allocate(rmvIds(numRmvIds))
                k = 0_int64
                do i = 1_int64, int(size(foundNodesBucket), int64)
                    do j = 1_int64, int(size(foundNodesBucket(i)%nodes), int64)
                        k = k + 1_int64
                        rmvIds(k) = foundNodesBucket(i)%nodes(j)%p%nodeId%node_id
                    end do
                end do
            end if

            ! build keep mask: mark pool nodes whose nodeId appears in rmvIds
            ! re-check against the current pool inside the critical section so
            ! concurrent rmvNodes calls that already removed a node are handled correctly
            allocate(keepMask(this%pop))
            keepMask(:) = .true.
            do i = 1_int64, this%pop
                do j = 1_int64, numRmvIds
                    if (this%nodePool(i)%nodeId%node_id .eq. rmvIds(j)) then
                        keepMask(i) = .false.
                        exit
                    end if
                end do
            end do

            numRmv = count(.not. keepMask)

            if (numRmv .gt. 0) then
                newPop = this%pop - int(numRmv, int64)
                if (newPop .gt. 0_int64) then
                    allocate(newPool(newPop))
                    k = 0_int64
                    do i = 1_int64, this%pop
                        if (keepMask(i)) then
                            k = k + 1_int64
                            newPool(k)                    = this%nodePool(i)
                            newPool(k)%nodeId%pool_idx    = k
                        end if
                    end do
                    deallocate(this%nodePool)
                    this%nodePool => newPool
                    this%pop      = newPop
                    call rebuild(this)
                else
                    ! all nodes removed ; tree is structurally empty but still initialized
                    deallocate(this%nodePool)
                    this%nodePool   => null()
                    this%pop        = 0_int64
                    this%rootIdx    = 0_int64
                    this%modifications = 0_int64
                end if
            end if

            !$OMP END CRITICAL (tree_mutate)

        end procedure rmvNodesKDT

end submodule KdTreeModders
