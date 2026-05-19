submodule(NdTreeFortran) KdTreeModders
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure addNodesKDT

            logical                 :: isInit, hasData, rootHasData, hasRoot
            integer(int64)          :: dataListSize, i, dim, pop, numNodeToAdd, tid, currIdx, currAxis
            type(NdNode), pointer   :: nodePoolTmp(:)

            ! initialize state variables
            call this%associatedRoot(hasRoot)
            if (hasRoot) rootHasData = this%nodePool(this%rootIdx)%hasData
            call this%getInitState(isInit)
            hasData = present(dataList)
            numNodeToAdd = size(coordsList, 2)
            dim = size(coordsList, 1)

            ! assertion checks
            if (.not. isInit) then
                error stop "addNodes: tree uninitialized (call this%build() first?)"
            else if (dim .ne. this%dim) then
                error stop "addNodes: dimension of coordinates must match dimension of tree!"
            else if (hasData) then
                dataListSize = size(dataList)
                if (dataListSize .eq. 0_int64) then
                    error stop "addNodes: size of data list must be greater than zero!"
                else if (dataListSize .ne. numNodeToAdd) then
                    error stop "addNodes: number of data points must match number of coordinates!"
                else if (hasRoot) then
                    if (.not. rootHasData) then
                        error stop "addNodes: tree takes no data input!"
                    else if (.not. same_type_as(dataList(1), this%nodePool(this%rootIdx)%data)) then
                        error stop "addNodes: data mismatch between tree and dataList"
                    end if
                end if
            else
                if (hasRoot .and. rootHasData) then
                    error stop "addNodes: tree requires data input!"
                end if
            end if

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
            logical                         :: isInit, hasIds, hasEps, hasRad, hasCrd, resIsPtr
            integer                         :: sizeRad, sizeCrd, sizeDim, sizeIds, buffSze
            integer(int64)                  :: dim
            character(len=9)                :: mtr
            type(NdNodePtr), allocatable    :: foundNodes(:)
            type(NdNodeBucket), allocatable :: foundNodesBucket(:)
            
            ! compaction variables
            integer(int64), allocatable     :: rmvIds(:)
            logical, allocatable            :: keepMask(:)
            type(NdNode), pointer           :: newPool(:)
            integer(int64)                  :: i, j, k, numRmvIds, newPop

            ! state variables
            call this%getInitState(isInit)
            hasIds = present(ids)
            hasEps = present(epsilon)
            hasCrd = present(coordsList)
            hasRad = present(radii)
            dim    = this%dim
            if (.not.hasRad) then; sizeRad=0; else; sizeRad=size(radii);        end if
            if (.not.hasCrd) then; sizeCrd=0; else; sizeCrd=size(coordsList,2); end if
            if (.not.hasCrd) then; sizeDim=0; else; sizeDim=size(coordsList,1); end if
            if (.not.hasIds) then; sizeIds=0; else; sizeIds=size(ids);          end if

            ! assertion checks
            if (.not. isInit) then
                error stop "rmvNodes: tree uninitialized (call this%build() first?)"
            else if (hasRad .and. .not. hasCrd) then
                error stop "rmvNodes: radii must be supplied with a list of coordinates"
            else if (hasRad .and. sizeRad .ne. sizeCrd) then
                error stop "rmvNodes: number of radii must match number of coordinates"
            else if (hasCrd .and. ((sizeDim .eq. 0) .or. (sizeCrd .eq. 0))) then
                error stop "rmvNodes: coordsList is empty"
            else if (hasCrd .and. (sizeDim .ne. dim)) then
                error stop "rmvNodes: dimension of coordinates must match dimension of tree"
            else if (hasIds .and. (sizeIds .eq. 0)) then
                error stop "rmvNodes: ids is empty"
            else if (.not. (hasIds .or. hasCrd)) then
                error stop "rmvNodes: must supply ids or coordsList"
            else if (hasCrd .and. hasIds .and. (.not. hasRad) .and. (sizeCrd .ne. sizeIds)) then
                error stop "rmvNodes: when coordsList and ids are passed without radii, sizes must match"
            end if
            if (present(bufferSize)) then
                if (bufferSize .le. 0) then
                    error stop "rmvNodes: invalid bufferSize"
                else
                    buffSze = bufferSize
                end if
            else
                buffSze = DEFAULT_BUFFER_SIZE
            end if

            if (.not. present(metric)) then
                mtr = DEFAULT_METRIC
            else
                select case (metric)
                    case ('euclidean'); mtr = 'euclidean'
                    case ('manhattan'); mtr = 'manhattan'
                    case ('chebyshev'); mtr = 'chebyshev'
                    case default;       error stop "rmvNodes: unknown metric"
                end select
            end if

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
