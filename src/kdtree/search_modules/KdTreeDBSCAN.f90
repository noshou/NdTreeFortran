submodule(KdTreeFortran) KdTreeDBSCAN
    implicit none
    contains 

        module procedure DBSCAN
            type(KdNodePtr), allocatable :: neighbourhood(:), clusterNodes(:)
            type(NodeId),    allocatable :: nodes(:)
            type(KdNodePtr), allocatable :: node
            integer                      :: bs
            integer(int64)               :: i, j, k, clusterIdx, seedsSize, oldSize, noiseCount
            character(len=9)             :: m
            logical                      :: isInit
            logical,         allocatable :: visited(:) 
            integer(int64),  allocatable :: cluster(:), seeds(:), seedsTmp(:), counts(:)
            integer(int64),  parameter   :: UNASSIGNED = -1_int64, NOISE = 0_int64  

            ! assertion checks + initializations
            call this%getInitState(isInit)
            if (.not. isInit) then 
                error stop "DBSCAN: tree is uninitialized (call build first?)"
            else if (minPts .lt. 0) then 
                error stop "DBSCAN: invalid minimum points"
            else if (radius .lt. 0_real64) then 
                error stop "DBSCAN: invalid radius"
            end if

            if (present(metric)) then 
                select case (metric)
                    case ('euclidean'); m = 'euclidean'
                    case ('manhattan'); m = 'manhattan'
                    case ('chebyshev'); m = 'chebyshev'
                    case default;       error stop "DBSCAN: unknown metric"
                end select 
            else
                m = DEFAULT_METRIC
            end if

            if (present(bufferSize)) then
                if (bufferSize .le. 0) then
                    error stop "DBSCAN: invalid bufferSize"
                else 
                    bs = bufferSize
                end if
            else
                bs = DEFAULT_BUFFER_SIZE
            end if

            ! tree is empty; return "empty" result
            if (this%pop .eq. 0) then 
                allocate(res(0))
            
            ! tree is not empty; scan 
            else 
                
                ! parallel boolean array of visited nodes
                ! visited(i) -> nodePool(i) = true iff node visited
                allocate(visited(this%pop))
                visited = .false.

                ! parallel integer array of node assignments
                ! cluster(i) -> nodePool(i) = cluster ID
                allocate(cluster(this%pop))
                cluster = UNASSIGNED

                ! counts(i) = number of nodes in cluster i; noiseCount = number of noise nodes
                allocate(counts(this%pop))
                counts     = 0_int64
                noiseCount = 0_int64
                clusterIdx = 0_int64

                ! get list of nodes to iterate thru
                allocate(nodes, source=this%getAllNodeIds())

                allocate(node)

                ! iterate over all nodes
                do i = 1_int64, this%pop

                    node%p => this%nodePool(nodes(i)%pool_idx)

                    ! if we haven't visited, get neighbourhood
                    if (.not. visited(i)) then
                        neighbourhood = this%rNN_Node( &
                            node,                      &
                            radius,                    &
                            bs,                        &
                            m                          &
                        )

                        ! set this node to visited
                        visited(i) = .true.

                        ! if size < minPts, then tentatively add to "noise"
                        ! else, explore
                        if (size(neighbourhood) .lt. minPts) then
                            cluster(i) = NOISE
                            noiseCount = noiseCount + 1_int64
                        else 
                            ! this node must belong to a new cluster
                            clusterIdx = clusterIdx + 1_int64
                            cluster(i) = clusterIdx
                            counts(clusterIdx) = counts(clusterIdx) + 1_int64

                            ! we now have a neighbourhood around a core point;
                            ! expand into seeds, search out in neighbourhood
                            if (allocated(seeds)) deallocate(seeds)
                            allocate(seeds, source=[(neighbourhood(k)%p%nodeId%pool_idx, k=1, size(neighbourhood))])
                            seedsSize = size(seeds)
                            j = 1
                            do while (j .le. seedsSize)
                                if (cluster(seeds(j)) .eq. UNASSIGNED .or. cluster(seeds(j)) .eq. NOISE) then
                                    if (cluster(seeds(j)) .eq. NOISE) noiseCount = noiseCount - 1_int64
                                    cluster(seeds(j)) = clusterIdx
                                    counts(clusterIdx) = counts(clusterIdx) + 1_int64
                                end if

                                if (.not. visited(seeds(j))) then
                                    node%p => this%nodePool(seeds(j))
                                    neighbourhood = this%rNN_Node( &
                                        node,                      &
                                        radius,                    &
                                        bs,                        &
                                        m                          &
                                    )
                                    visited(seeds(j)) = .true.

                                    if (size(neighbourhood) .ge. minPts) then
                                        oldSize = seedsSize
                                        seedsSize = seedsSize + size(neighbourhood)
                                        allocate(seedsTmp(seedsSize))
                                        seedsTmp(1:oldSize) = seeds
                                        seedsTmp(oldSize+1:seedsSize) = [(neighbourhood(k)%p%nodeId%pool_idx, k=1, size(neighbourhood))]
                                        call move_alloc(from=seedsTmp, to=seeds)
                                    end if
                                end if
                                j = j + 1
                            end do

                        end if
                    end if
                end do

                ! free up memory
                deallocate(visited)
                if (allocated(seeds))    deallocate(seeds)
                if (allocated(seedsTmp)) deallocate(seedsTmp)

                ! since finalizer of KdNodePtr frees the node itself, we don't want
                ! that happening to the node pool. so set it to null here
                node%p => null()

                ! we know a total of "clusterIdx" points have been assigned, 
                ! so assign clusterIdx + 1 buckets
                allocate(res(clusterIdx+1))
                do i = 1, clusterIdx
                    allocate(res(i)%nodes(counts(i)))
                end do
                allocate(res(clusterIdx+1)%nodes(noiseCount))

                ! reuse counts as a write cursor: counts(c) starts at the allocated size
                ! of res(c) and decrements each time a node is placed into res(c)%nodes.
                ! iterate over all pool positions; for each node get its cluster assignment c,
                ! place a copy at res(c)%nodes(counts(c)), then decrement counts(c).
                do i = 1_int64, this%pop
                    associate(c => cluster(i))
                        if (c .eq. NOISE) then
                            allocate(res(clusterIdx+1)%nodes(noiseCount)%p, source=this%nodePool(i))
                            noiseCount = noiseCount - 1_int64
                        else if (c .ne. UNASSIGNED) then
                            allocate(res(c)%nodes(counts(c))%p, source=this%nodePool(i))
                            counts(c) = counts(c) - 1_int64
                        end if
                    end associate
                end do


            end if
        end procedure DBSCAN

end submodule KdTreeDBSCAN