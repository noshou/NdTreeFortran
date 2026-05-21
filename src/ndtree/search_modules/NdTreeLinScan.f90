submodule(NdTreeFortran) NdTreeLinScan
    implicit none
    contains
        module procedure linScan
            type(NdNode), pointer :: copy
            type(NdNodePtr), allocatable :: tmp(:)
            integer(int64) :: i, j, numFound, hint

            if (.not. this%initialized) then
                error stop "linScan: tree is not initialized (call build first)"
            end if
            if (size(ids) .eq. 0 .or. this%pop .eq. 0_int64) then
                allocate(res(0))
                return
            end if

            allocate(res(size(ids)))
            numFound = 0_int64

            do j = 1_int64, size(ids)
                ! O(1) fast path: use pool_idx hint
                hint = ids(j)%pool_idx
                if (hint .ge. 1_int64 .and. hint .le. this%pop) then
                    if (this%nodePool(hint)%nodeId%node_id .eq. ids(j)%node_id) then
                        numFound = numFound + 1_int64
                        allocate(copy, source=this%nodePool(hint))
                        res(numFound)%p => copy
                        cycle
                    end if
                end if
                ! O(n) fallback: hint stale or unknown.
                ! refresh the caller's pool_idx hint to the resolved slot
                do i = 1_int64, this%pop
                    if (this%nodePool(i)%nodeId%node_id .eq. ids(j)%node_id) then
                        ids(j)%pool_idx = i
                        numFound = numFound + 1_int64
                        allocate(copy, source=this%nodePool(i))
                        res(numFound)%p => copy
                        exit
                    end if
                end do
            end do

            if (numFound .eq. 0_int64) then
                deallocate(res)
                allocate(res(0))
            else if (numFound .lt. int(size(res), int64)) then
                allocate(tmp(numFound))
                do i = 1_int64, numFound
                    tmp(i)%p    => res(i)%p
                    res(i)%p    => null()
                end do
                call move_alloc(from=tmp, to=res)
            end if
        end procedure linScan

end submodule NdTreeLinScan
