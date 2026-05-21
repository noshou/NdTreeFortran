submodule(NdTreeFortran) NdTreeUtils
    use iso_fortran_env, only: output_unit, int64, real64
    implicit none
    contains
        module procedure setRebuildRatio
            if (ratio .le. 0.0_real64) then
                error stop "setRebuildRatio: rebuildRatio must be greater than zero!"
            else if (ratio .ge. 1.0_real64) then
                error stop "setRebuildRatio: rebuildRatio must be less than 1!"
            else
                this%rebuildRatio = ratio
            end if
        end procedure setRebuildRatio


        module procedure isMember
            integer(int64) :: i, hint
            
            ! null pointer
            if (.not. associated(target)) then
                res = .false.
                return
            end if
            
            ! tree id mismatch
            if (this%treeId .ne. target%treeId) then
                res = .false.
                return
            end if

            ! use pool_idx hint for O(1) fast path
            hint = target%nodeId%pool_idx
            if (hint .ge. 1_int64 .and. hint .le. this%pop) then
                if (this%nodePool(hint)%nodeId%node_id .eq. target%nodeId%node_id) then
                    res = .true.
                    return
                end if
            end if
            
            ! hint stale or slot reused; fall back to O(n) scan
            res = .false.
            do i = 1_int64, this%pop
                if (this%nodePool(i)%nodeId%node_id .eq. target%nodeId%node_id) then
                    res = .true.
                    return
                end if
            end do
        end procedure isMember

        module procedure assertMetric

            select type(this)
                class is (BallTree)
                    m = this%metric
                    if (present(metric)) then
                        if (m .ne. metric) then
                            write (*,*) name, ": expected metric: ", m, " but got: ", metric
                            stop 1
                        end if
                    end if
                
                class is (KdTree) 
                    if (present(metric)) then 
                        select case (metric)
                            case ('euclidean'); m = 'euclidean'
                            case ('manhattan'); m = 'manhattan'
                            case ('chebyshev'); m = 'chebyshev'
                            case default
                                write (*,*) name, ": unkown metric: ", metric
                                stop 1
                        end select 
                    else
                        m = DEFAULT_METRIC
                    end if
            end select
        end procedure assertMetric


        !> Strips leading zeros from exponent fields (e.g. E+001 -> E+1) for
        !! portable comparison across compilers with different exponent widths.
        function normalizeExp(s) result(t)
            character(len=*), intent(in) :: s
            character(len=len(s))        :: t
            integer                      :: i, j, k, n
            n = len_trim(s)
            t = ' '
            i = 1
            j = 1
            do while (i <= n)
                if ((s(i:i) == 'E' .or. s(i:i) == 'e') .and. i < n) then
                    if (s(i+1:i+1) == '+' .or. s(i+1:i+1) == '-') then
                        t(j:j) = s(i:i)
                        j = j + 1
                        t(j:j) = s(i+1:i+1)
                        j = j + 1
                        k = i + 2
                        ! skip leading zeros, keeping at least one digit
                        do while (k < n .and. s(k:k) == '0' .and. &
                                    s(k+1:k+1) >= '0' .and. s(k+1:k+1) <= '9')
                            k = k + 1
                        end do
                        do while (k <= n .and. s(k:k) >= '0' .and. s(k:k) <= '9')
                            t(j:j) = s(k:k)
                            j = j + 1
                            k = k + 1
                        end do
                        i = k
                    else
                        t(j:j) = s(i:i)
                        j = j + 1
                        i = i + 1
                    end if
                else
                    t(j:j) = s(i:i)
                    j = j + 1
                    i = i + 1
                end if
            end do
        end function normalizeExp

        !> Returns the substring of s from the first '(' onward, or
        !! adjustl(s) if no '(' is present (e.g. '**empty tree**').
        function stripPrefix(s) result(t)
            character(len=*), intent(in) :: s
            character(len=64)            :: t
            integer                      :: pos
            pos = index(s, '(')
            if (pos.eq.0) then
                t = normalizeExp(adjustl(s))
            else
                t = normalizeExp(s(pos:))
            end if
        end function stripPrefix

        !> Insertion sort, lexicographic, in place.
        subroutine sortLines(arr)
            character(len=*), intent(inout) :: arr(:)
            integer                         :: i, j
            character(len=len(arr))         :: tmp
            do i = 2, size(arr)
                tmp = arr(i)
                j = i - 1
                do while (j .ge. 1)
                    if (.not. (arr(j) .gt. tmp)) exit
                    arr(j+1) = arr(j)
                    j = j - 1
                end do
                arr(j+1) = tmp
            end do
        end subroutine sortLines

        !> Prints a labeled block of lines for diagnostics.
        subroutine dumpLines(label, lines)
            character(len=*), intent(in) :: label
            character(len=*), intent(in) :: lines(:)
            integer                      :: i
            write(*, '(A)') label
            do i = 1, size(lines)
                write(*, '(A)') '    "' // trim(lines(i)) // '"'
            end do
        end subroutine dumpLines

        module procedure assert

            integer                        :: u, ios, i
            character(len=64)              :: line
            character(len=64), allocatable :: actual(:), expCopy(:)

            ! capture per-node lines from printTree, stripped to coord-tuples
            allocate(actual(0))
            open(newunit=u, status='scratch', action='readwrite')
            call this%printTree(unit=u)
            rewind(u)
            do
                read(u, '(A)', iostat=ios) line
                if (ios .ne. 0) exit
                actual = [actual, stripPrefix(line)]
            end do
            close(u)

            if (size(actual) .ne. size(expected)) then
                write(*, '(A,I0,A,I0)') '--- ' // testName // ' FAILED: node count mismatch &
                    &-> expected ', size(expected), ', got ', size(actual)
                call dumpLines('  expected:', expected)
                call dumpLines('  got:     ', actual)
                stop 1
            end if

            ! make a sortable copy of expected, also stripped to coord-tuples
            allocate(expCopy(size(expected)))
            do i = 1, size(expected)
                expCopy(i) = stripPrefix(expected(i))
            end do

            call sortLines(actual)
            call sortLines(expCopy)

            do i = 1, size(actual)
                if (trim(actual(i)) .ne. trim(expCopy(i))) then
                    write(*, '(A)') '--- ' // testName // ' FAILED: node set mismatch'
                    call dumpLines('  expected (coords, sorted):', expCopy)
                    call dumpLines('  got      (coords, sorted):', actual)
                    stop 1
                end if
            end do
        end procedure assert
        
        module procedure associatedNodePool
            assoc = associated(this%nodePool)
        end procedure associatedNodePool

        module procedure associatedRoot
            assoc = (this%rootIdx .ne. 0_int64)
        end procedure associatedRoot

        module procedure destroy
            if (associated(this%nodePool)) deallocate(this%nodePool)
            this%nodePool       => null()
            this%rootIdx        = 0_int64
            this%dim            = 0_int64
            this%pop            = 0_int64
            this%treeId         = 0_int64
            this%initialized    = .false.
            this%modifications  = 0_int64
            this%rebuildRatio   = 0.25_real64
            this%currNodeId     = 0_int64
        end procedure destroy

end submodule NdTreeUtils