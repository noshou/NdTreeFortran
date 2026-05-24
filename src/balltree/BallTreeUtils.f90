submodule(NdTreeFortran) BallTreeUtils
    use iso_fortran_env, only: output_unit, int64, real64
    implicit none
    contains

        module procedure printBallTree
            integer :: u
            u = output_unit
            if (present(unit)) u = unit
            if (this%rootIdx .ne. 0_int64) then
                call printBallNode(this%nodePool, this%rootIdx, 0_int64, u)
            else
                write(u, '(A)') '**empty tree**'
            end if
        end procedure printBallTree

        recursive subroutine printBallNode(nodePool, idx, depth, u)
            type(NdNode),   intent(in) :: nodePool(:)
            integer(int64), intent(in) :: idx, depth
            integer,        intent(in) :: u
            integer(int64)             :: i, d

            do d = 1, int(depth)
                write(u, '(A)', advance='no') '  '
            end do
            write(u, '(A,G0.4,A)', advance='no') '[r=', nodePool(idx)%nodeParams(1), '] ('
            do i = 1, size(nodePool(idx)%coords)
                if (i > 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') nodePool(idx)%coords(i)
            end do
            write(u, '(A)') ')'

            if (nodePool(idx)%children(1) .ne. 0_int64) &
                call printBallNode(nodePool, nodePool(idx)%children(1), depth + 1_int64, u)
            if (nodePool(idx)%children(2) .ne. 0_int64) &
                call printBallNode(nodePool, nodePool(idx)%children(2), depth + 1_int64, u)
        end subroutine printBallNode


        module procedure setMetricBLT
            logical :: isInit
            call this%getInitState(isInit)
            if (isInit) then  
                error stop "setMetric: tree is already initialized!"
            else 
                if (present(metric)) then ! comes preloaded with default, no need to double set 
                    select case (metric)
                        case ('euclidean')
                        case ('manhattan')
                        case ('chebyshev')
                        case default
                            error stop "setMetric: unknown metric"
                    end select
                    this%metric = metric
                end if
            end if
        end procedure setMetricBLT

        module procedure getMetricBLT
            logical :: isInit
            call this%getInitState(isInit)
            if (.not. isInit) then 
                error stop "getMetric: tree is not initialized (call build first?)"    
            else 
                metric = this%metric
            end if
        end procedure getMetricBLT

        module procedure getBallRadius
            logical :: isInit
            call this%getInitState(isInit)
            if (.not. isInit) then
                error stop "getBallRadius: tree is not initialized (call build first?)"
            else if (.not. this%isMember(node)) then
                error stop "getBallRadius: node not a member of tree"
            else
                radius = node%nodeParams(1)
            end if
        end procedure getBallRadius
        
        module procedure finalizerBLT
            this%metric = DEFAULT_METRIC
            call this%destroy()
        end procedure finalizerBLT

end submodule BallTreeUtils