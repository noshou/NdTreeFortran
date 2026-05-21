submodule(NdTreeFortran) NdNodeUtils
    implicit none
    contains

        module procedure printNode
            integer        :: u
            integer(int64) :: i, c

            u = output_unit
            if (present(unit)) u = unit

            ! indentation: 2 spaces per depth level
            do i = 1, depth
                write(u, '(A)', advance='no') '  '
            end do

            write(u, '(A)', advance='no') '('
            do i = 1, size(this%coords)
                if (i .gt. 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

            if (allocated(this%children)) then
                do c = 1, size(this%children, kind=int64)
                    if (this%children(c) .ne. 0_int64) &
                        call nodePool(this%children(c))%printNode(depth + 1_int64, nodePool, unit)
                end do
            end if
        end procedure printNode

        module procedure printNodeSingle 
            integer                       :: u
            integer(int64)                :: i

            u = output_unit
            if (present(unit)) u = unit

            write(u, '(A)', advance='no') '('
            do i = 1, size(this%coords)
                if (i .gt. 1) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') this%coords(i)
            end do
            write(u, '(A)') ')'

        end procedure printNodeSingle
        
        module procedure destroyNodePtr
            if (associated(this%p)) deallocate(this%p)
            this%p => null()
        end procedure destroyNodePtr

        module procedure finalizerNodePtr
            call destroyNodePtr(this)
        end procedure finalizerNodePtr

        module procedure destroyNodeBucket
            integer(int64) :: i
            do i = 1, size(this%nodes)
                call this%nodes(i)%destroy()
            end do
        end procedure destroyNodeBucket

        module procedure finalizerNodeBucket
            call destroyNodeBucket(this)
        end procedure finalizerNodeBucket

end submodule NdNodeUtils