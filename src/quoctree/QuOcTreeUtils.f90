submodule(NdTreeFortran) QuOcTree
    implicit none
    contains
        
        module procedure getTypeQOT
            type = this%type
        end procedure getTypeQOT

        module procedure finalizerQOT
            this%type = QOT_UND
            call this%destroy()
        end procedure finalizerQOT

        module procedure getBBoxCoords

            logical        :: isInit
            integer(int64) :: iFlat, dim

            call this%getInitState(isInit)

            if (.not. isInit) then
                error stop "getBBoxCoords: tree is not initialized (call build first?)"
            else if (.not. this%isMember(node)) then
                error stop "getBBoxCoords: node is not a member of this tree!"
            else 
                select case (this%type)

                    ! Quadtree -> 2D square bounding box
                    case (QOT_QUT)
                        iFlat = 4_int64
                        dim   = 2_int64

                    ! Octree  -> 3D cube bounding box
                    case (QOT_OCT)
                        iFlat = 8_int64
                        dim   = 3_int64
                end select 
                allocate(coords(dim, iFlat))
                coords = reshape(node%nodeParams(1:dim*iFlat), [dim, iFlat])
            end if
        end procedure getBBoxCoords

        module procedure getBBoxMeasure

            logical                   :: isInit
            real(real64)              :: xMin, yMin, zMin, xMax, yMax, zMax
            real(real64), allocatable :: coords(:,:)

            call this%getInitState(isInit)

            if (.not. isInit) then
                error stop "getBBoxMeasure: tree is not initialized (call build first?)"
            else if (.not. this%isMember(node)) then
                error stop "getBBoxMeasure: node is not a member of this tree!"
            else 
                coords = this%getBBoxCoords(node)
                select case (this%type)
                    case (QOT_QUT)
                        xMin = minval(coords(1, :))
                        xMax = maxval(coords(1, :))
                        yMin = minval(coords(2, :))
                        yMax = maxval(coords(2, :))
                        measure = (xMax-xMin) * (yMax-yMin)
                    case (QOT_OCT)
                        xMin = minval(coords(1, :))
                        xMax = maxval(coords(1, :))
                        yMin = minval(coords(2, :))
                        yMax = maxval(coords(2, :))
                        zMin = minval(coords(3, :))
                        zMax = maxval(coords(3, :))
                        measure = (xMax-xMin) * (yMax-yMin) * (zMax-zMin)
                end select 
            end if 
        end procedure getBBoxMeasure
    
        module procedure printQuOcTree
            integer :: u
            u = output_unit
            if (present(unit)) u = unit
            if (this%rootIdx .ne. 0_int64) then
                call printQOTNode(this%nodePool, this%rootIdx, 0_int64, u)
            else
                write(u, '(A)') '**empty tree**'
            end if
        end procedure printQuOcTree

        recursive subroutine printQOTNode(nodePool, idx, depth, u)
            type(NdNode),   intent(in) :: nodePool(:)
            integer(int64), intent(in) :: idx, depth
            integer,        intent(in) :: u
            integer(int64)             :: i, d, c, dim

            dim = size(nodePool(idx)%coords, kind=int64)

            do d = 1_int64, depth
                write(u, '(A)', advance='no') '  '
            end do

            ! print bbox lo..hi per axis; stride dim through nodeParams to collect all values per axis
            write(u, '(A)', advance='no') '[bbox=('
            do i = 1_int64, dim
                if (i > 1_int64) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4,A,G0.4)', advance='no') &
                    minval(nodePool(idx)%nodeParams(i::int(dim))), &
                    '..', &
                    maxval(nodePool(idx)%nodeParams(i::int(dim)))
            end do
            write(u, '(A)', advance='no') ')] ('

            ! print coords
            do i = 1_int64, dim
                if (i > 1_int64) write(u, '(A)', advance='no') ', '
                write(u, '(G0.4)', advance='no') nodePool(idx)%coords(i)
            end do
            write(u, '(A)') ')'

            do c = 1_int64, size(nodePool(idx)%children, kind=int64)
                if (nodePool(idx)%children(c) .ne. 0_int64) &
                    call printQOTNode(nodePool, nodePool(idx)%children(c), depth + 1_int64, u)
            end do
        end subroutine printQOTNode

end submodule QuOcTree