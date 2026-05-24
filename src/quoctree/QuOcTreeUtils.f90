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

end submodule QuOcTree