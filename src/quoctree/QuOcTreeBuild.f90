submodule(NdTreeFortran) QuOcTreeBuild
    implicit none
    contains 
        module procedure buildSubtreeQOT
            real(real64)   :: randomNumber, xMin, xMax, yMin, yMax, zMin, zMax
            integer(int64) :: pivotIdx, i, tmp, sw, se, nw, swd, swu, sed, seu, nwd, nwu, ned 

            ! assertion checks
            if (this%type .eq. QOT_UND) then 
                select case (this%dim)
                    case (2)
                        this%type = QOT_QUT
                    case (3)
                        this%type = QOT_OCT
                    case default
                        error stop "buildSubtreeQOT: illegal coordinate dimension (must be 2 or 3)"
                end select
            end if

            if (lowerIdx .gt. upperIdx) then 
                rootIdx = 0_int64
            
            else 

                ! get coordinates of bounding box
                select case (this%type)
                    
                ! Quadtree -> four extrema
                case (QOT_QUT)
                        xMin = this%nodePool(indices(lowerIdx))%coords(1)
                        xMax = this%nodePool(indices(lowerIdx))%coords(1)
                        yMin = this%nodePool(indices(lowerIdx))%coords(2)
                        yMax = this%nodePool(indices(lowerIdx))%coords(2)
                        do i = lowerIdx, upperIdx
                            if (xMin .gt. this%nodePool(indices(i))%coords(1)) then 
                                xMin = this%nodePool(indices(i))%coords(1)
                            end if
                            if (xMax .lt. this%nodePool(indices(i))%coords(1)) then 
                                xMax = this%nodePool(indices(i))%coords(1)
                            end if
                            if (yMin .gt. this%nodePool(indices(i))%coords(2)) then 
                                yMin = this%nodePool(indices(i))%coords(2)
                            end if
                            if (yMax .lt. this%nodePool(indices(i))%coords(2)) then 
                                yMax = this%nodePool(indices(i))%coords(2)
                            end if
                        end do
                    
                    ! Octree -> six extrema 
                    case (QOT_OCT)
                        xMin = this%nodePool(indices(lowerIdx))%coords(1)
                        xMax = this%nodePool(indices(lowerIdx))%coords(1)
                        yMin = this%nodePool(indices(lowerIdx))%coords(2)
                        yMax = this%nodePool(indices(lowerIdx))%coords(2)
                        zMin = this%nodePool(indices(lowerIdx))%coords(3)
                        zMax = this%nodePool(indices(lowerIdx))%coords(3)
                        do i = lowerIdx, upperIdx
                            if (xMin .gt. this%nodePool(indices(i))%coords(1)) then 
                                xMin = this%nodePool(indices(i))%coords(1)
                            end if
                            if (xMax .lt. this%nodePool(indices(i))%coords(1)) then 
                                xMax = this%nodePool(indices(i))%coords(1)
                            end if
                            if (yMin .gt. this%nodePool(indices(i))%coords(2)) then 
                                yMin = this%nodePool(indices(i))%coords(2)
                            end if
                            if (yMax .lt. this%nodePool(indices(i))%coords(2)) then 
                                yMax = this%nodePool(indices(i))%coords(2)
                            end if
                            if (zMin .gt. this%nodePool(indices(i))%coords(3)) then 
                                zMin = this%nodePool(indices(i))%coords(3)
                            end if
                            if (zMax .lt. this%nodePool(indices(i))%coords(3)) then 
                                zMax = this%nodePool(indices(i))%coords(3)
                            end if
                        end do
                        
                    end select

                ! get root of this tree, stash root at end so the partition doesn't touch it
                call random_number(randomNumber)
                pivotIdx = lowerIdx + floor(randomNumber * (upperIdx - lowerIdx + 1_int64))
                tmp = indices(pivotIdx)
                indices(pivotIdx) = indices(upperIdx)
                indices(upperIdx) = tmp
                rootIdx  = indices(upperIdx)

                ! allocate children; 4 for quadtree, 8 for octree
                ! nodeParams: flattened corners of the bounding box, dim values per corner
                !
                ! Quadtree (8 values, 4 corners):
                !   (1,2)=(xMin,yMin)  (3,4)=(xMin,yMax)
                !   (5,6)=(xMax,yMax)  (7,8)=(xMax,yMin)
                !   lo = nodeParams(1:2),  hi = nodeParams(5:6)
                !
                ! Octree (24 values, 8 corners):
                !   ( 1, 2, 3)=(xMin,yMin,zMin)  ( 4, 5, 6)=(xMax,yMin,zMin)
                !   ( 7, 8, 9)=(xMin,yMax,zMin)  (10,11,12)=(xMax,yMax,zMin)
                !   (13,14,15)=(xMin,yMin,zMax)  (16,17,18)=(xMax,yMin,zMax)
                !   (19,20,21)=(xMin,yMax,zMax)  (22,23,24)=(xMax,yMax,zMax)
                !   lo = nodeParams(1:3),  hi = nodeParams(22:24)
                select case (this%type)
                    
                    ! BBox in a QuadTree is a 2D box, so in total there are 
                    ! 4 children/corners and 8 values in nodeParams
                    case (QOT_QUT)

                        ! initialize root
                        allocate(this%nodePool(rootIdx)%children(4))
                        allocate(this%nodePool(rootIdx)%nodeParams(8))
                        this%nodePool(rootIdx)%children(:) = 0_int64
                        this%nodePool(rootIdx)%treeId      = this%treeId

                        ! corner 1
                        this%nodePool(rootIdx)%nodeParams(1) = xMin
                        this%nodePool(rootIdx)%nodeParams(2) = yMin
                        
                        ! corner 2
                        this%nodePool(rootIdx)%nodeParams(3) = xMin
                        this%nodePool(rootIdx)%nodeParams(4) = yMax
                        
                        ! corner 3
                        this%nodePool(rootIdx)%nodeParams(5) = xMax
                        this%nodePool(rootIdx)%nodeParams(6) = yMax
                        
                        ! corner 4 
                        this%nodePool(rootIdx)%nodeParams(7) = xMax
                        this%nodePool(rootIdx)%nodeParams(8) = yMin 
                    
                    ! BBox in an OcTree is a 3D box, so in total there are 
                    ! 8 children/corners and 24 values in nodeParams
                    case (QOT_OCT)
                        
                        ! initialize root
                        allocate(this%nodePool(rootIdx)%children(8))
                        allocate(this%nodePool(rootIdx)%nodeParams(24))
                        this%nodePool(rootIdx)%children(:) = 0_int64
                        this%nodePool(rootIdx)%treeId      = this%treeId
                        
                        ! corner 1
                        this%nodePool(rootIdx)%nodeParams(1)  = xMin
                        this%nodePool(rootIdx)%nodeParams(2)  = yMin
                        this%nodePool(rootIdx)%nodeParams(3)  = zMin

                        ! corner 2
                        this%nodePool(rootIdx)%nodeParams(4)  = xMax
                        this%nodePool(rootIdx)%nodeParams(5)  = yMin
                        this%nodePool(rootIdx)%nodeParams(6)  = zMin
                        
                        ! corner 3
                        this%nodePool(rootIdx)%nodeParams(7)  = xMin
                        this%nodePool(rootIdx)%nodeParams(8)  = yMax
                        this%nodePool(rootIdx)%nodeParams(9)  = zMin 
                        
                        ! corner 4
                        this%nodePool(rootIdx)%nodeParams(10) = xMax
                        this%nodePool(rootIdx)%nodeParams(11) = yMax
                        this%nodePool(rootIdx)%nodeParams(12) = zMin
                        
                        ! corner 5
                        this%nodePool(rootIdx)%nodeParams(13) = xMin
                        this%nodePool(rootIdx)%nodeParams(14) = yMin
                        this%nodePool(rootIdx)%nodeParams(15) = zMax
                        
                        ! corner 6
                        this%nodePool(rootIdx)%nodeParams(16) = xMax
                        this%nodePool(rootIdx)%nodeParams(17) = yMin
                        this%nodePool(rootIdx)%nodeParams(18) = zMax
                        
                        ! corner 7
                        this%nodePool(rootIdx)%nodeParams(19) = xMin
                        this%nodePool(rootIdx)%nodeParams(20) = yMax 
                        this%nodePool(rootIdx)%nodeParams(21) = zMax
                        
                        ! corner 8
                        this%nodePool(rootIdx)%nodeParams(22) = xMax
                        this%nodePool(rootIdx)%nodeParams(23) = yMax
                        this%nodePool(rootIdx)%nodeParams(24) = zMax

                end select

                ! partition indices, recurse on children
                select case (this%type)
                
                    case (QOT_QUT)
                        call partitionQuadrants(  &
                            this%nodePool,        &
                            (xMin+xMax)/2_real64, &
                            (yMin+yMax)/2_real64, &
                            indices,              &
                            lowerIdx,             &
                            upperIdx,             &
                            sw,                   &
                            se,                   &
                            nw                    &
                        )
                        call this%buildSubtree(this%nodePool(rootIdx)%children(1), indices, lowerIdx,   sw)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(2), indices, sw+1_int64, se)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(3), indices, se+1_int64, nw)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(4), indices, nw+1_int64, upperIdx-1_int64)

                    case (QOT_OCT)
                        call partitionOctants(    &
                            this%nodePool,        &
                            (xMin+xMax)/2_real64, &
                            (yMin+yMax)/2_real64, &
                            (zMin+zMax)/2_real64, &
                            indices,              &
                            lowerIdx,             &
                            upperIdx,             &
                            swd,                  &
                            swu,                  &
                            sed,                  &
                            seu,                  &
                            nwd,                  &
                            nwu,                  &
                            ned                   &
                        )
                        call this%buildSubtree(this%nodePool(rootIdx)%children(1), indices, lowerIdx,    swd)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(2), indices, swd+1_int64, swu)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(3), indices, swu+1_int64, sed)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(4), indices, sed+1_int64, seu)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(5), indices, seu+1_int64, nwd)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(6), indices, nwd+1_int64, nwu)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(7), indices, nwu+1_int64, ned)
                        call this%buildSubtree(this%nodePool(rootIdx)%children(8), indices, ned+1_int64, upperIdx-1_int64)

                end select

            end if

        end procedure buildSubtreeQOT

        !> for a quadtree, each root has 4 subtrees split into quadrants:
        !!
        !!   1. North-East: (x ≥ xMid) && (y ≥ yMid)
        !!
        !!   2. North-West: (x ≥ xMid) && (y < yMid)
        !!
        !!   3. South-East: (x < xMid) && (y ≥ yMid)
        !!
        !!   4. South-West: (x < xMid) && (y < yMid)
        !!
        !! partition: [South-West|South-East|North-West|North-East]
        !!
        !! bounds:    [lowerIdx..sw|sw+1..se|se+1..ne|ne+1..rootIdx-1]
        !!
        !! @param[in]    nodePool the tree's node pool
        !! @param[in]    xMid     midpoint along x axis
        !! @param[in]    yMid     midpoint along y axis
        !! @param[inout] indices  index permutation array; rearranged in-place
        !! @param[in]    lowerIdx lower bound of the slice to partition (inclusive)
        !! @param[in]    upperIdx upper bound of the slice (exclusive);
        !!                        caller must ensure that the root is at this index
        !! @param[inout] sw       last index of the South-West partition
        !! @param[inout] se       last index of the South-East partition
        !! @param[inout] nw       last index of the North-East partition
        subroutine partitionQuadrants( &
            nodePool,                 &    
            xMid,                     & 
            yMid,                     & 
            indices,                  & 
            lowerIdx,                 &
            upperIdx,                 &
            sw,                       &
            se,                       &
            nw                        &
        )
            type(NdNode),   intent(in)    :: nodePool(:)
            real(real64),   intent(in)    :: xMid, yMid
            integer(int64), intent(in)    :: lowerIdx, upperIdx
            integer(int64), intent(inout) :: indices(:), sw, se, nw
            integer(int64)                :: pivot, lo, hi

            ! pass 1: [lowerIdx..upperIdx-1] -> 
            !         [lowerIdx..pivot|pivot+1..upperIdx-1]
            lo = lowerIdx
            hi = upperIdx - 1_int64
            call dutchNationalFlag(lo, hi, nodePool, indices, 1_int64, xMid)
            if (nodePool(indices(hi))%coords(1) .le. xMid) then
                pivot = hi
            else
                pivot = hi - 1_int64
            end if

            ! pass 2: [lowerIdx..pivot|pivot+1..upperIdx-1] ->
            !         [lowerIdx..sw|sw+1..pivot|pivot+1..upperIdx-1]
            lo = lowerIdx
            hi = pivot
            call dutchNationalFlag(lo, hi, nodePool, indices, 2_int64, yMid)
            if (nodePool(indices(hi))%coords(2) .le. yMid) then
                sw = hi
            else
                sw = hi - 1_int64
            end if

            ! pass 3: [lowerIdx..sw|sw+1..pivot|pivot+1..upperIdx-1] ->
            !         [lowerIdx..sw|sw+1..pivot|pivot+1..nw|nw+1..upperIdx-1]
            lo = pivot + 1
            hi = upperIdx - 1_int64
            call dutchNationalFlag(lo, hi, nodePool, indices, 2_int64, yMid)
            if (nodePool(indices(hi))%coords(2) .le. yMid) then
                nw = hi
            else
                nw = hi - 1_int64
            end if

            ! final:  [lowerIdx..sw|sw+1..pivot|pivot+1..nw|nw+1..upperIdx-1] ->
            !         [lowerIdx..sw|sw+1..se   |se+1..nw   |nw+1..upperIdx-1]
            se = pivot

        end subroutine partitionQuadrants


        !> for an octree, each root has 8 subtrees split into octants:
        !!
        !!   1. North-East-Up:   (x ≥ xMid) && (y ≥ yMid) && (z ≥ zMid)
        !!
        !!   2. North-East-Down: (x ≥ xMid) && (y ≥ yMid) && (z < zMid)
        !!
        !!   3. North-West-Up:   (x ≥ xMid) && (y < yMid) && (z ≥ zMid)
        !!
        !!   4. North-West-Down: (x ≥ xMid) && (y < yMid) && (z < zMid)
        !!
        !!   5. South-East-Up:   (x < xMid) && (y ≥ yMid) && (z ≥ zMid)
        !!
        !!   6. South-East-Down: (x < xMid) && (y ≥ yMid) && (z < zMid)
        !!
        !!   7. South-West-Up:   (x < xMid) && (y < yMid) && (z ≥ zMid)
        !!
        !!   8. South-West-Down: (x < xMid) && (y < yMid) && (z < zMid)
        !!
        !! partition: [SWD|SWU|SED|SEU|NWD|NWU|NED|NEU]
        !!
        !! bounds:    
        !! 
        !! [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..seu | seu+1..nwd | nwd+1..nwu | nwu+1..ned | ned+1..upperIdx-1]
        !!
        !! @param[in]    nodePool the tree's node pool
        !! @param[in]    xMid     midpoint along x axis
        !! @param[in]    yMid     midpoint along y axis
        !! @param[in]    zMid     midpoint along z axis
        !! @param[inout] indices  index permutation array; rearranged in-place
        !! @param[in]    lowerIdx lower bound of the slice to partition (inclusive)
        !! @param[in]    upperIdx upper bound of the slice (exclusive);
        !!                        caller must ensure that the root is at this index
        !! @param[inout] swd      last index of the South-West-Down  partition
        !! @param[inout] swu      last index of the South-West-Up    partition
        !! @param[inout] sed      last index of the South-East-Down  partition
        !! @param[inout] seu      last index of the South-East-Up    partition
        !! @param[inout] nwd      last index of the North-West-Down  partition
        !! @param[inout] nwu      last index of the North-West-Up    partition
        !! @param[inout] ned      last index of the North-East-Down  partition
        subroutine partitionOctants( &
            nodePool,                &
            xMid,                    &
            yMid,                    &
            zMid,                    &
            indices,                 &
            lowerIdx,                &
            upperIdx,                &
            swd,                     &
            swu,                     &
            sed,                     &
            seu,                     &
            nwd,                     &
            nwu,                     &
            ned                      &
        )
            type(NdNode),   intent(in)    :: nodePool(:)
            real(real64),   intent(in)    :: xMid, yMid, zMid
            integer(int64), intent(in)    :: lowerIdx, upperIdx
            integer(int64), intent(inout) :: indices(:), swd, swu, sed, seu, nwd, nwu, ned
            integer(int64)                :: lo, hi, pivot

            ! pass 1: [lowerIdx..upperIdx-1] ->
            !         [lowerIdx..pivot | pivot+1..upperIdx-1]
            lo = lowerIdx
            hi = upperIdx - 1_int64
            call dutchNationalFlag(lo, hi, nodePool, indices, 1_int64, xMid)
            if (nodePool(indices(hi))%coords(1) .le. xMid) then
                pivot = hi
            else
                pivot = hi - 1_int64
            end if

            ! pass 2: [lowerIdx..pivot | pivot+1..upperIdx-1] ->
            !         [lowerIdx..swu | swu+1..pivot | pivot+1..upperIdx-1]
            lo = lowerIdx
            hi = pivot
            call dutchNationalFlag(lo, hi, nodePool, indices, 2_int64, yMid)
            if (nodePool(indices(hi))%coords(2) .le. yMid) then
                swu = hi
            else
                swu = hi - 1_int64
            end if

            ! pass 3: [lowerIdx..swu | swu+1..pivot | pivot+1..upperIdx-1] ->
            !         [lowerIdx..swu | swu+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1]
            lo = pivot + 1
            hi = upperIdx - 1_int64
            call dutchNationalFlag(lo, hi, nodePool, indices, 2_int64, yMid)
            if (nodePool(indices(hi))%coords(2) .le. yMid) then
                nwu = hi
            else
                nwu = hi - 1_int64
            end if

            ! pass 4: [lowerIdx..swu | swu+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1] ->
            !         [lowerIdx..swd | swd+1..swu   | swu+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1]
            lo = lowerIdx
            hi = swu
            call dutchNationalFlag(lo, hi, nodePool, indices, 3_int64, zMid)
            if (nodePool(indices(hi))%coords(3) .le. zMid) then
                swd = hi
            else
                swd = hi - 1_int64
            end if

            ! pass 5: [lowerIdx..swd | swd+1..swu | swu+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1] ->
            !         [lowerIdx..swd | swd+1..swu | swu+1..sed   | sed+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1]
            lo = swu + 1
            hi = pivot
            call dutchNationalFlag(lo, hi, nodePool, indices, 3_int64, zMid)
            if (nodePool(indices(hi))%coords(3) .le. zMid) then
                sed = hi
            else
                sed = hi - 1_int64
            end if

            ! pass 6: [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..pivot | pivot+1..nwu | nwu+1..upperIdx-1] ->
            !         [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..pivot | pivot+1..nwd | nwd+1..nwu | nwu+1..upperIdx-1]
            lo = pivot + 1
            hi = nwu
            call dutchNationalFlag(lo, hi, nodePool, indices, 3_int64, zMid)
            if (nodePool(indices(hi))%coords(3) .le. zMid) then
                nwd = hi
            else
                nwd = hi - 1_int64
            end if

            ! pass 7: [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..pivot | pivot+1..nwd | nwd+1..nwu | nwu+1..upperIdx-1] ->
            !         [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..pivot | pivot+1..nwd | nwd+1..nwu | nwu+1..ned | ned+1..upperIdx-1]
            lo = nwu + 1
            hi = upperIdx - 1_int64
            call dutchNationalFlag(lo, hi, nodePool, indices, 3_int64, zMid)
            if (nodePool(indices(hi))%coords(3) .le. zMid) then
                ned = hi
            else
                ned = hi - 1_int64
            end if

            ! final:  [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..pivot | pivot+1..nwd | nwd+1..nwu | nwu+1..ned | ned+1..upperIdx-1] ->
            !         [lowerIdx..swd | swd+1..swu | swu+1..sed | sed+1..seu   | seu+1..nwd   | nwd+1..nwu | nwu+1..ned | ned+1..upperIdx-1]
            seu = pivot
        end subroutine partitionOctants

        !> Two-pointer partition of indices(lo:hi) around mid along coordinate dim.
        !! On exit, indices(lo:hi-1) ≤ mid and indices(hi:) > mid along dim.
        !! lo and hi converge to the split point; caller reads hi as the first "north" index.
        subroutine dutchNationalFlag(lo, hi, nodePool, indices, dim, mid)
            integer(int64), intent(inout) :: lo, hi, indices(:)
            type(NdNode),   intent(in)    :: nodePool(:)
            integer(int64), intent(in)    :: dim
            real(real64),   intent(in)    :: mid
            integer(int64)                :: tmp
            do while (lo .lt. hi)
                do while((lo .lt. hi) .and. (nodePool(indices(lo))%coords(dim) .le. mid))
                    lo = lo + 1_int64
                end do 
                do while ((lo .lt. hi) .and. (nodePool(indices(hi))%coords(dim)) .gt. mid)
                    hi = hi - 1_int64
                end do
                if (lo < hi) then 
                    tmp = indices(lo)
                    indices(lo) = indices(hi)
                    indices(hi) = tmp
                end if
            end do
        end subroutine dutchNationalFlag
        
end submodule QuOcTreeBuild

