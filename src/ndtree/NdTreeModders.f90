submodule(NdTreeFortran) NdTreeModders
    use iso_fortran_env, only: int64, real64
    implicit none
    contains

        module procedure addNodes

            logical      :: isInit, hasData, rootHasData, hasRoot
            integer(int64) :: dataListSize, numNodeToAdd, dim

            call this%associatedRoot(hasRoot)
            if (hasRoot) rootHasData = this%nodePool(this%rootIdx)%hasData
            call this%getInitState(isInit)
            hasData      = present(dataList)
            numNodeToAdd = size(coordsList, 2)
            dim          = size(coordsList, 1)

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

            call this%addNodesImpl(coordsList, dataList)

        end procedure addNodes

        module procedure rmvNodes

            logical          :: isInit, hasIds, hasRad, hasCrd
            integer          :: sizeRad, sizeCrd, sizeDim, sizeIds, buffSze
            integer(int64)   :: dim
            character(len=9) :: mtr

            call this%getInitState(isInit)
            hasIds = present(ids)
            hasCrd = present(coordsList)
            hasRad = present(radii)
            dim    = this%dim
            if (.not.hasRad) then; sizeRad=0; else; sizeRad=size(radii);        end if
            if (.not.hasCrd) then; sizeCrd=0; else; sizeCrd=size(coordsList,2); end if
            if (.not.hasCrd) then; sizeDim=0; else; sizeDim=size(coordsList,1); end if
            if (.not.hasIds) then; sizeIds=0; else; sizeIds=size(ids);          end if

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

            mtr    = this%assertMetric('rmvNodes', metric)
            numRmv = this%rmvNodesImpl(coordsList, radii, ids, epsilon, mtr, buffSze)

        end procedure rmvNodes

end submodule NdTreeModders
