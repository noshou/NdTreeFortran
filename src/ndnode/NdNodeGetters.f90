submodule(NdTreeFortran) NdNodeGetters
    implicit none 
    contains 

        module procedure getData 
            data = this%data
        end procedure getData
    
        module procedure getCoords
            allocate(coords, source=this%coords)
        end procedure getCoords

        module procedure getNodeId
            id = this%nodeId
        end procedure getNodeId
end submodule NdNodeGetters