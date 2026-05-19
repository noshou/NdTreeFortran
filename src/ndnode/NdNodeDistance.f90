submodule(NdTreeFortran) NdNodeDistance
    implicit none 
    contains 

        !> Performs assertion checks for distance functions
        !! @param[in] name the name of the method
        !! @param[in] coords1 the first coordinate 
        !! @param[in] coords2 the second coordinates
        subroutine assertDistance(name, coords1, coords2)
            real(kind=real64), allocatable, intent(in)  :: coords1(:), coords2(:)
            character(len=*),               intent(in)  :: name 
            
            if (.not. allocated(coords1) .or. .not. allocated(coords2)) then
                write(*, '(2A)') name, ': coords not allocated'
                error stop
            else if ((size(coords1).eq. 0) .or. (size(coords2) .eq. 0)) then
                write(*, '(2A)') name, ': axis size must be > 0'
                error stop
            else if (size(coords1) .ne. size(coords2)) then
                write(*, '(2A)') name, ': axis size mismatch'
                error stop
            end if
        end subroutine assertDistance

        module procedure euclideanDist 
            call assertDistance('euclideanDist', this%coords, that%coords)
            dist = sqrt(sum((that%coords - this%coords)**2))
        end procedure euclideanDist

        module procedure euclideanDistPoint 
            call assertDistance('euclideanDistPoint', this%coords, point)
            dist = sqrt(sum((point - this%coords)**2))
        end procedure euclideanDistPoint

        module procedure manhattanDist
            call assertDistance('manhattanDist', this%coords, that%coords)
            dist = sum(abs(that%coords - this%coords))
        end procedure manhattanDist
    
        module procedure manhattanDistPoint
            call assertDistance('manhattanDistPoint', this%coords, point)
            dist = sum(abs(point - this%coords))
        end procedure manhattanDistPoint

        module procedure chebyshevDist
            call assertDistance('chebyshevDist', this%coords, that%coords)
            dist = maxval(abs(that%coords - this%coords))
        end procedure chebyshevDist

        module procedure chebyshevDistPoint
            call assertDistance('chebyshevDistPoint', this%coords, point)
            dist = maxval(abs(point - this%coords))
        end procedure chebyshevDistPoint

end submodule NdNodeDistance