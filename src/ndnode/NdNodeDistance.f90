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

        module procedure bboxMinDist
            character(len=9) :: m
            integer(int64)   :: i
            real(real64)     :: d, di

            if (size(lo) .ne. size(this%coords) .or. size(hi) .ne. size(this%coords)) &
                error stop "bboxMinDist: dimension mismatch"

            if (present(metric)) then
                select case (metric)
                    case ('euclidean', 'manhattan', 'chebyshev'); m = metric
                    case default; error stop "bboxMinDist: unknown metric"
                end select
            else
                m = DEFAULT_METRIC
            end if

            select case (m)
                case ('euclidean')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = max(0.0_real64, lo(i) - this%coords(i)) + max(0.0_real64, this%coords(i) - hi(i))
                        d  = d + di * di
                    end do
                    minDist = sqrt(d)
                case ('manhattan')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        d = d + max(0.0_real64, lo(i) - this%coords(i)) + max(0.0_real64, this%coords(i) - hi(i))
                    end do
                    minDist = d
                case ('chebyshev')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = max(0.0_real64, lo(i) - this%coords(i)) + max(0.0_real64, this%coords(i) - hi(i))
                        if (di .gt. d) d = di
                    end do
                    minDist = d
            end select
        end procedure bboxMinDist

        module procedure bboxMaxDist
            character(len=9) :: m
            integer(int64)   :: i
            real(real64)     :: d, di

            if (size(lo) .ne. size(this%coords) .or. size(hi) .ne. size(this%coords)) &
                error stop "bboxMaxDist: dimension mismatch"

            if (present(metric)) then
                select case (metric)
                    case ('euclidean', 'manhattan', 'chebyshev'); m = metric
                    case default; error stop "bboxMaxDist: unknown metric"
                end select
            else
                m = DEFAULT_METRIC
            end if

            select case (m)
                case ('euclidean')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = max(abs(this%coords(i) - lo(i)), abs(this%coords(i) - hi(i)))
                        d  = d + di * di
                    end do
                    maxDist = sqrt(d)
                case ('manhattan')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        d = d + max(abs(this%coords(i) - lo(i)), abs(this%coords(i) - hi(i)))
                    end do
                    maxDist = d
                case ('chebyshev')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = max(abs(this%coords(i) - lo(i)), abs(this%coords(i) - hi(i)))
                        if (di .gt. d) d = di
                    end do
                    maxDist = d
            end select
        end procedure bboxMaxDist

        module procedure sphereMinDist
            character(len=9) :: m
            integer(int64)   :: i
            real(real64)     :: d, di

            if (size(center) .ne. size(this%coords)) &
                error stop "sphereMinDist: dimension mismatch"

            if (present(metric)) then
                select case (metric)
                    case ('euclidean', 'manhattan', 'chebyshev'); m = metric
                    case default; error stop "sphereMinDist: unknown metric"
                end select
            else
                m = DEFAULT_METRIC
            end if

            select case (m)
                case ('euclidean')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = this%coords(i) - center(i)
                        d  = d + di * di
                    end do
                    minDist = max(0.0_real64, sqrt(d) - radius)
                case ('manhattan')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        d = d + abs(this%coords(i) - center(i))
                    end do
                    minDist = max(0.0_real64, d - radius)
                case ('chebyshev')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = abs(this%coords(i) - center(i))
                        if (di .gt. d) d = di
                    end do
                    minDist = max(0.0_real64, d - radius)
            end select
        end procedure sphereMinDist

        module procedure sphereMaxDist
            character(len=9) :: m
            integer(int64)   :: i
            real(real64)     :: d, di

            if (size(center) .ne. size(this%coords)) &
                error stop "sphereMaxDist: dimension mismatch"

            if (present(metric)) then
                select case (metric)
                    case ('euclidean', 'manhattan', 'chebyshev'); m = metric
                    case default; error stop "sphereMaxDist: unknown metric"
                end select
            else
                m = DEFAULT_METRIC
            end if

            select case (m)
                case ('euclidean')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = this%coords(i) - center(i)
                        d  = d + di * di
                    end do
                    maxDist = sqrt(d) + radius
                case ('manhattan')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        d = d + abs(this%coords(i) - center(i))
                    end do
                    maxDist = d + radius
                case ('chebyshev')
                    d = 0.0_real64
                    do i = 1, size(this%coords)
                        di = abs(this%coords(i) - center(i))
                        if (di .gt. d) d = di
                    end do
                    maxDist = d + radius
            end select
        end procedure sphereMaxDist

end submodule NdNodeDistance