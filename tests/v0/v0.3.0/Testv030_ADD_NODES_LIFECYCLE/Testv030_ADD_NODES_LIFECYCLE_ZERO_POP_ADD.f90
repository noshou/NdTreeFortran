program Testv030_ADD_NODES_LIFECYCLE_ZERO_POP_ADD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call zeroPopsAdd()
    contains
        !> Build with zero nodes then addNodes.
        !! 0+N > 0.25*0=0.0 always triggers rebuild on first add (N > 0 is always true).
        !! Verifies state after zero-pop build, then pop/numMods/rNN after add.
        subroutine zeroPopsAdd()
            type(KdTree)                 :: t
            real(real64)               :: zero_coords(2, 0)
            real(real64)               :: new_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)
            integer(int64)             :: pop, numMods
            logical                    :: isInit, nodePoolAssoc, rootAssoc

            call t%build(zero_coords)

            pop = t%getPop()
            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)
            if (pop .ne. 0_int64 .or. .not. isInit .or. .not. nodePoolAssoc .or. rootAssoc) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_LIFECYCLE_ZERO_POP_ADD ---'
                write(*, '(A)')    'after build(zero):'
                write(*, '(A,I0)') 'expected pop=0,        got: ', pop
                write(*, '(A,L2)') 'expected isInit=T,     got: ', isInit
                write(*, '(A,L2)') 'expected nodePool=T,   got: ', nodePoolAssoc
                write(*, '(A,L2)') 'expected rootAssoc=F,  got: ', rootAssoc
                stop 1
            end if

            call t%addNodes(new_coords)

            pop     = t%getPop()
            numMods = t%getNumMods()
            if (pop .ne. 3_int64 .or. numMods .ne. 0_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_LIFECYCLE_ZERO_POP_ADD ---'
                write(*, '(A)')    'after addNodes (rebuild expected):'
                write(*, '(A,I0)') 'expected pop=3,     got: ', pop
                write(*, '(A,I0)') 'expected numMods=0, got: ', numMods
                stop 1
            end if

            ! euclidean: (0,0),(5,0),(0,5) are all within r=5 of centroid (2.5, 2.5)
            ! distances: sqrt(2.5^2+2.5^2) = sqrt(12.5) ~= 3.54
            res = t%rNN_Centroid([2.5_real64, 2.5_real64], 5.0_real64, metric='euclidean')
            if (size(res) .ne. 3) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_LIFECYCLE_ZERO_POP_ADD ---'
                write(*, '(A,I0)') 'expected 3 nodes findable after add, got: ', size(res)
                stop 1
            end if
        end subroutine zeroPopsAdd
end program Testv030_ADD_NODES_LIFECYCLE_ZERO_POP_ADD
