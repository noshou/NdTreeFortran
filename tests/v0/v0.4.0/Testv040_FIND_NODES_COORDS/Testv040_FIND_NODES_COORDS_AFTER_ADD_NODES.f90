program Testv040_FIND_NODES_COORDS_AFTER_ADD_NODES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call findNodesCoordsAfterAdd()
    contains
        !> rNN_Coords must find a node inserted via addNodes.
        !! Tree pop must grow by 1 after addNodes; findNodes must return the new node.
        subroutine findNodesCoordsAfterAdd()
            type(KdTree) :: t
            type(NdNodeBucket), allocatable :: res(:)
            real(real64), allocatable :: found(:)
            integer(int64) :: pop_before, pop_after
            real(real64) :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, 0.0_real64, 10.0_real64], [2, 3])
            real(real64) :: new_coord(2, 1) = reshape([5.0_real64, 5.0_real64], [2, 1])

            call t%build(init_coords)
            pop_before = t%getPop()

            call t%addNodes(new_coord)
            pop_after = t%getPop()

            if (pop_after .ne. pop_before + 1_int64) then
                write(*, '(A)')         '--- Testv040_FIND_NODES_COORDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0,A,I0)') 'expected pop ', pop_before + 1, ', got: ', pop_after
                stop 1
            end if

            res = t%rNN_Coords(new_coord, epsilon=0.5_real64)

            if (size(res(1)%nodes) .ne. 1) then
                write(*, '(A)')    '--- Testv040_FIND_NODES_COORDS_AFTER_ADD_NODES ---'
                write(*, '(A,I0)') 'expected 1 node (added node), got: ', size(res(1)%nodes)
                stop 1
            end if
            found = res(1)%nodes(1)%p%getCoords()
            if (abs(found(1) - 5.0_real64) > 1e-10_real64 .or. &
                abs(found(2) - 5.0_real64) > 1e-10_real64) then
                write(*, '(A)')       '--- Testv040_FIND_NODES_COORDS_AFTER_ADD_NODES ---'
                write(*, '(A,2F8.4)') 'expected (5,5), got: ', found(1), found(2)
                stop 1
            end if
        end subroutine findNodesCoordsAfterAdd
end program Testv040_FIND_NODES_COORDS_AFTER_ADD_NODES
