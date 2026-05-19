program Testv050_RMV_NODES_REMOVE_MULTIPLE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRemoveMultiple()
    contains
        !> Remove 4 corner nodes in one call; numRmv=4, pop=5.
        subroutine rmvNodesRemoveMultiple()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: corners(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=corners)
            pop    = t%getPop()

            if (numRmv .ne. 4) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_MULTIPLE ---'
                write(*, '(A,I0)') 'expected numRmv=4, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 5_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_MULTIPLE ---'
                write(*, '(A,I0)') 'expected pop=5, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRemoveMultiple
end program Testv050_RMV_NODES_REMOVE_MULTIPLE
