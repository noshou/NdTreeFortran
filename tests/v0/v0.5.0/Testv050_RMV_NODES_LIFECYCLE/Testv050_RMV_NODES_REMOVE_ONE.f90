program Testv050_RMV_NODES_REMOVE_ONE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRemoveOne()
    contains
        !> Remove a single node by exact coordinate; numRmv=1 and pop decrements by 1.
        subroutine rmvNodesRemoveOne()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: query(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=query)
            pop    = t%getPop()

            if (numRmv .ne. 1) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_ONE ---'
                write(*, '(A,I0)') 'expected numRmv=1, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 8_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_ONE ---'
                write(*, '(A,I0)') 'expected pop=8, got: ', pop
                stop 1
            end if
        end subroutine rmvNodesRemoveOne
end program Testv050_RMV_NODES_REMOVE_ONE
