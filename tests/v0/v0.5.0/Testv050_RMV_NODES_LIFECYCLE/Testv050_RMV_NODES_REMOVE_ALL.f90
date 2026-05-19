program Testv050_RMV_NODES_REMOVE_ALL
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call rmvNodesRemoveAll()
    contains
        !> Remove all 9 nodes; numRmv=9, pop=0, root not associated.
        subroutine rmvNodesRemoveAll()
            type(KdTree)   :: t
            real(real64)   :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64)   :: radii(1) = [100.0_real64]
            real(real64)   :: centre(2, 1) = reshape([1.0_real64, 1.0_real64], [2, 1])
            integer        :: numRmv
            integer(int64) :: pop
            logical        :: rootAssoc

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=centre, radii=radii)
            pop    = t%getPop()
            call t%associatedRoot(rootAssoc)

            if (numRmv .ne. 9) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_ALL ---'
                write(*, '(A,I0)') 'expected numRmv=9, got: ', numRmv
                stop 1
            end if
            if (pop .ne. 0_int64) then
                write(*, '(A)')    '--- Testv050_RMV_NODES_REMOVE_ALL ---'
                write(*, '(A,I0)') 'expected pop=0, got: ', pop
                stop 1
            end if
            if (rootAssoc) then
                write(*, '(A)') '--- Testv050_RMV_NODES_REMOVE_ALL ---'
                write(*, '(A)') 'expected root not associated after removing all nodes'
                stop 1
            end if
        end subroutine rmvNodesRemoveAll
end program Testv050_RMV_NODES_REMOVE_ALL
