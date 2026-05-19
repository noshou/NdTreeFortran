program Testv050_GET_ALL_NODES_CORRECT_COUNT
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call getAllNodesCorrectCount()
    contains
        !> getAllNodes returns exactly pop nodes.
        subroutine getAllNodesCorrectCount()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            type(NdNodePtr), allocatable :: nodes(:)
            integer(int64)               :: pop

            call t%build(coords)
            nodes = t%getAllNodes()
            pop   = t%getPop()

            if (size(nodes) .ne. int(pop)) then
                write(*, '(A)')    '--- Testv050_GET_ALL_NODES_CORRECT_COUNT ---'
                write(*, '(A,I0,A,I0)') 'expected size=', pop, ', got: ', size(nodes)
                stop 1
            end if
        end subroutine getAllNodesCorrectCount
end program Testv050_GET_ALL_NODES_CORRECT_COUNT
