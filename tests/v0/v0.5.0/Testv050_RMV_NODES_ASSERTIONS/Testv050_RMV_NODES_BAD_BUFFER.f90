program Testv050_RMV_NODES_BAD_BUFFER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rmvNodesBadBuffer()
    contains
        !> bufferSize=0 must error stop.
        subroutine rmvNodesBadBuffer()
            type(KdTree) :: t
            real(real64) :: init(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 1.0_real64], [2, 3])
            real(real64) :: query(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            integer      :: numRmv
            call t%build(init)
            numRmv = t%rmvNodes(coordsList=query, bufferSize=0)
            write(*, '(A)') '--- Testv050_RMV_NODES_BAD_BUFFER ---'
            write(*, '(A)') 'expected error stop for bufferSize=0'
        end subroutine rmvNodesBadBuffer
end program Testv050_RMV_NODES_BAD_BUFFER
