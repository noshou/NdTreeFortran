program Testv050_RNN_RAD_EMPTY_TREE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call rnnRadEmptyTree()
    contains
        !> rNN_Rad on a tree drained to pop=0 returns empty buckets without error.
        subroutine rnnRadEmptyTree()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64], [2, 2])
            real(real64)                    :: rmvQ(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: q(2, 1)    = reshape([0.5_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)       = [100.0_real64]
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: numRmv

            call t%build(coords)
            numRmv = t%rmvNodes(coordsList=rmvQ, radii=[100.0_real64])
            res    = t%rNN_Rad(q, r)

            if (size(res) .ne. 1) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_EMPTY_TREE: expected 1 bucket, got: ', size(res)
                stop 1
            end if
            if (size(res(1)%nodes) .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_RNN_RAD_EMPTY_TREE: expected 0 nodes, got: ', &
                    size(res(1)%nodes)
                stop 1
            end if
        end subroutine rnnRadEmptyTree
end program Testv050_RNN_RAD_EMPTY_TREE
