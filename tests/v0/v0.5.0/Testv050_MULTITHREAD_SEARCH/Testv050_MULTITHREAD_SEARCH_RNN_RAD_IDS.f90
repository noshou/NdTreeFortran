program Testv050_MULTITHREAD_SEARCH_RNN_RAD_IDS
    use NdTreeFortran
    use iso_fortran_env, only: real64
    use omp_lib
    implicit none
    call multithreadRnnRadIds()
    contains
        !> 4 threads concurrently call rNN_RadIds on a read-only tree.
        !! All must return exactly 1 node (the id-filtered centre).
        subroutine multithreadRnnRadIds()
            type(KdTree)                    :: t
            real(real64)                    :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64], [2, 5])
            real(real64)                    :: q(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
            real(real64)                    :: r(1)    = [1.1_real64]
            type(NdNodePtr), allocatable    :: origin(:)
            type(NodeId)                    :: ids(1)
            type(NdNodeBucket), allocatable :: res(:)
            integer                         :: errors

            call t%build(coords)
            origin = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
            if (size(origin) .ne. 1) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_SEARCH_RNN_RAD_IDS: setup failed'
                stop 1
            end if
            ids(1) = origin(1)%p%getNodeId()
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) SHARED(t, q, r, ids, errors) NUM_THREADS(4) &
            !$OMP   PRIVATE(res)
            res = t%rNN_RadIds(q, r, ids)
            if (size(res(1)%nodes) .ne. 1) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A,I0)') '--- Testv050_MULTITHREAD_SEARCH_RNN_RAD_IDS: errors = ', errors
                stop 1
            end if
        end subroutine multithreadRnnRadIds
end program Testv050_MULTITHREAD_SEARCH_RNN_RAD_IDS
