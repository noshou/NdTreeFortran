program Testv050_MULTITHREAD_GET_ALL_CONCURRENT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    use omp_lib
    implicit none
    call multithreadGetAll()
    contains
        !> Multiple threads call getAllNodes and getAllCoords concurrently on a
        !! read-only tree.  All calls must return consistent results: correct
        !! count and all nodes must be members.
        subroutine multithreadGetAll()
            type(KdTree)                 :: t
            real(real64)                 :: coords(2, 6) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64], [2, 6])
            integer                      :: tid, errors
            type(NdNodePtr), allocatable :: nodes(:)
            real(real64),    allocatable :: gotCoords(:,:)
            integer                      :: i

            call t%build(coords)
            errors = 0

            !$OMP PARALLEL DEFAULT(NONE) &
            !$OMP   SHARED(t, errors) &
            !$OMP   PRIVATE(tid, nodes, gotCoords, i) &
            !$OMP   NUM_THREADS(4)
            tid = omp_get_thread_num()

            nodes     = t%getAllNodes()
            gotCoords = t%getAllCoords()

            if (size(nodes) .ne. 6) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if

            if (size(gotCoords, 2) .ne. 6) then
                !$OMP ATOMIC UPDATE
                errors = errors + 1
            end if

            do i = 1, size(nodes)
                if (.not. t%isMember(nodes(i)%p)) then
                    !$OMP ATOMIC UPDATE
                    errors = errors + 1
                end if
            end do
            !$OMP END PARALLEL

            if (errors .ne. 0) then
                write(*, '(A)')    '--- Testv050_MULTITHREAD_GET_ALL_CONCURRENT ---'
                write(*, '(A,I0)') 'errors across threads: ', errors
                stop 1
            end if
        end subroutine multithreadGetAll
end program Testv050_MULTITHREAD_GET_ALL_CONCURRENT
