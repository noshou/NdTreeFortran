program Testv050_MULTITHREAD_INTERNAL_STATE_INIT_PRESERVED
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call multithreadInternalStateInitPreserved()
    contains
        !> After 4 concurrent rmvNodes calls, tree remains initialized (isInit=T)
        !! and nodePool+root remain associated (partial removal, pop>0).
        subroutine multithreadInternalStateInitPreserved()
            type(KdTree) :: t
            real(real64) :: coords(2, 9) = reshape( &
                [0.0_real64, 0.0_real64, 1.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 2.0_real64, 1.0_real64, &
                 0.0_real64, 2.0_real64, 1.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 9])
            real(real64) :: queries(2, 4) = reshape( &
                [0.0_real64, 0.0_real64, 2.0_real64, 0.0_real64, &
                 0.0_real64, 2.0_real64, 2.0_real64, 2.0_real64], [2, 4])
            logical      :: isInit, nodePoolAssoc, rootAssoc
            integer      :: i

            call t%build(coords)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(t, queries)
            do i = 1, 4
                block
                    real(real64) :: q(2, 1)
                    integer      :: numRmv
                    q(:, 1) = queries(:, i)
                    numRmv  = t%rmvNodes(coordsList=q)
                end block
            end do
            !$OMP END PARALLEL DO

            call t%getInitState(isInit)
            call t%associatedNodePool(nodePoolAssoc)
            call t%associatedRoot(rootAssoc)

            if (.not. isInit) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_INTERNAL_STATE_INIT_PRESERVED ---'
                write(*, '(A)') 'expected isInit=T after concurrent removals'
                stop 1
            end if
            if (.not. nodePoolAssoc) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_INTERNAL_STATE_INIT_PRESERVED ---'
                write(*, '(A)') 'expected nodePool still associated (pop>0)'
                stop 1
            end if
            if (.not. rootAssoc) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_INTERNAL_STATE_INIT_PRESERVED ---'
                write(*, '(A)') 'expected root still associated (pop>0)'
                stop 1
            end if
        end subroutine multithreadInternalStateInitPreserved
end program Testv050_MULTITHREAD_INTERNAL_STATE_INIT_PRESERVED
