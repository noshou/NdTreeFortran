program Testv050_MULTITHREAD_CONCURRENT_RMV_IS_MEMBER
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call concurrentRmvIsMember()
    contains
        !> 4 threads each work on an independent tree and exercise all three
        !! isMember paths that interact with rmvNodes:
        !!   A. node obtained BEFORE removal -> isMember=false (slow path, gone)
        !!   B. surviving node obtained BEFORE removal -> isMember=true (slow path, found)
        !!   C. surviving node obtained AFTER removal -> isMember=true (fast path)
        subroutine concurrentRmvIsMember()
            logical :: failed = .false.
            integer :: i

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    real(real64)                 :: coords(2, 3) = reshape( &
                        [0.0_real64, 0.0_real64, 5.0_real64, 0.0_real64, 0.0_real64, 5.0_real64], [2, 3])
                    real(real64)                 :: rmvQuery(2, 1) = reshape([0.0_real64, 0.0_real64], [2, 1])
                    type(NdNodePtr), allocatable :: preRes(:), postRes(:)
                    type(NdNode),    pointer     :: toRemove, beforeRmv, afterRmv
                    integer                      :: numRmv

                    call t%build(coords)

                    ! get references before removal
                    preRes = t%rNN_Centroid([0.0_real64, 0.0_real64], 0.01_real64)
                    toRemove => preRes(1)%p

                    preRes = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
                    beforeRmv => preRes(1)%p

                    numRmv = t%rmvNodes(coordsList=rmvQuery)

                    ! A: removed node must not be a member
                    if (t%isMember(toRemove)) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! B: surviving node found before removal -> slow path
                    if (.not. t%isMember(beforeRmv)) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                    ! C: surviving node found after removal -> fast path
                    postRes = t%rNN_Centroid([5.0_real64, 0.0_real64], 0.01_real64)
                    if (size(postRes) .eq. 1) then
                        afterRmv => postRes(1)%p
                        if (.not. t%isMember(afterRmv)) then
                            !$OMP CRITICAL
                            failed = .true.
                            !$OMP END CRITICAL
                        end if
                    else
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if

                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv050_MULTITHREAD_CONCURRENT_RMV_IS_MEMBER ---'
                write(*, '(A)') 'one or more threads failed an isMember check after rmvNodes'
                stop 1
            end if
        end subroutine concurrentRmvIsMember
end program Testv050_MULTITHREAD_CONCURRENT_RMV_IS_MEMBER
