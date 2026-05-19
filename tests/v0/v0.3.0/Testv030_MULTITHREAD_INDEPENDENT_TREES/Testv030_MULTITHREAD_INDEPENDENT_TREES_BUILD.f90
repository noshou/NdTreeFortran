program Testv030_MULTITHREAD_INDEPENDENT_TREES_BUILD
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call independentTreesBuild()
    contains
        !> 4 threads each build their own Tree from identical data.
        !! Each verifies pop=7, dim=4, and distinct treeIds.
        subroutine independentTreesBuild()
            real(real64)   :: coords(4, 7) = reshape( &
                [3.0_real64, 1.0_real64, -13.31_real64,     0.92_real64,     &
                1.0_real64, 4.0_real64,  34.14_real64,    92.0_real64,      &
                4.0_real64, 1.0_real64, -1093.3_real64,  312.0_real64,      &
                1.0_real64, 5.0_real64,  0.013_real64, 13112.0_real64,      &
                5.0_real64, 9.0_real64,  33.13_real64,    -5.13_real64,     &
                9.0_real64, 2.0_real64, 734.35_real64,     0.00_real64,     &
                2.0_real64, 6.0_real64, -93.13_real64,     0.94_real64], [4, 7])
            integer        :: i
            logical        :: failed = .false.
            integer(int64) :: treeIds(4)

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed, treeIds)
            do i = 1, 4
                block
                    type(KdTree)     :: t
                    integer(int64) :: pop, dim

                    call t%build(coords)
                    pop = t%getPop()
                    dim = t%getDim()
                    treeIds(i) = t%getTreeId()

                    if (pop .ne. 7_int64 .or. dim .ne. 4_int64) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INDEPENDENT_TREES_BUILD ---'
                write(*, '(A)') 'one or more threads got wrong pop or dim'
                stop 1
            end if
            if (treeIds(1) .eq. treeIds(2) .or. treeIds(1) .eq. treeIds(3) .or. &
                treeIds(1) .eq. treeIds(4) .or. treeIds(2) .eq. treeIds(3) .or. &
                treeIds(2) .eq. treeIds(4) .or. treeIds(3) .eq. treeIds(4)      &
                ) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INDEPENDENT_TREES_BUILD ---'
                write(*, '(A)') 'concurrent builds produced duplicate treeIds'
                stop 1
            end if
        end subroutine independentTreesBuild
end program Testv030_MULTITHREAD_INDEPENDENT_TREES_BUILD
