program Testv030_MULTITHREAD_INDEPENDENT_TREES_RNN
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call independentTreesRnn()
    contains
        !> 4 threads each build their own 2D tree and do rNN_Centroid. Covers v0.2.0 scenarios.
        !! Geometry: 4 pts at corners; 1 pt at center; centroid at center r=1 finds 1 node.
        subroutine independentTreesRnn()
            real(real64) :: coords(2, 5) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64,  0.0_real64, &
                0.0_real64, 10.0_real64, 10.0_real64, 10.0_real64, &
                5.0_real64, 5.0_real64], [2, 5])
            integer      :: i
            logical      :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    type(NdNodePtr), allocatable :: res(:)
                    call t%build(coords)
                    res = t%rNN_Centroid([5.0_real64, 5.0_real64], 1.0_real64)
                    if (size(res) .ne. 1) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INDEPENDENT_TREES_RNN ---'
                write(*, '(A)') 'one or more threads got wrong rNN result'
                stop 1
            end if
        end subroutine independentTreesRnn
end program Testv030_MULTITHREAD_INDEPENDENT_TREES_RNN
