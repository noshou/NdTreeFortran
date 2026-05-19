program Testv030_MULTITHREAD_INDEPENDENT_TREES_ADD_NODES
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call independentTreesAddNodes()
    contains
        !> 4 threads each build their own tree, addNodes, and verify the result independently.
        subroutine independentTreesAddNodes()
            real(real64) :: init_coords(2, 3) = reshape( &
                [0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64, 0.0_real64, 10.0_real64], [2, 3])
            real(real64) :: new_coords(2, 2) = reshape( &
                [1.0_real64, 1.0_real64, 2.0_real64, 2.0_real64], [2, 2])
            integer      :: i
            logical      :: failed = .false.

            !$OMP PARALLEL DO NUM_THREADS(4) SCHEDULE(STATIC, 1) SHARED(failed)
            do i = 1, 4
                block
                    type(KdTree)                 :: t
                    type(NdNodePtr), allocatable :: res(:)
                    integer(int64)             :: pop

                    call t%build(init_coords)
                    call t%addNodes(new_coords)
                    pop = t%getPop()
                    res = t%rNN_Centroid([1.5_real64, 1.5_real64], 1.0_real64)

                    if (pop .ne. 5_int64 .or. size(res) .ne. 2) then
                        !$OMP CRITICAL
                        failed = .true.
                        !$OMP END CRITICAL
                    end if
                end block
            end do
            !$OMP END PARALLEL DO

            if (failed) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_INDEPENDENT_TREES_ADD_NODES ---'
                write(*, '(A)') 'one or more threads got wrong pop or rNN result after addNodes'
                stop 1
            end if
        end subroutine independentTreesAddNodes
end program Testv030_MULTITHREAD_INDEPENDENT_TREES_ADD_NODES
