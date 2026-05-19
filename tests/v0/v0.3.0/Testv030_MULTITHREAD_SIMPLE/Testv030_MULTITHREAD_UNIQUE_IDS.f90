program Testv030_MULTITHREAD_UNIQUE_IDS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none

    integer(int64), parameter   :: N = 8
    type(KdTree)           :: trees(N)
    real(real64)         :: coords(2, 4) = reshape( &
        [1.0_real64, 2.0_real64,  &
        3.0_real64, 4.0_real64,  &
        5.0_real64, 6.0_real64,  &
        7.0_real64, 8.0_real64], [2, 4])
    integer(int64) :: i, j, ids(N)

    !$OMP PARALLEL DO PRIVATE(i) SHARED(trees, coords)
    do i = 1, N
        call trees(i)%build(coords)
    end do
    !$OMP END PARALLEL DO

    do i = 1, N
        ids(i) = trees(i)%getTreeId()
    end do

    do i = 1, N
        do j = i + 1, N
            if (ids(i) == ids(j)) then
                write(*, '(A)') '--- Testv030_MULTITHREAD_UNIQUE_IDS ---'
                write(*, *) 'expected: all tree IDs to be unique, got collision:', ids(i), 'at indices', i, 'and', j
                stop 1
            end if
        end do
    end do

end program Testv030_MULTITHREAD_UNIQUE_IDS
