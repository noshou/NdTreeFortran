program Testv030_MULTITHREAD_SEQUENTIAL_IDS
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none

    integer(int64), parameter :: N = 4
    type(KdTree)         :: trees(N)
    real(real64)       :: coords(2, 3) = reshape( &
        [1.0_real64, 2.0_real64,  &
        3.0_real64, 4.0_real64,  &
        5.0_real64, 6.0_real64], [2, 3])
    integer(int64) :: i, ids(N)

    do i = 1, N
        call trees(i)%build(coords)
        ids(i) = trees(i)%getTreeId()
    end do

    do i = 2, N
        if (ids(i) <= ids(i-1)) then
            write(*, '(A)') '--- Testv030_MULTITHREAD_SEQUENTIAL_IDS ---'
            write(*, *) 'expected: strictly increasing IDs, got:', ids(i-1), 'then', ids(i), 'at index', i
            stop 1
        end if
    end do

end program Testv030_MULTITHREAD_SEQUENTIAL_IDS
