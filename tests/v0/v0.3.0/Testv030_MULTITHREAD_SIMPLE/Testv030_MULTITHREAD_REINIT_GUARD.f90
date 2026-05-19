program Testv030_MULTITHREAD_REINIT_GUARD
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    type(KdTree)   :: t
    real(real64) :: coords(2, 3) = reshape( &
        [1.0_real64, 2.0_real64,  &
        3.0_real64, 4.0_real64,  &
        5.0_real64, 6.0_real64], [2, 3])

    call t%build(coords)
    call t%build(coords)  ! expected: error stop
    write(*, '(A)') '--- Testv030_MULTITHREAD_REINIT_GUARD ---'
    write(*, '(A)') 'Double build should fail, but executed successfully!'
end program Testv030_MULTITHREAD_REINIT_GUARD
