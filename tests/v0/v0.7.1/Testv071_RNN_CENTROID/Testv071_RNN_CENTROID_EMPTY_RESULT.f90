!> BallTree rNN_Centroid on a populated tree where the query returns no nodes.
program Testv071_RNN_CENTROID_EMPTY_RESULT
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none

    call run()
    contains

        !> Query should return no nodes; tree is populated
        subroutine run()
            type(BallTree)               :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                4.0_real64, 2.0_real64,  0.42_real64,             &
                3.0_real64, 3.0_real64,  0.00003_real64,          &
                0.0_real64, 0.0_real64,  0.00000031_real64,       &
                1.0_real64, 5.0_real64, -93131913.0_real64,       &
                0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            type(NdNodePtr), allocatable :: res(:)
            real(real64)               :: centroid(3) = [-1e16_real64, -1e12_real64, -4.0_real64], r = 0.000000000000031_real64

            call t%build(coords)
            res = t%rNN_Centroid(centroid, r, metric='euclidean')
            if (size(res) .ne. 0) then
                write(*, '(A)') '--- Testv071_RNN_CENTROID_EMPTY_RESULT ---'
                write(*, *) 'expected 0 nodes, got:', size(res)
                stop 1
            end if

        end subroutine run
end program Testv071_RNN_CENTROID_EMPTY_RESULT
