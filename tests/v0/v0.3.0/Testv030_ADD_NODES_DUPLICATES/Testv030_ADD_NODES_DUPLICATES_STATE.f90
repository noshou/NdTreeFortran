program Testv030_ADD_NODES_DUPLICATES_STATE
    use NdTreeFortran
    use iso_fortran_env, only: real64, int64
    implicit none
    call duplicatesState()
    contains
        !> Duplicate coordinates must be stored as distinct nodes (pop grows by count).
        !! Build 8 nodes; add 2 duplicates.
        !! 0+2 > 0.25*(10-2)=2.0 -> FALSE: leaf insert, numMods=2.
        !! Verifies pop=10 (duplicates not deduplicated) and numMods=2.
        subroutine duplicatesState()
            type(KdTree)     :: t
            real(real64)   :: init_coords(2, 8) = reshape( &
                [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64, &
                4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
                0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64], [2, 8])
            real(real64)   :: dup_coords(2, 2) = reshape( &
                [0.0_real64, 0.0_real64, 3.0_real64, 0.0_real64], [2, 2])
            integer(int64) :: pop, numMods
            logical        :: isInit

            call t%build(init_coords)
            call t%addNodes(dup_coords)

            pop     = t%getPop()
            numMods = t%getNumMods()
            call t%getInitState(isInit)

            if (pop .ne. 10_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_DUPLICATES_STATE ---'
                write(*, '(A,I0)') 'expected pop=10 (duplicates stored), got: ', pop
                stop 1
            end if
            if (numMods .ne. 2_int64) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_DUPLICATES_STATE ---'
                write(*, '(A,I0)') 'expected numMods=2 (leaf insert), got: ', numMods
                stop 1
            end if
            if (.not. isInit) then
                write(*, '(A)')    '--- Testv030_ADD_NODES_DUPLICATES_STATE ---'
                write(*, '(A,L2)') 'expected isInit=T, got: ', isInit
                stop 1
            end if
        end subroutine duplicatesState
end program Testv030_ADD_NODES_DUPLICATES_STATE
