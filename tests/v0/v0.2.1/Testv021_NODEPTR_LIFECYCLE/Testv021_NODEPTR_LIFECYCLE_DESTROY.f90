program Testv021_NODEPTR_LIFECYCLE_DESTROY
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call nodePtrLifecycleDestroy()
    contains

        !> checks that after destroy, NodePtr%p is null
        subroutine nodePtrLifecycleDestroy()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64, &
                3.0_real64, 4.0_real64, &
                5.0_real64, 6.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 100.0_real64)
            call res(1)%destroy()

            if (associated(res(1)%p)) then
                write(*, '(A)') '--- Testv021_NODEPTR_LIFECYCLE_DESTROY ---'
                write(*, '(A)') 'expected: associated(res(1)%p) = F'
                write(*, '(A,L2)') 'got:      associated(res(1)%p) = ', associated(res(1)%p)
                stop 1
            end if
        end subroutine nodePtrLifecycleDestroy

end program Testv021_NODEPTR_LIFECYCLE_DESTROY
