program Testv021_NODEPTR_LIFECYCLE_DESTROY_DOUBLE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call nodePtrLifecycleDestroyDouble()
    contains

        !> checks that destroying a NodePtr twice does not error-stop
        subroutine nodePtrLifecycleDestroyDouble()
            type(KdTree)                 :: t
            real(real64)               :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64, &
                3.0_real64, 4.0_real64, &
                5.0_real64, 6.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:)

            call t%build(coords)
            res = t%rNN_Centroid([0.0_real64, 0.0_real64], 100.0_real64)
            call res(1)%destroy()
            call res(1)%destroy()
        end subroutine nodePtrLifecycleDestroyDouble

end program Testv021_NODEPTR_LIFECYCLE_DESTROY_DOUBLE
