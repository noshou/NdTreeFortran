program Testv021_IS_MEMBER_FALSE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call isMemberFalse()
    contains 

        !> Checks if a node is a member after a tree is destroyed then rebuilt 
        subroutine isMemberFalse()
            
            type(KdTree)   :: t1, t2
            real(real64) :: coords(2, 3) = reshape( &
                [1.0_real64, 2.0_real64,  &
                3.0_real64, 4.0_real64,  &
                5.0_real64, 6.0_real64], [2, 3])
            type(NdNodePtr), allocatable :: res(:) 
            real(real64) :: centroid(2)  = [0.0_real64, 0.0_real64]
            type(NdNode), pointer :: n

            call t1%build(coords)
            res = t1%rNN_Centroid(centroid, 100.0_real64)
            n => res(1)%p
            call t2%build(coords)
            if (t2%isMember(n)) then 
                write(*, '(A)') '--- Testv021_IS_MEMBER_FALSE ---'
                write(*, '(A)') 'expected node not to be member, but found node to be member!'
                stop 1
            end if
        end subroutine isMemberFalse
end program Testv021_IS_MEMBER_FALSE