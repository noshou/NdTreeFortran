program Testv021_IS_MEMBER_TRUE
    use NdTreeFortran
    use iso_fortran_env, only: real64
    implicit none
    call isMemberTrue()
    contains 

        !> Checks if a node is a member after a tree is destroyed then rebuilt 
        subroutine isMemberTrue()
            
            type(KdTree)   :: t1
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
            if (.not. t1%isMember(n)) then 
                write(*, '(A)') '--- Testv021_IS_MEMBER_TRUE ---'
                write(*, '(A)') 'expected node to be member, but found node to not be member!'
                stop 1
            end if
        end subroutine isMemberTrue
end program Testv021_IS_MEMBER_TRUE