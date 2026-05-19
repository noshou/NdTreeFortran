program Testv021_DATA_INPUT_RNN_CHEBYSHEV
    use NdTreeFortran
    use iso_fortran_env, only: real64

    call dataInput_rNN_Chebyshev()
    contains 
        
        !> expected to return exactly two nodes with data "4" and "6"
        subroutine dataInput_rNN_Chebyshev()
            type(KdTree)                 :: t
            real(real64)               :: coords(3, 6) = reshape( &
                [5.0_real64, 1.0_real64,  0.92_real64,            &
                4.0_real64, 2.0_real64,  0.42_real64,             &
                3.0_real64, 3.0_real64,  0.00003_real64,          &
                0.0_real64, 0.0_real64,  0.00000031_real64,       &
                1.0_real64, 5.0_real64, -93131913.0_real64,       &
                0.0_real64, 0.0_real64,  0.0_real64], [3, 6])
            character(len=1)           :: data(6) = ['1', '2', '3', '4', '5', '6']
            type(NdNodePtr), allocatable :: res(:)
            real(real64)               :: r
            integer                    :: i
            logical                    :: found4 = .false., found6 = .false.

            call t%build(coords, data)
            r = maxval(abs([0.0_real64, 0.0_real64, 0.00000031_real64] - [0.0_real64, 0.0_real64, 0.0_real64]))
            res = t%rNN_Centroid([0.0_real64, 0.0_real64, 0.0_real64], r, metric='chebyshev')
            
            do i = 1, size(res)
                select type (d => res(i)%p%getData())
                    type is (character(*))
                        if ((d .eq. '4') .and. (.not. found4)) then
                            found4 = .true.
                        else if ((d .eq. '4') .and. found4) then
                            write(*, '(A)') '--- dataInput_rNN_Chebyshev ---'
                            write(*,*) 'expected: nodes with data ["4", "6"], got: a node with duplicate data "4"'
                            stop 1
                        else if ((d .eq. '6') .and. (.not. found6)) then
                            found6 = .true.
                        else if ((d .eq. '6') .and. found6) then
                            write(*, '(A)') '--- dataInput_rNN_Chebyshev ---'
                            write(*,*) 'expected: nodes with data ["4", "6"], got: a node with duplicate data "6"'
                            stop 1
                        else
                            write(*, '(A)') '--- dataInput_rNN_Chebyshev ---'
                            write(*,*) 'expected: nodes with data ["4", "6"], got: a node with data "', d, '"'
                            stop 1
                        end if
                end select
            end do
            
        end subroutine dataInput_rNN_Chebyshev

end program Testv021_DATA_INPUT_RNN_CHEBYSHEV