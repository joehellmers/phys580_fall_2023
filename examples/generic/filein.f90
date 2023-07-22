program filein

    implicit none
    
    real, dimension(100) :: p, q
    integer :: i

    ! opening the file for reading
    open (2, file = 'data1.dat', status = 'old')
    do i = 1,100
        read(2,*) p(i), q(i)
    end do
    close(2)
    
    do i = 1,100
        write(*,*) p(i), q(i)
    end do

end program filein
