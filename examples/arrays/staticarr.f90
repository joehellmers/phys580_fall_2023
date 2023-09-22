program staticarr
    implicit none

    integer, parameter :: n = 10

    real(8), dimension(n) :: myarray1based
    real(8), dimension(0:n-1) :: myarray0based
    integer :: i

    write(*,*) "Fortran Static Array example"

    write(*,*) "index 1 based array"
    do i = 1,n
        write(*,*)  i, myarray1based(i)
    end do

    write(*,*) "index 0 based array"
    do i = 0,n-1
        write(*,*)  i, myarray0based(i)
    end do

    write(*,*) "...What do you think about the contents of these arrays?"

end program staticarr
