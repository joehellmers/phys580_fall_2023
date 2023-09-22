program dynamicarr
    implicit none

    integer :: n = 0

    real(8), allocatable, dimension(:) :: myarray
    integer :: i

    write(*,*) "Fortran Static Array example"
    
    write(*,*) "Enter the number of elements in the array:"
    read(*,*) n

    write(*,*) "The number of array elements will be ", n

    allocate(myarray(0:n-1))

    write(*,*) "Dynamic array elements"
    do i = 0,n-1
        write(*,*)  i, myarray(i)
    end do

end program dynamicarr
