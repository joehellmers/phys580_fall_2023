program getstring

    implicit none

    ! The 'parameter' key word indicates a constant value
    ! This is can only be changed while the program is running

    integer(2), parameter :: maxfilenamesize = 20

    ! strings in Fortran need to have a length defined
    character(len=maxfilenamesize) :: filename


    write(*,*) "Enter a file name (max length is ", maxfilenamesize, ")"
    read(*,*) filename
    write(*,*) "You entered '", trim(filename), "'"

    ! Question: What do you think happens if you enter a file name longer that the 
    !           maxfilenamesize?

end program getstring
