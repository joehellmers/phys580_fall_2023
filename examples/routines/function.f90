real function f(x)
    implicit none

    real :: x

    f = x*x

end function f

program function_example
    implicit none
    
    real :: x = 1.5
    real :: f;

    write(*,*) "Fortran function example"

    write(*,*) x, " squared is ", f(x)

end program function_example
