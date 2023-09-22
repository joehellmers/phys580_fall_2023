subroutine f(x, xsquared)
    implicit none

    real, intent(in) :: x
    real, intent(out) :: xsquared
    
    xsquared = x*x

end subroutine f

program function_example
    implicit none
    
    real :: x = 1.5
    real :: xsquared = 0.0

    write(*,*) "Fortran procedure example"
    call f(x, xsquared)
    write(*,*) x, " squared is ", xsquared
    
end program function_example
