program precisionstuff

    implicit none
    real(4) :: fx_single
    real(8) :: fx_double
    real(16) :: fx_quad

    real :: x = 0.1

    fx_single = cos(x)
    fx_double = cos(x)
    fx_quad = cos(x)

    write(*,*) "Single = ", fx_single
    write(*,*) "Double = ", fx_double
    write(*,*) "Quad = ", fx_quad

end program precisionstuff
