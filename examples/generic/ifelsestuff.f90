program ifelsestuff

    implicit none

    real(8) :: num1, num2


    write(*,*) "Enter a couple of numbers"
    read(*,*) num1, num2

    if (num1 .eq. num2) then
        write(*,*) "The numbers are the same" 
    else
        if (num1 < num2) then
            write(*,*) "The first number is less than the second"
        else
            write(*,*) "The second number is less than the second"
        end if
    end if 

end program ifelsestuff
