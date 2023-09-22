! Put constants you want to use anywhere here
module constants
        implicit none
        integer, parameter :: dbl = 8
        
end module constants


! Various integration techniques
module integration
        use constants
        implicit none

contains
        real(dbl) function simpson13_array(farray, h, N)
                
                real(dbl), intent(in), allocatable, dimension(:) :: farray
                real(dbl), intent(in) :: h
                
                integer, intent(in) :: N
                integer :: i,cnt
                real(dbl) :: accum = 0.0D0
                
                accum = 0

                do i=2,N,2
                        accum = accum + (farray(i-2) + 4.0D0*farray(i-1) + farray(i))
                end do

                simpson13_array = (h/3.0D0)*accum
        end function simpson13_array
 
end module integration


! Here are some example functions
module example_functions
        use constants
        implicit none

contains
        real(dbl) function thinrod_em_potential(x)
                real(dbl), intent(in) :: x
                thinrod_em_potential = 1/(sqrt(x*x + 1))
        end function thinrod_em_potential

end module example_functions


! Program to example the Simpson 1/3 method
program simpson
        use constants
        use example_functions
        use integration
        implicit none

        integer :: N
        ! Declare a resizable array so I can run multiple values of N
        real(dbl), allocatable, dimension(:) :: farray
        real(dbl) :: h
        real(dbl) :: x0 = 0.0D0 ! Change this to change the lower integration bound
        real(dbl) :: x1 = 1.0D0 ! Change this for the upper integration bound
        integer   :: i,j 
        integer   :: NloopStart = 1
        integer   :: NloopCnt = 7
        real(dbl) :: result = 0.0D0

        print *,"Simpson Integration Example"
        print *,"==========================="

        N = 10
        do j=NloopStart,NloopCnt

                allocate(farray(0:N))

                ! Load function array
                h = (x1-x0)/N
                do i=0,N
                        farray(i) = thinrod_em_potential(i*h) 
                end do
        
                result = simpson13_array(farray, h, N)
                
                print *,"Result for N=",N, " is ", result
        
                deallocate(farray)
                N = N*10
        end do

end program simpson
