MODULE ode
!
!   General Description:
!   ====================
!
!       Module ODE_Integrator contains the numerical integration routine used to integrate a set
!       of n first order linear  differential equations that depend on one independent variable, x.
!
!       This module accepts n ODEs with vectors of initial conditions.
!       A user-defined EXTERNAL function that returns the derivative of one of n specified functions is
!       required to have the form:
!
!           REAL(qp) FUNCTION Deriv(i, x, y, ok)
!
!       where 
!           (a)  all function derivative definitions are included in the single routine,
!           (b)  i designates which function value is to be returned
!           (c)  x is a scalar representing the independent variable
!           (d)  y is a vector of dimension n representing the set of dependent variables at x
!           (e)  ok is a logical flag.  If ok == .TRUE. the derivative was computed successfully.
!
!---------------------------------------------------------------------

    USE constants,   ONLY       :   qp
    PRIVATE                     ::  qp

    CONTAINS
    SUBROUTINE RK_4(n, x0, h, y0, y4, f0, f, ok)

!       General Description:
!       ====================
!           This routine uses the fourth order Runge Kutta scheme to move the solution forward
!           by one step size, h.
!
!           For details of the method, see 
!
!               Press, Teukolsky, Vetterling, and Flannery, "Numerical Recipes in Fortran 77:  The Art
!                   of Scientific Computing", second edition, Cambridge University Press, Cambridge, 1996.
!
!---------------------------------------------------------------------

        IMPLICIT NONE
        INTEGER,                    INTENT(IN)  ::  n       !Number of ODEs
        REAL(qp),                  INTENT(IN)  ::  h       !step size
        REAL(qp),                  INTENT(IN)  ::  x0      !The independent variable
        REAL(qp),  DIMENSION(:),   INTENT(IN)  ::  y0      !The array of initial y values at the start of the interval
        REAL(qp),  DIMENSION(:),   INTENT(OUT) ::  y4      !The array of results to 4th order at x + h
        REAL(qp),  DIMENSION(:),   INTENT(IN)  ::  f0      !The array of first derivatives at the start of the interval
        REAL(qp),                  EXTERNAL    ::  f       !The name of the user-defined function that returns n derivatives
        LOGICAL,                    INTENT(OUT) ::  ok      !Reports if function evaluation was successful

        INTEGER                                 ::  i

!       Temporary work arrays containing values at intermediate steps
        REAL(qp),  DIMENSION(n)                ::  k1, k2, k3, k4

!---------------------------------------------------------------------

        ok = .TRUE.
             
!       Calcualtion intermediate derivatives using the user-defined external function
        k1 = h*f0

        DO i = 1, n
            k2(i) = h*f(i, x0 + h/2.0_qp, y0 + k1/2.0_qp, ok)
            IF (.NOT. ok) RETURN
        END DO
        
        DO i = 1, n
            k3(i) = h*f(i, x0 + h/2.0_qp, y0 + k2/2.0_qp, ok)
            IF (.NOT. ok) RETURN
        END DO
        
        DO i = 1, n
            k4(i) = h*f(i, x0 + h,   y0 + k3,   ok)
            IF (.NOT. ok) RETURN
        END DO
        
!       Compute the variables for the next shell using the 4th order Runge-Kutta formula
        y4 = y0 + k1/6.0_qp + k2/3.0_qp + k3/3.0_qp + k4/6.0_qp
        !print *,"y0=",y0(3),"K(3)=",K1(3),",",K2(3),",",K3(3),",",K4(3),",",y4(3)
        !print *,"y0(3)=",y0(3)," k1(1)= ",k1(1)," y4(3)=",y4(3)
        if (y4(3) .lt. 0.0_qp) then
            print *," y4(3)=", y4(3)
        end if
    END SUBROUTINE RK_4
END MODULE ode
