MODULE zones
!
!   General Description:
!   ====================
!       This module holds required data from the previous and current zones
!
!---------------------------------------------------------------------

    USE constants, ONLY :   qp

    IMPLICIT NONE
    PRIVATE             ::  qp

!   Previous zone data
    REAL(qp)                               ::  Pm, Mm, Lm, Tm, rm
    REAL(qp)                               ::  Xm, Zm
    REAL(qp)                               ::  kappam, rhom, epsilonm, taum
!   Current zone data
    REAL(qp)                               ::  gamma
    REAL(qp)                               ::  rho, epsilon, kappa, dlnPdlnT, tau
    CHARACTER(1)                           ::  rc_flag

!   Number of stellar structure equations
    INTEGER,                    PARAMETER   ::  n = 4
    
!   Current step size flag
    INTEGER                                 ::  step_size_condition

!   The first derivatives from the stellar structure equations to be used by Runge Kutta routines
    REAL(qp),   DIMENSION(n)                ::  dfdr0
END MODULE zones
