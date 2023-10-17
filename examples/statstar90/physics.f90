MODULE physics
!
!   General Description:
!   ====================
!       This module contains the physics routines required for the construction of stellar models.
!
!---------------------------------------------------------------------

    USE constants, ONLY :   qp, a_o3 => a_rad_o3, k => k_B, m_H
    PRIVATE             ::  qp, a_o3, k, m_H

    CONTAINS
    REAL(qp) FUNCTION PTgradient(Pm, P, Tm, T) RESULT(dlnPdlnT)

!       General Description:
!       ====================
!           Compute the pressure gradient with respect to temperature to determine whether convection
!           is required. Limit value of dlnPdlnT for output purposes.

        IMPLICIT NONE
        REAL(qp),  INTENT(IN)  ::  P, Pm, Tm, T
        dlnPdlnT = ((Tm + T)/(Pm + P))*((Pm - P)/(Tm - T))
        IF (dlnPdlnT > 99.9) dlnPdlnT = 99.9
    END FUNCTION PTgradient
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Specific_Heat_Ratio() RESULT(Gamma)

!       General Description:
!       ====================
!           Compute the ratio C_P/C_V

        IMPLICIT NONE
        REAL(qp),   PARAMETER   ::  monatomic = 5/3.0_qp
        
        gamma = monatomic                               !Assume a purely monatomic gas, Eq. (10.80)
    END FUNCTION Specific_Heat_Ratio
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Density(T, P, mu) RESULT(rho)

!       General Description:
!       ====================
!           Density computes the density of the gas, assuming the ideal gas law and radiation pressure
!           A negative value for the density indicates that an error was detected in the routine

        USE zones, ONLY : step_size_condition

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  mu
        REAL(qp)               ::  P_gas, P, T

        P_gas = P - a_o3*T**4.0_qp                           !Eq. (10.20)
        IF (P_gas <= 0.0_qp .AND. T > 0.0_qp) THEN                !Do something desperate
            SELECT CASE (step_size_condition)
                CASE (0) 
                    P_gas = P
                CASE (1) 
                    P_gas = 0.001_qp*P
                CASE (2) 
                    P_gas = 0.0001_qp*P
            END SELECT
        END IF
        IF (T > 0.0_qp .AND. P_gas > 0.0_qp) THEN
            rho = P_gas*mu*m_H/(k*T)                    !Eq. (10.11)
        ELSE
            rho = -1.0_qp
        END IF
        IF (rho < 0) THEN
            print *,"T=",T," P_gas=",P_gas
            WRITE (*,'("A negative density was computed!", /, &
                     & "Sorry but I am not programmed to handle this new physics :-)", /, &
                     & "Terminating calculation with: ", /, &
                     & "         T     = ", ES13.6, /, &
                     & "         P     = ", ES13.6, /, &
                     & "         P_gas = ", ES13.6)') T, P, P_gas
        END IF
    END FUNCTION Density
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Opacity(T, rho, X, Z) RESULT(kappa)

!       General Description:
!       ====================
!           Opacity computes an approximation of the Rosseland Mean Opacity, based on approximation formulae

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  X, Z
        REAL(qp),  INTENT(IN)  ::  rho, T
        REAL(qp)                ::  kappa_es
        REAL(qp)               ::  tog_bf, kappa_bf, kappa_ff, kappa_Hminus
        REAL(qp),   PARAMETER   ::  g_ff = 1                    !the free-free Gaunt factor is on the order of unity
        REAL(qp),   PARAMETER   ::  A_bf = 4.34E21, A_ff = 3.68E18, A_es = 0.02, A_Hm = 7.9E-34

        tog_bf = 0.708_qp*(rho*(1.0_qp + X))**0.2_qp                    !Taken from Novotny (1973), p. 469

        kappa_bf = (A_bf/tog_bf)*Z*(1.0_qp + X)*rho/T**3.5_qp           !Eq. (9.22)
        kappa_ff = A_ff*g_ff*(1.0_qp - Z)*(1.0_qp + X)*rho/T**3.5_qp    !Eq. (9.23)
        kappa_es = A_es*(1.0_qp + X)                                    !Eq. (9.27)
        
        IF ((T > 3000.0_qp .AND. T < 6000.0_qp) .AND. (rho > 1E-10 .AND. rho < 1E-5) .AND. (Z > 0.001_qp .AND. Z < 0.03_qp)) THEN
            kappa_Hminus = A_Hm*(Z/0.02_qp)*SQRT(rho)*T**9.0_qp         !Eq. (9.28)
        ELSE
            kappa_Hminus = 0.0_qp
        END IF

        kappa = kappa_bf + kappa_ff + kappa_es + kappa_Hminus


    END FUNCTION Opacity
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Optical_Depth_Change(kappa, kappam, rho, rhom, r, rm) RESULT(dtau)

!       General Description:
!       ====================
!           Compute the change in optical depth across the zone

        IMPLICIT NONE
        REAL(qp),  INTENT(IN)  ::  kappa, rho, kappam, rhom, rm, r
        dtau = -(kappa*rho + kappam*rhom)*(r - rm)/2.0_qp            !Eq. (9.15)
    END FUNCTION Optical_Depth_Change
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Nuclear(T, rho, X, Z) RESULT(epsilon)

!       General Description:
!       ====================
!           Nuclear computes the nuclear energy generation rates for the proton-proton chains, the CNO cycle,
!           and helium burning.

        USE composition

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  X, Z
        REAL(qp),  INTENT(IN)  ::  rho, T
        REAL(qp)                ::  XCNO, Y
        REAL(qp)               ::  Cpp, psipp, CCNO
        REAL(qp),   PARAMETER   ::  fpp = 1.0_qp, f3a = 1.0_qp                            !screening factors
        REAL(qp)               ::  eps_pp, eps_CNO, eps_He
        REAL(qp)               ::  T6, T8
        REAL(qp),   PARAMETER   ::  onethird = 1/3.0_qp, twothirds = 2.0_qp*onethird
        REAL(qp),   PARAMETER   ::  fourthirds = 4.0_qp*onethird, fivethirds = 5.0_qp*onethird
        REAL(qp),   PARAMETER   ::  A_pp = 0.241_qp, A_CNO = 8.67E20, A_He = 50.9_qp      !reaction rate coefficients

        T6 = T*1.0E-06
        T8 = T*1.0E-08

!       PP chains (see Hansen and Kawaler, Eq. 6.65, 6.73, and 6.74)
        psipp = 1 + 1.412E8*(1.0_qp/X - 1.0_qp)*EXP(-49.98_qp*T6**(-onethird))
        Cpp = 1.0_qp + 0.0123_qp*T6**onethird + 0.0109_qp*T6**twothirds + 0.000938_qp*T6
        eps_pp = A_pp*rho*X*X*fpp*psipp*Cpp*T6**(-twothirds)*EXP(-33.80_qp*T6**(-onethird))    !Eq. (10.46)

!       CNO cycle (Kippenhahn and Weigert, Eq. 18.65)
        XCNO = CNO(Z)
        CCNO = 1.0_qp + 0.0027_qp*T6**onethird - 0.00778_qp*T6**twothirds - 0.000149_qp*T6  
        eps_CNO = A_CNO*rho*X*XCNO*CCNO*T6**(-twothirds)*EXP(-152.28_qp*T6**(-onethird))       !Eq. (10.58)

!       Helium burning (Kippenhahn and Weigert, Eq. 18.67)
        Y = Helium(X, Z)
        eps_He = A_He*rho**2*Y**3/T8**3*f3a*EXP(-44.027_qp/T8)                                 !Eq. (10.62)
!       Combined energy generation rate
        epsilon = eps_pp + eps_CNO + eps_He
    
    END FUNCTION Nuclear
    
END MODULE physics
