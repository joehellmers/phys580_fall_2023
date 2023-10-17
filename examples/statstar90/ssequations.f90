MODULE ssequations

!   General Description:
!   ====================
!
!       This module contains the basic equations of stellar structure.  The module also
!       contains a driver function that selects among the required equations for the 
!       Runge Kutta routines.
!---------------------------------------------------------------------

    USE constants, ONLY         :   qp, G, four_pi, a => a_rad, four_ac_o3, c, m_H, k => k_B
    USE zones, ONLY   :   n
    PRIVATE                     ::  qp, G, four_pi, a, four_ac_o3, c, m_H, k

    CONTAINS

!   Driver for stellar structure equations
    REAL(qp) FUNCTION Structure_Eqns(i, r, S, ok)   RESULT(dfdr)

        USE composition
        USE physics
        USE zones,  X => Xm, Z => Zm

        IMPLICIT NONE
        INTEGER,                    INTENT(IN)  ::  i
        REAL(qp),                  INTENT(IN)  ::  r       !independent variable
        REAL(qp),  DIMENSION(n),   INTENT(IN)  ::  S       !dependent variables
        LOGICAL,                    INTENT(OUT) ::  ok      !Returns .TRUE. if the derivative calculation was successful

        REAL(qp)                               ::  L_r, P, T, M_r
        REAL(qp)                                ::  Y, mu
        
        ok = .TRUE.

        P   = S(1)
        M_r = S(2)
        L_r = S(3)
        T   = S(4)

        Y   = Helium(X, Z)
        mu  = Mean_Molecular_Weight(X, Y, Z)
        if (L_r .lt. 0.0_qp) then
            print *,"Structure_Eqns: i=",i,"L_r=",L_r
        end if
        rho = Density(T, P, mu)
        IF (rho < 0.0_qp) THEN
            WRITE (*, '("Density calculation error in FUNCTION Structure_Eqns")')
            ok = .FALSE.
        END IF

        dfdr = 0.0_qp
        SELECT CASE (i)
            CASE (1)
                IF (ok) THEN
                    dfdr = dPdr(M_r, rho, r)
                ELSE
                    dfdr = 0.0_qp
                END IF
                dfdr0(1) = dfdr                     !Save result for next zone start

            CASE (2)
                IF (ok) THEN
                    dfdr = dMdr(r, rho)
                ELSE
                    dfdr = 0.0_qp
                END IF
                dfdr0(2) = dfdr                     !Save result for next zone start

            CASE (3)
                IF (ok) THEN
                    epsilon  = Nuclear(T, rho, X, Z)
                    dfdr     = dLdr(r, rho, epsilon)
                ELSE
                    dfdr     = 0.0_qp
                END IF
                dfdr0(3) = dfdr                     !Save result for next zone start

            CASE (4)
                IF (ok) THEN
                    kappa    = Opacity(T, rho, X, Z)
                    gamma    = Specific_Heat_Ratio()
                    dlnPdlnT = PTgradient(Pm, P, Tm, T)
                    dfdr     = dTdr(kappa, rho, T, L_r, r, mu, M_r, gamma, dlnPdlnT)
                ELSE
                    dfdr     = 0.0_qp
                END IF
                dfdr0(4) = dfdr                     !Save result for next zone start
        END SELECT
        !print *,"dfdr0(3)=",dfdr0(3)
    END FUNCTION Structure_Eqns
!---------------------------------------------------------------------

!   Hydrostatic Equilibrium
    REAL(qp) FUNCTION dPdr(M_r, rho, r)
        IMPLICIT NONE
        REAL(qp),  INTENT(IN)  ::  rho, M_r, r
        dPdr = -G*M_r*rho/r**2              !Eq. (10.6)
    END FUNCTION dPdr
!---------------------------------------------------------------------

!   Mass Conservation
    REAL(qp) FUNCTION dMdr(r, rho)
        IMPLICIT NONE
        REAL(qp),  INTENT(IN)  ::  rho, r
        dMdr = four_pi*r**2*rho             !Eq. (10.7)
    END FUNCTION dMdr
!---------------------------------------------------------------------

!   Luminosity Gradient
    REAL(qp) FUNCTION dLdr(r, rho, epsilon)
        IMPLICIT NONE
        REAL(qp),  INTENT(IN)  ::  rho, epsilon, r
        dLdr = four_pi*r**2*rho*epsilon     !Eq. (10.36)
    END FUNCTION dLdr
!---------------------------------------------------------------------

!   Temperature Gradient
    REAL(qp) FUNCTION dTdr(kappa, rho, T, L_r, r, mu, M_r, gamma, dlnPdlnT)
        
        USE zones, ONLY   : rc_flag
        
        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  mu, gamma
        REAL(qp),  INTENT(IN)  ::  rho, kappa, L_r, dlnPdlnT, T, M_r, r
        REAL(qp)                ::  gamma_ratio

        gamma_ratio = gamma/(gamma - 1)
        IF (dlnPdlnT > gamma_ratio) THEN                                !radiation criterion,   Eq. (10.95)
            dTdr = -(kappa*rho/T**3)*(L_r/(four_pi*r**2))/four_ac_o3    !radiation,             Eq. (10.68)
            rc_flag = "r"
        ELSE
            dTdr = -(1/gamma_ratio)*(mu*m_H/k)*(G*M_r/r**2)             !adiabatic convection,  Eq. (10.89)
            rc_flag = "c"
        END IF
    END FUNCTION dTdr
END MODULE ssequations
