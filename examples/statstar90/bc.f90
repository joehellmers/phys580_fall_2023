MODULE bc
!
!   General Description:
!   ====================
!       This module contains:
!           (a) the starting conditions for the surface inward integration,
!               assuming that the surface pressure, temperature, and density are all zero.
!           (b) extrapolation formuale to estimate central conditions from the last computed zone
!
!---------------------------------------------------------------------

    USE constants, ONLY :   qp, pi, two_pi, four_pi_o3, G, a => a_rad, a_o3 => a_rad_o3, c, k => k_B, m_H
    PRIVATE             ::  qp, pi, two_pi, four_pi_o3, G, a, a_o3, c, k, m_H

    CONTAINS
    SUBROUTINE Surface(i, Ms, Ls, rm, X, Z, dr, r, P, T, M_r, L_r, rho, kappa, epsilon, good_surface)

!       General Description:
!       ====================
!           Estimate the temperature and pressure of the outermost zone from the zero boundary condition.
!           Electron scattering and H- ion contributions to the opacity are neglected for simplification.

        USE composition
        USE physics
        USE ssequations
        USE zones, ONLY   :   Mm, Lm, Pm, Tm, dlnPdlnT, gamma, rc_flag

        IMPLICIT NONE
        INTEGER,    INTENT(IN)      ::  i                                   !zone number
        REAL(qp),   INTENT(IN)      ::  X, Z
        REAL(qp),  INTENT(IN)      ::  Ms, Ls, rm
        REAL(qp),  INTENT(INOUT)   ::  dr
        REAL(qp),  INTENT(OUT)     ::  r
        REAL(qp),  INTENT(OUT)     ::  rho, epsilon, kappa, L_r, M_r, P, T
        LOGICAL,    INTENT(OUT)     ::  good_surface

        REAL(qp)                    ::  Y, mu, XCNO
        REAL(qp)                   ::  tog_bf, Aop
        REAL(qp),   PARAMETER       ::  g_ff = 1.0_qp                   !the free-free Gaunt factor is on the order of unity
        REAL(qp),   PARAMETER       ::  A_bf = 4.34E21, A_ff = 3.68E18  !Bound-free and free-free coefficients
        REAL(qp)                    ::  gamma_ratio
        REAL(qp)                    ::  kPadiabatic
        REAL(qp),   PARAMETER       ::  maximum   = 1.0E-8              !Maximum change in Ms and Ls over surface zone

        INTEGER                     ::  j
        INTEGER,    PARAMETER       ::  j_max = 50

        r = rm + dr

        Y  = Helium(X, Z)
        mu = Mean_Molecular_Weight(X, Y, Z)
        gamma = Specific_Heat_Ratio()
        gamma_ratio = gamma/(gamma - 1.0_qp)

        j = 0
        outerzone: DO
!           Compute the temperature and pressure for the radiative boundary condition
            rc_flag = "r"
            T = G*Ms*(mu*m_H/(4.25_qp*k))*(1.0_qp/r - 1.0_qp/rm)                       !Eq. (L.2); radiative assumption

            IF (i < 2) THEN
                tog_bf = 0.01_qp                                                       !Assume small value for surface
            ELSE
                tog_bf = 2.82_qp*(rho*(1.0_qp + X))**0.2_qp                            !Taken from Novotny (1973), p. 469
            END IF
            Aop = (A_bf/tog_bf)*Z*(1.0_qp+X) + A_ff*g_ff*(1.0_qp-Z)*(1.0_qp+X)          !From Eq. (9.22) and (9.23)
            P = SQRT((1.0_qp/4.25_qp)*(16.0_qp*pi/3.0_qp)*(G*Ms/Ls)*(a*c*k/(Aop*mu*m_H)))*T**4.25_qp     !Eq. (L.1)

!           If the zone is convective, recompute the adiabatic temperature and pressure
            dlnPdlnT = PTgradient(Pm, P, Tm, T)
            IF (dlnPdlnT < gamma_ratio .AND. i > 2) THEN
                rc_flag = "c"
                kPadiabatic = Pm/Tm**gamma_ratio
                T = G*Ms*(mu*m_H/(k*gamma_ratio))*(1.0_qp/r - 1.0_qp/rm)                !Eq. (L.3)
                P = kPadiabatic*T**gamma_ratio                                          !Eq. (10.83)
            END IF

!           Compute remaining surface quantities
            rho = Density(T, P, mu)
            IF (rho < 0.0_qp) THEN
                good_surface = .FALSE.
                EXIT outerzone
            END IF
            kappa   = Opacity(T, rho, X, Z)
            XCNO    = CNO(Z)
            epsilon = Nuclear(T, rho, X, Z)

!           Test to be sure that variations in M_r and L_r are not too large
            M_r = Mm + dMdr(r, rho)*dr
            L_r = Lm + dLdr(r, rho, epsilon)*dr
            IF (ABS((Ms - M_r)/Ms) < maximum .AND. ABS((Ls - L_r)/Ls) < maximum) THEN
                good_surface = .TRUE.
                EXIT outerzone
            END IF

!           If changes in M_r and L_r were too large, repeat with one-half the step size
            j = j + 1
            IF (j > j_max) THEN
                WRITE (*,*) "Unable to converge in SUBROUTINE Surface --- Exiting"
                good_surface = .FALSE.
                EXIT outerzone
            END IF
            dr = dr/2.0_qp
            r = rm + dr
        END DO outerzone

        IF (.NOT. good_surface) THEN
            WRITE (*,'("The last values obtained by SUBROUTINE Surface were: ", /, &
                    "     M_r = ", ES13.6, "   dM_r/Ms = ", ES13.6, /, &
                    "     L_r = ", ES13.6, "   dL_r/Ls = ", ES13.6)')  M_r, (Ms - M_r)/Ms, L_r, (Ls - L_r)/Ls
        END IF
    END SUBROUTINE Surface
!---------------------------------------------------------------------

    SUBROUTINE Core(M_r, L_r, P, T, X, Z, r, P_0, T_0, rho_0, kappa_0, epsilon_0, rc_flag, dlnPdlnT, good_T)

!       General Description:
!       ====================
!           This routine extrapolates from the inner-most zone to obtain estimates of core conditions in the star

        USE composition
        USE physics

        IMPLICIT NONE
        REAL(qp),       INTENT(IN)  ::  X, Z
        REAL(qp),      INTENT(IN)  ::  L_r, P, T, M_r, r
        REAL(qp),      INTENT(OUT) ::  rho_0, kappa_0, P_0, T_0, epsilon_0
        REAL(qp),      INTENT(OUT) ::  dlnPdlnT
        LOGICAL,        INTENT(OUT) ::  good_T
        CHARACTER(1),   INTENT(OUT) ::  rc_flag

        REAL(qp)                    ::  Y, mu
        REAL(qp)                   ::  dT
        REAL(qp)                    ::  gamma
        REAL(qp),       PARAMETER   ::  converged = 1.0E-8
        INTEGER                     ::  i
        INTEGER,        PARAMETER   ::  i_max = 50

        print *,"Core: M_r=", M_r
        
        rho_0     = M_r/(four_pi_o3*r**3.0_qp)             !Average density of the central ball
        P_0       = P + (two_pi/3)*G*rho_0**2.0_qp*r**2.0_qp    !Central pressure, Eq. (L.4)
        epsilon_0 = L_r/M_r                           !Average energy generation rate of the central ball
        
!       Find core temperature by Newton-Raphson method (including radiation pressure)
        Y   = Helium(X, Z)
        mu  = Mean_Molecular_Weight(X, Y, Z)

        IF (rho_0 > 0.0_qp) THEN
            i = 0
            T_0 = T
            good_T = .TRUE.
            Find_T_0: DO
                i = i + 1
                dT = -f(T_0)/dfdT(T_0)
                IF (ABS(dT/T_0) < converged) EXIT Find_T_0
                T_0 = T_0 + dT
                IF (i > i_max) THEN
                    WRITE (*,*) "Unable to converge on core temperature in SUBROUTINE Core --- Exiting"
                    good_T = .FALSE.
                    EXIT FIND_T_0
                END IF
            END DO Find_T_0
        ELSE
            T_0 = -T
            good_T = .FALSE.
        END IF

        IF (good_T) THEN
            kappa_0  = Opacity(T_0, rho_0, X, Z)
            dlnPdlnT = PTgradient(P, P_0, T, T_0)
            gamma    = Specific_Heat_Ratio()
            IF (dlnPdlnT < (gamma/(gamma - 1.0_qp))) THEN
                rc_flag = "c"
            ELSE
                rc_flag = "r"
            END IF
        ELSE
            kappa_0  = -99.9_qp
            dlnPdlnT = -99.9_qp
            rc_flag  = "*"
        END IF

        CONTAINS
            REAL(qp) FUNCTION f(T)
                IMPLICIT NONE
                REAL(qp),   INTENT(IN)  ::  T

                f = rho_0*k*T/(mu*m_H) + a_o3*T**4.0_qp - P_0    !f = Ideal Gas Law + Radiation Pressure - core P = 0
            END FUNCTION f

            REAL(qp) FUNCTION dfdT(T)
                IMPLICIT NONE
                REAL(qp),   INTENT(IN)  ::  T

                dfdT = rho_0*k/(mu*m_H) + 4.0_qp*a_o3*T**3       !df/dT
            END FUNCTION dfdT
    END SUBROUTINE Core    
END MODULE bc
