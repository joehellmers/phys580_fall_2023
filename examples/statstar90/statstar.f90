PROGRAM StatStar
!
!   General Description:
!   ====================
!       StatStar computes a ZAMS model using a number of simplifying assumptions about the physics.  The code
!       is not designed to produce precise, research-quality models of ZAMS stars; rather, it is meant to 
!       illustrate many of the fundamental physical ideas discussed in
!
!           "An Introduction to Modern Astrophysics"
!           Bradley W. Carroll and Dale A. Ostlie
!           Second Edition, Addison Wesley,   Copyright 2007.
!
!       StatStar performs an inward integration of the stellar structure equations, given values for the
!       star's mass, luminosity, effective temperature, and composition.
!
!       The simplifying assumptions made here include:
!           (a) A constant composition throughout (characteristic of a ZAMS star).
!           (b) The use of the ideal gas law throughout.
!           (c) The gas is assumed to be completely ionized throughout.
!           (d) Radiation pressure is incorporated.
!           (e) Convection, when present, is taken to be purely adiabatic.
!           (f) Opacity is computed by using approximation formulae:
!               1.  Bound-free processes via Kramer's formula.
!               2.  Free-free processes via Kramer's formula.
!               3.  Electron scattering via Thomson formula.
!               4.  H- ion via fitting function.
!           (g) Surface is assumed to have P = 0, T = 0, rho = 0.
!           (h) Outermost (optically thin) zone is assumed to be radiative.
!           (i) No attempt is made to satisfy the Eddington approximation by
!               adjusting the outside boundary condition.
!
!   Modification History
!   03/31/2018  Joe Hellmers    Trying to fix negative density issues
!   04/15/2018  Joe Hellmers    Modify output for Python manipulation
!   MM/DD/YYYY  Joe Hellmers    Trying to fix negative luminosity issues
!---------------------------------------------------------------------

    USE constants, ONLY :   qp, R_Sun, L_Sun
    USE userio
    USE bc
    USE composition
    USE physics
    USE zones
    USE ode
    USE ssequations

    IMPLICIT NONE
    REAL(qp)                                ::  Msolar, Lsolar, Ms, Teff, X, Y, Z, mu
    REAL(qp)                                ::  Rsolar, Ls, Rs
    REAL(qp)                                ::  r
    REAL(qp)                                ::  L_r, P, T, M_r
    REAL(qp)                                ::  rho_0, kappa_0, P_0, T_0, epsilon_0
    REAL(qp)                                ::  dr
    REAL(qp),  DIMENSION(n)                ::  PMLT, PMLT0
    REAL(qp),                   PARAMETER   ::  dr_over_r = 1.0E-04         !Initial fractional step size
    
    REAL(qp),                   PARAMETER   ::  M_fraction_limit = 0.01_qp     !Mass fraction stop condition
    REAL(qp),                   PARAMETER   ::  L_fraction_limit = 0.10_qp     !Luminosity stop condition
    REAL(qp),                   PARAMETER   ::  r_fraction_limit = 0.02_qp     !radius stop condition
    INTEGER,                    PARAMETER   ::  maximum_zones = 10000       !Maximum number of zones allowed

    INTEGER,                    PARAMETER   ::  n_surface = 1               !Number of surface boundary zones
    INTEGER                                 ::  i                           !Zone counter
    INTEGER                                 ::  ios                         !I/O status flag
    CHARACTER(1)                            ::  all_new = "Y"               !Select new model parameter

    LOGICAL                                 ::  ok_surface, ok_core, ok_Runge
    LOGICAL,                    PARAMETER   ::  adjust_step_size = .TRUE.   !Allow variable step size

    CHARACTER(26)                           ::  format_table = '(I5, 9ES11.3, 2X, A, F5.1)'
    CHARACTER(117)                          ::  format_table2 = '(I5,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,ES11.3,A,A,A,F5.1)'
    
!---------------------------------------------------------------------

    New_model: DO
        i = 0       !Initialize zone number

        CALL Initial_Model(Msolar, Lsolar, Rsolar, Ms, Ls, Rs, Teff, X, Y, Z, all_new)
        IF (all_new == "E" .OR. all_new == "e") EXIT New_model

        OPEN(UNIT = 10, FILE = "ZAMSmodel.txt", STATUS = 'REPLACE', ACTION = 'WRITE', IOSTAT = ios)
        IF (ios /= 0) THEN
            WRITE (*,*) "Unable to open output file 'ZAMSmodel.txt' --- terminating calculation"
            EXIT New_model
        END IF

        OPEN(UNIT = 15, FILE = "model.dat", STATUS = 'REPLACE', ACTION = 'WRITE', IOSTAT = ios)
        IF (ios /= 0) THEN
            WRITE (*,*) "Unable to open output file 'model.dat' --- terminating calculation"
            EXIT New_model
        END IF

!       Write input data to the output file
        WRITE (10,'(T46,"A ZAMS Stellar Model")')
        WRITE (10,'(T46,"--------------------",/)')
        WRITE (10,'(T46,"M    = ", F11.5, " solar")') Msolar
        WRITE (10,'(T46,"L    = ", F11.5, " solar")') Lsolar
        WRITE (10,'(T46,"R    = ", F11.5, " solar")') Rsolar
        WRITE (10,'(T46,"Teff = ", F11.5, " K")')     Teff
        WRITE (10,'(T46,"X    = ", F11.5)')           X
        WRITE (10,'(T46,"Y    = ", F11.5)')           Y
        WRITE (10,'(T46,"Z    = ", F11.5)')           Z
        WRITE (10,'(//)')

!       Set up previous zone values
        Pm   = 0
        Tm   = 0
        Xm   = X
        Zm   = Z
        rm   = Rs
        taum = 0
        rhom = 0
        kappam = 0
        epsilonm = 0
        dlnPdlnT = 99.9
        rc_flag = "r"

        WRITE (10,'(" zone      r         tau     1-M_r/Ms      L_r         T          P         rho        &
                    &kap        eps    dlnPdlnT")')
        WRITE (15,'("zone,r,rScaled,tau,massFactor,L_r,LScaled,T,P,rho,kappa,eps,rc_flag,dlnPdlnT")')

        WRITE (10,format_table) i, rm, 0.0, 0.0, Ls, Tm, Pm, rhom, kappam, epsilonm, rc_flag, dlnPdlnT
        WRITE (15,format_table2) i,',',rm,',',rm/R_Sun,',',0.0,',', 0.0,',', Ls,',',Ls/L_Sun,',', Tm,',', Pm,',', rhom,',', kappam,',', epsilonm,',', rc_flag,',', dlnPdlnT        
!       Compute surface zones and step size
        Mm = Ms
        Lm = Ls
        rm = Rs
        dr = -dr_over_r*Rs
        step_size_condition = 0
        Surface_boundary: DO
            i = i + 1

!           Update last zone values
            IF (i > 1) THEN
                Mm = M_r
                Lm = L_r
                rm = r
                Pm = P
                Tm = T
                Xm = X
                Zm = Z
                taum = tau
                rhom = rho
                kappam = kappa
                epsilonm = epsilon
            END IF
        
            CALL Surface(i, Mm, Lm, rm, X, Z, dr, r, P, T, M_r, L_r, rho, kappa, epsilon, ok_surface)
            IF (.NOT. ok_surface) EXIT Surface_boundary

            tau = taum + Optical_Depth_Change(kappa, kappam, rho, rhom, r, rm)
            WRITE (10,format_table) i, r, tau, 1 - M_r/Ms, L_r, T, P, rho, kappa, epsilon, rc_flag, dlnPdlnT
            WRITE (15,format_table2) i, ',',r,',',rm/R_Sun,',', tau,',', 1 - M_r/Ms,',', L_r,',',L_r/L_Sun,',', T,',', P,',', rho,',', kappa,',', epsilon,',', rc_flag,',', dlnPdlnT

            IF (i == n_surface) EXIT Surface_boundary
        END DO Surface_boundary

        Satisfactory_Surface: IF (ok_surface) THEN
!           Load array of first derivatives to start the general inward integration
            Y        = Helium(X, Z)
            mu       = Mean_Molecular_Weight(X, Y, Z)
            gamma    = Specific_Heat_Ratio()
            dlnPdlnT = PTgradient(Pm, P, Tm, T)
            
            dfdr0(1) = dPdr(M_r, rho, r)
            dfdr0(2) = dMdr(r, rho)
            dfdr0(3) = dLdr(r, rho, epsilon)
            dfdr0(4) = dTdr(kappa, rho, T, L_r, r, mu, M_r, gamma, dlnPdlnT)

!           Main inward integration loop
            Main_Loop: DO
                i = i + 1

!               Update last zone values
                Mm = M_r
                Lm = L_r
                Pm = P
                Tm = T
                Xm = X
                Zm = Z
                rm = r
                taum = tau
                rhom = rho
                kappam = kappa
                epsilonm = epsilon
                PMLT0 = (/ Pm, Mm, Lm, Tm /)
                
                
                CALL RK_4(n, rm, dr, PMLT0, PMLT, dfdr0, Structure_Eqns, ok_Runge)
                        if  (PMLT(3) .lt. 0.0_qp) then
                    print *,"BAD LUMINOSITY"
                    print *,"StatStar: PMLT(3) = ",PMLT(3)
                    STOP
                end if        
                
                IF (.NOT. ok_Runge) EXIT Main_Loop

!               Results from the current step
                P   = PMLT(1)
                M_r = PMLT(2)
                L_r = PMLT(3)
                T   = PMLT(4)

                tau = taum + Optical_Depth_Change(kappa, kappam, rho, rhom, rm + dr, rm)
                
                WRITE (10,format_table) i, r, tau, 1-M_r/Ms, L_r, T, P, rho, kappa, epsilon, rc_flag, dlnPdlnT
                WRITE (15,format_table2) i, ',',r,',',rm/R_Sun,',', tau,',', 1-M_r/Ms,',', L_r,',',L_r/L_Sun,',', T,',', P,',', rho,',', kappa,',', epsilon,',', rc_flag,',', dlnPdlnT

                IF ((M_r/Ms < M_fraction_limit .OR. L_r/Ls < L_fraction_limit .OR. r/Rs < r_fraction_limit) &
                        .OR. T < 0.0_qp .OR. P < 0.0_qp) EXIT Main_Loop
                IF (i > maximum_zones) THEN
                    CALL Too_Many_Zones(Msolar, Ms, M_r, Lsolar, Ls, L_r, r, Rs, Rsolar, Teff, X, Y, Z, P_0, T_0, rho_0, kappa_0, epsilon_0, rc_flag)
                    ok_Runge = .FALSE.
                    EXIT Main_Loop
                END IF
                
!               Is it time to change step size?
                IF (adjust_step_size) THEN
                    SELECT CASE (step_size_condition)
                        CASE(0)
                            IF (M_r < 0.99_qp*Ms) THEN
                                dr = -Rs/100.0_qp
                                step_size_condition = 1
                            END IF
                        CASE(1)
                            IF (ABS(dr) > r/20.0_qp) THEN
                                dr = dr/100.0_qp
                                step_size_condition = 2
                            END IF
                    END SELECT
                END IF
                r = r + dr
            END DO Main_Loop

            Core_Extrapolation: IF (ok_Runge) THEN
!               Determine core conditions
                i = i + 1
                CALL Core(M_r, L_r, P, T, X, Z, r, P_0, T_0, rho_0, kappa_0, epsilon_0, rc_flag, dlnPdlnT, ok_core)
                IF (.NOT. ok_core) THEN
                    WRITE (*,'(/,"WARNING:  There was a problem with the core extrapolation routine",/)')
                END IF

                tau = tau + Optical_Depth_Change(kappa_0, kappa, rho_0, rho, 0.0_qp, r)
                !WRITE (10,format_table) i, 0, tau, 1-M_r/Ms, L_r, T_0, P_0, rho_0, kappa_0, epsilon_0, rc_flag, dlnPdlnT
                !WRITE (15,format_table2) i,',', 0,',',0.0/R_Sun,',', tau,',', 1-M_r/Ms,',', L_r,',',L_r/L_Sun,',', T_0,',', P_0,',', rho_0,',', kappa_0,',', epsilon_0,',', rc_flag,',', dlnPdlnT
                WRITE (15,format_table2) i, ',',0.0_qp,',',0/R_Sun,',', tau,',', 1-M_r/Ms,',', L_r,',',L_r/L_Sun,',', T_0,',', P_0,',', rho_0,',', kappa_0,',', epsilon_0,',', rc_flag,',', dlnPdlnT

!               Write initial and final conditions to the screen
                CALL Final_Results(i, Msolar, Ms, M_r, Lsolar, Ls, L_r, r, Rs, Rsolar, Teff, X, Y, Z, &
                        P, T, rho, kappa, epsilon, P_0, T_0, rho_0, kappa_0, epsilon_0, rc_flag)
            END IF Core_Extrapolation
        END IF Satisfactory_Surface

!       Does the user want to compute a new model?
        all_new = "Y"
        CALL Change_Model(Msolar, Lsolar, Rsolar, Ms, Ls, Rs, Teff, X, Y, Z, all_new)
        CLOSE (UNIT = 10, IOSTAT = ios)
        IF (ios /= 0) THEN
            WRITE (*,*) "Unable to close the output file - the new model calculation is being aborted"
            EXIT New_model
        END IF
        IF (all_new == "E") Exit New_model
        WRITE (*, '(//)')
    END DO New_model
END PROGRAM StatStar
