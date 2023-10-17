MODULE constants
!
!   General Description:
!   ====================
!
!       This module contains the most up-to-date physical and
!       astronomical constants in SI units.  This module also identifies 
!       the correct kind parameters for the current machine.
!
!       "An Introduction to Modern Astrophysics", Appendix I
!       Bradley W. Carroll and Dale A. Ostlie
!       Addison Wesley, 2007
!
!       Weber State University
!       Ogden, UT
!       modastro@weber.edu
!-------------------------------------------------------------------

    IMPLICIT NONE

!The system's precision and range
    INTEGER,    PARAMETER   ::  qp          = SELECTED_REAL_KIND(16)
    INTEGER,    PARAMETER   ::  qi          = SELECTED_INT_KIND(32)

!The smallest non-zero number and the number of significant figures
    REAL(qp),   PARAMETER   ::  tiny_qp     = TINY(1.0_qp)
    INTEGER,    PARAMETER   ::  sig_fig_qp  = PRECISION(1.0_qp)
    REAL(qp),   PARAMETER   ::  eps_qp      = 10.0_qp**(-sig_fig_qp)

!The largest number for given precision
    REAL(qp),   PARAMETER   ::  biggest_qp  = HUGE(1.0_qp)
    INTEGER(qi),PARAMETER   ::  biggest_qi  = HUGE(1_qi)

!Values related to pi and e
    REAL(qp),  PARAMETER   ::  pi          = 3.14159265358979323846264338327950_qp
    REAL(qp),  PARAMETER   ::  two_pi      = 2*pi
    REAL(qp),  PARAMETER   ::  four_pi     = 4*pi
    REAL(qp),  PARAMETER   ::  four_pi_o3  = four_pi/3
    REAL(qp),  PARAMETER   ::  pi_over_2   = pi/2
    
    REAL(qp),  PARAMETER   ::  natural_e   = 2.71828182845904523536028747135266_qp

!Conversions for radians to degrees and degrees to radians
    REAL(qp),  PARAMETER   ::  degrees_to_radians = pi/180
    REAL(qp),  PARAMETER   ::  radians_to_degrees = 180/pi

!Physical constants
    REAL(qp),   PARAMETER   ::  G           = 6.673e-11_qp
    REAL(qp),   PARAMETER   ::  c           = 2.99792458e08_qp
    REAL(qp),   PARAMETER   ::  mu_0        = four_pi*1e-07_qp
    REAL(qp),   PARAMETER   ::  epsilon_0   = 1/(mu_0*c**2)

    REAL(qp),  PARAMETER   ::  e_C         = 1.602176462e-19_qp
    REAL(qp),  PARAMETER   ::  eV          = e_C
    REAL(qp),  PARAMETER   ::  keV         = eV*1.0e3_qp
    REAL(qp),  PARAMETER   ::  MeV         = eV*1.0e6_qp
    REAL(qp),  PARAMETER   ::  GeV         = eV*1.0e9_qp
    
    REAL(qp),  PARAMETER   ::  h           = 6.62606876e-34_qp
    REAL(qp),  PARAMETER   ::  hbar        = h/two_pi

    REAL(qp),  PARAMETER   ::  k_B         = 1.3806503e-23_qp

    REAL(qp),  PARAMETER   ::  sigma       = 2*pi**5*k_B**4/(15*c**2*h**3)
    REAL(qp),  PARAMETER   ::  a_rad       = 4*sigma/c
    REAL(qp),  PARAMETER   ::  a_rad_o3    = a_rad/3
    REAL(qp), PARAMETER   ::  four_ac_o3  = 4*a_rad_o3*c
    
    REAL(qp),  PARAMETER   ::  m_e         = 9.10938188e-31_qp
    
    REAL(qp),  PARAMETER   ::  m_p         = 1.67262158e-27_qp

    REAL(qp),  PARAMETER   ::  m_n         = 1.67492716e-27_qp
    
    REAL(qp),  PARAMETER   ::  m_H         = 1.673532499e-27_qp
    
    REAL(qp),   PARAMETER   ::  u           = 1.66053873e-27_qp
    
    REAL(qp),  PARAMETER   ::  N_A         = 6.02214199e23_qp
    
    REAL(qp),   PARAMETER   ::  R_gas       = 8.314472_qp
    
    REAL(qp),  PARAMETER   ::  a_0         = four_pi*epsilon_0*hbar**2/(m_e*e_C**2)
    
    REAL(qp),  PARAMETER   ::  R_infty     = m_e*e_C**4/(64*pi**3*epsilon_0**2*hbar**3*c)
    REAL(qp),  PARAMETER   ::  R_H         = m_p/(m_e + m_p)*R_infty

!Time constants
    INTEGER(qi),PARAMETER   ::  hr          = 3600_qi
    INTEGER(qi),PARAMETER   ::  day         = 24*hr
    REAL(qp),   PARAMETER   ::  J_yr        = 365.25_qp*day

    REAL(qp),  PARAMETER   ::  yr          = 3.15581450e7_qp
    
    REAL(qp), PARAMETER   ::  T_yr        = 3.155692519e7_qp
    
    REAL(qp), PARAMETER   ::  G_yr        = 3.1556952e7_qp
    
!Astronomical length constants
    REAL(qp),  PARAMETER   ::  AU          = 1.4959787066e11_qp
    
    REAL(qp),  PARAMETER   ::  pc          = 206264.806_qp*AU
    
    REAL(qp),   PARAMETER   ::  ly          = c*J_yr

!Solar constants
    REAL(qp),  PARAMETER   ::  M_Sun       = 1.9891e30_qp
    
    REAL(qp),  PARAMETER   ::  S_Sun       = 1.365e3_qp
    
    REAL(qp),  PARAMETER   ::  L_Sun       = four_pi*AU**2*S_Sun
    
    REAL(qp),  PARAMETER   ::  R_Sun       = 6.95508e8_qp
    
    REAL(qp),  PARAMETER   ::  Te_Sun      = (L_Sun/(four_pi*R_Sun**2*sigma))**0.25_qp
    
!Solar magnitudes
    REAL(qp),   PARAMETER   ::  Mbol_Sun    =   4.74_qp
    REAL(qp),   PARAMETER   ::  MU_Sun      =   5.67_qp
    REAL(qp),   PARAMETER   ::  MB_Sun      =   5.47_qp
    REAL(qp),   PARAMETER   ::  MV_Sun      =   4.82_qp
    REAL(qp),   PARAMETER   ::  Mbol_Sun_ap = -26.83_qp
    REAL(qp),   PARAMETER   ::  MU_Sun_ap   = -25.91_qp
    REAL(qp),   PARAMETER   ::  MB_Sun_ap   = -26.10_qp
    REAL(qp),   PARAMETER   ::  MV_Sun_ap   = -26.75_qp
    REAL(qp),   PARAMETER   ::  BC_Sun      =  -0.08_qp

!Earth constants
    REAL(qp), PARAMETER   ::  M_Earth     = 5.9736e24_qp
    
    REAL(qp), PARAMETER   ::  R_Earth     = 6.378136e6_qp
    
!Unit Conversions
    REAL(qp),   PARAMETER   ::  cm          = 1e-2_qp
    REAL(qp),   PARAMETER   ::  gram        = 1e-3_qp
    REAL(qp),   PARAMETER   ::  erg         = 1e-7_qp
    REAL(qp),   PARAMETER   ::  dyne        = 1e-5_qp
    REAL(qp),   PARAMETER   ::  esu         = 3.335640952e-10_qp
    REAL(qp),   PARAMETER   ::  statvolt    = 2.997924580e2_qp
    REAL(qp),   PARAMETER   ::  gauss       = 1e-4_qp
    REAL(qp),   PARAMETER   ::  angstrom    = 1e-10_qp
    REAL(qp),   PARAMETER   ::  jansky      = 1e-26_qp
END MODULE constants
