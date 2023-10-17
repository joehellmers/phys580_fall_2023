MODULE composition
!
!   General Description:
!   ====================
!       This module contains the information about the composition of the gas
!
!---------------------------------------------------------------------

    USE constants, ONLY :   qp
    PRIVATE             ::  qp

    CONTAINS
    REAL(qp) FUNCTION Mean_Molecular_Weight(X, Y, Z)   RESULT(mu)

!       General Description:
!       ====================
!           Calculate the mean molecular weight of the gas

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  X, Y, Z

        mu = 1.0_qp/(2.0_qp*X + 3.0_qp*Y/4.0_qp + Z/2.0_qp)           !Assume complete ionization, Eq. (10.16)
    END FUNCTION Mean_Molecular_Weight
!---------------------------------------------------------------------

    REAL(qp) FUNCTION Helium(X, Z) RESULT(Y)

!       General Description:
!       ====================
!           Calculate the amount of Helium-4 in the mixture

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  X, Z
    
        Y = 1.0_qp - X - Z
    END FUNCTION Helium
!---------------------------------------------------------------------

    REAL(qp) FUNCTION CNO(Z) RESULT(XCNO)

!       General Description:
!       ====================
!           Calculate the mass fraction of C, N, and O in the mixture

        IMPLICIT NONE
        REAL(qp),   INTENT(IN)  ::  Z

        XCNO = Z/2.0_qp
    END FUNCTION CNO
END MODULE composition
