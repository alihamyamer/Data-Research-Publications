SUBROUTINE numderiv(n,X,Y,dY_dX)
! COMPUTES NUMERICAL DERIVATIVE OF VALUE FUNCTIONS
use global1
IMPLICIT NONE
    ! ARGUMENTS
    INTEGER, INTENT(in) :: n
    REAL(long),DIMENSION(n),INTENT(in) :: X,Y
    REAL(long),DIMENSION(n),INTENT(out) :: dY_dX
    ! LOCALS
    REAL(long), DIMENSION(n) :: X_minus,X_plus,dX,Y_minus,Y_plus,dY
    
    dX = EOSHIFT(X,1,X(n))-EOSHIFT(X,-1,X(n))
    dY = EOSHIFT(Y,1,Y(n))-EOSHIFT(Y,-1,Y(n))

    dY_dX = dY/dX 

    ! If Inada condition is binding marginal utility is infinity.
    WHERE (Y(:) < INADA+5.D-2*ABS(INADA)) ! USE A NUMBER LARGER THAN LOWER BOUND in PUTILITY
    dY_dX(:) = INADERIV         ! This is same infinite Marginal Utility as in MUOC
	ENDWHERE 
	
END SUBROUTINE numderiv
