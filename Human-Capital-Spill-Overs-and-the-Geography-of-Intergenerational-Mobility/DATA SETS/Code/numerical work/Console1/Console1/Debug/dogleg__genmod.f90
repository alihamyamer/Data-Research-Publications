        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE DOGLEG__genmod
          INTERFACE 
            SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)
              INTEGER(KIND=4) :: LR
              INTEGER(KIND=4) :: N
              REAL(KIND=4) :: R(LR)
              REAL(KIND=4) :: DIAG(N)
              REAL(KIND=4) :: QTB(N)
              REAL(KIND=4) :: DELTA
              REAL(KIND=4) :: X(N)
              REAL(KIND=4) :: WA1(N)
              REAL(KIND=4) :: WA2(N)
            END SUBROUTINE DOGLEG
          END INTERFACE 
        END MODULE DOGLEG__genmod
