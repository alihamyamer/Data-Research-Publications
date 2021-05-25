        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE HYBRD__genmod
          INTERFACE 
            SUBROUTINE HYBRD(FCN,N,X,FVEC,XTOL,MAXFEV,ML,MU,EPSFCN,DIAG,&
     &MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,LDFJAC,R,LR,QTF,WA1,WA2,WA3,WA4)
              INTEGER(KIND=4) :: LR
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=4) :: X(N)
              REAL(KIND=4) :: FVEC(N)
              REAL(KIND=4) :: XTOL
              INTEGER(KIND=4) :: MAXFEV
              INTEGER(KIND=4) :: ML
              INTEGER(KIND=4) :: MU
              REAL(KIND=4) :: EPSFCN
              REAL(KIND=4) :: DIAG(N)
              INTEGER(KIND=4) :: MODE
              REAL(KIND=4) :: FACTOR
              INTEGER(KIND=4) :: NPRINT
              INTEGER(KIND=4) :: INFO
              INTEGER(KIND=4) :: NFEV
              REAL(KIND=4) :: FJAC(LDFJAC,N)
              REAL(KIND=4) :: R(LR)
              REAL(KIND=4) :: QTF(N)
              REAL(KIND=4) :: WA1(N)
              REAL(KIND=4) :: WA2(N)
              REAL(KIND=4) :: WA3(N)
              REAL(KIND=4) :: WA4(N)
            END SUBROUTINE HYBRD
          END INTERFACE 
        END MODULE HYBRD__genmod
