        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE HYBRD1__genmod
          INTERFACE 
            SUBROUTINE HYBRD1(FCN,N,X,FVEC,TOL,INFO,WA,LWA)
              INTEGER(KIND=4) :: LWA
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=4) :: X(N)
              REAL(KIND=4) :: FVEC(N)
              REAL(KIND=4) :: TOL
              INTEGER(KIND=4) :: INFO
              REAL(KIND=4) :: WA(LWA)
            END SUBROUTINE HYBRD1
          END INTERFACE 
        END MODULE HYBRD1__genmod
