        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE FDJAC1__genmod
          INTERFACE 
            SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,     &
     &EPSFCN,WA1,WA2)
              INTEGER(KIND=4) :: LDFJAC
              INTEGER(KIND=4) :: N
              EXTERNAL FCN
              REAL(KIND=4) :: X(N)
              REAL(KIND=4) :: FVEC(N)
              REAL(KIND=4) :: FJAC(LDFJAC,N)
              INTEGER(KIND=4) :: IFLAG
              INTEGER(KIND=4) :: ML
              INTEGER(KIND=4) :: MU
              REAL(KIND=4) :: EPSFCN
              REAL(KIND=4) :: WA1(N)
              REAL(KIND=4) :: WA2(N)
            END SUBROUTINE FDJAC1
          END INTERFACE 
        END MODULE FDJAC1__genmod
