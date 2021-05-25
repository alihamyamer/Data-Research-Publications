        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE QRFAC__genmod
          INTERFACE 
            SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,RDIAG,ACNORM,WA)
              INTEGER(KIND=4) :: LIPVT
              INTEGER(KIND=4) :: LDA
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: A(LDA,N)
              LOGICAL(KIND=4) :: PIVOT
              INTEGER(KIND=4) :: IPVT(LIPVT)
              REAL(KIND=4) :: RDIAG(N)
              REAL(KIND=4) :: ACNORM(N)
              REAL(KIND=4) :: WA(N)
            END SUBROUTINE QRFAC
          END INTERFACE 
        END MODULE QRFAC__genmod
