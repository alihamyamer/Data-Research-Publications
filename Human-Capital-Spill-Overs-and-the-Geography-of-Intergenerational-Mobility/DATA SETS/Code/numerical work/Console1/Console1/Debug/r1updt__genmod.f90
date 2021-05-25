        !COMPILER-GENERATED INTERFACE MODULE: Mon Feb 08 15:12:14 2016
        MODULE R1UPDT__genmod
          INTERFACE 
            SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)
              INTEGER(KIND=4) :: LS
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: S(LS)
              REAL(KIND=4) :: U(M)
              REAL(KIND=4) :: V(N)
              REAL(KIND=4) :: W(M)
              LOGICAL(KIND=4) :: SING
            END SUBROUTINE R1UPDT
          END INTERFACE 
        END MODULE R1UPDT__genmod
