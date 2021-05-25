FUNCTION con_beq(cons)
! From the intratemporal optimality condition bequests equal
! max(0,gamma_1**(1.d0/eta)*consumption**(sigma/eta)-gamma_2)
! The sum of consumption + bequests equals total resources const_beq 

USE global1 
USE global2

IMPLICIT NONE
REAL(long), INTENT(in):: cons
REAL(long) :: con_beq
con_beq = cons + MAX(0.d0,gamma_1**(1.d0/eta)*cons**(sigma/eta)-gamma_2) - const_beq

END FUNCTION con_beq


