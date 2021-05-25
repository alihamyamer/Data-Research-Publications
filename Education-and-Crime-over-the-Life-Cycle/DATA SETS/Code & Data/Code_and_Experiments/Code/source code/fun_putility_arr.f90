MODULE FUN_PUTILITY

  use global1
  use global2
  implicit none

  INTERFACE utils
     MODULE PROCEDURE utils_sc, utils_ar
  END INTERFACE

contains

  FUNCTION UTILS_sc (lage,lconsumption,lstind,ljail)
    ! Function for computing period utility from scalar
	INTEGER, INTENT (in)		:: lage,lstind,ljail
    REAL (long),  INTENT (in)	:: lconsumption
    REAL (long)					:: utils_sc
    REAL (long)					:: lbequest
    REAL (long)					:: lpar

    utils_sc = inada 

    lpar = 1.d0
    if (lage==beqage .and. mod(jail,2)==1) lpar = psi

    IF (sigma == 1.d0) THEN ! LOG UTILITY CASE
       if (lconsumption > CLOW) then 
          utils_sc  = LOG(lpar*lconsumption)
       endif
    ELSE  ! NON-LOG UTILITY
       if (lconsumption > CLOW) then 
          utils_sc  = (1.d0/(1.d0-sigma))*(lpar*lconsumption)**(1.d0-sigma)
       endif
    ENDIF

    if (lage==beqage .and. mod(jail,2)==1) then
       ! Age= beqage and out of jail. Inter-vivos transfer of:
       ! 1) a FRACTION (1-psi) of consumption expenditure (Storesletten)
       ! 2) bequest=max(0,gamma_1^(1/eta)*c^(sigma/eta)-gamma_2)
       ! Warm glow utility term 
       IF (eta == 1.d0) THEN  
          if (lconsumption > CLOW) then
             lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
             utils_sc = utils_sc + gamma_1*LOG(gamma_2+lbequest) 
          endif
       ELSE
          if (lconsumption > CLOW) then
             lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
             utils_sc = utils_sc + gamma_1*(gamma_2+lbequest)**(1.d0-eta)/(1.d0-eta)
          endif
       END IF
    ENDIF


    utils_sc = MAX(utils_sc,inada) ! LOWER BOUND ON PERIOD UTILITY

  END FUNCTION UTILS_sc

  FUNCTION UTILS_ar (lage,lconsumption,lstind,ljail)
    ! Function for computing period utility
	INTEGER, INTENT (in)		:: lage,lstind,ljail
	REAL (long), DIMENSION(:), INTENT (in)	:: lconsumption
	REAL (long), DIMENSION(SIZE(lconsumption))	:: utils_ar
    REAL (long), DIMENSION(SIZE(lconsumption))	:: lbequest
    real (long):: lpar

    utils_ar = inada 

    lpar = 1.d0
    if (lage==beqage .and. mod(jail,2)==1) lpar = psi

    IF (sigma == 1.d0) THEN ! LOG UTILITY CASE
       WHERE (lconsumption > CLOW) 
          utils_ar  = LOG(lpar*lconsumption)
       endwhere
    ELSE  ! NON-LOG UTILITY
       WHERE (lconsumption > CLOW) 
          utils_ar  = (1.d0/(1.d0-sigma))*(lpar*lconsumption)**(1.d0-sigma)
       endwhere
    ENDIF

    if (lage==beqage .and. mod(jail,2)==1) then
       ! Age= beqage and out of jail. Inter-vivos transfer of:
       ! 1) a FRACTION (1-psi) of consumption expenditure (Storesletten)
       ! 2) bequest=max(0,gamma_1^(1/eta)*c^(sigma/eta)-gamma_2)
       ! Warm glow utility term 
       IF (eta == 1.d0) THEN  
          where (lconsumption > CLOW)
             lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
             utils_ar = utils_ar + gamma_1*LOG(gamma_2+lbequest) 
          endwhere
       ELSE
          where (lconsumption > CLOW)
             lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
             utils_ar = utils_ar + gamma_1*(gamma_2+lbequest)**(1.d0-eta)/(1.d0-eta)
          endwhere
       END IF
    ENDIF


    utils_ar = MAX(utils_ar,inada) ! LOWER BOUND ON PERIOD UTILITY

  END FUNCTION UTILS_ar

end MODULE FUN_PUTILITY
