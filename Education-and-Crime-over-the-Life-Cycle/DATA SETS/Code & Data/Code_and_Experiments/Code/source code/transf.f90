REAL(long) FUNCTION transf(ledu,lage,lwealth,lmedwealth,ldraw, ltuit)

	use global1
	use global2

	use numerical_libraries


	real(long) , intent(in) :: lwealth, lmedwealth, ltuit, ldraw
	integer , intent(in) :: ledu,lage

	! Locals

	integer		:: i
	real(long)	:: pctr , rnt(1)

	! Initialize
	pctr = 0.0d0


	SELECT CASE (subregime)

	CASE (0)
    ! No subsidy
	pctr = 0.d0

	CASE (1) ! ******* Amounts based on file SummaryTuitionGrantsLoans.exe
	if (ledu==1) then
        if (lwealth<=meanstestw1) then    ! people with wealth < than median wealth
		    if (ldraw<=schshare(1)) then !
		pctr = propsub ! Transfer of the same order as the Quantum Opportunity Program
		    else
            pctr =  0.d0
		    endif
        else
		pctr =  0.d0
        endif
	else
		pctr =  0.d0
	endif

	CASE (2) ! *******
	if (ledu==1) then
		pctr = propsub ! Transfer of the same order as the Quantum Opportunity Program
	else
		pctr =  0.d0
	endif

	CASE (3) ! *******
	! This transfer policy gives money to people with less than median wealth, 
	! proportionally to their distance from the median
	if (ledu==2) then
	pctr	= max(lmedwealth - lwealth,0.0d0)
	pctr	= min(pctr / MAX(lmedwealth,1.D-2),1.d0)
	else
	pctr	= 0.d0
	endif

	CASE (4) ! *******
	! This transfer policy gives money to people with less than median wealth,
	! and the amount is the same for everyone
	if (ledu==2) then
		if (lmedwealth > lwealth) then
		pctr	= propsub
		endif
	else
		pctr	= 0.d0
	endif

    CASE (5) ! *** College transfer equal to a subsidy of $1000 in 2001 (that is, $420 in 1980)
    ! This allows to compare to the responses reported by Kane (NBER WP, 2003).
    	if (ledu==2) then
		    pctr	= 0.046d0/.097d0
	    else
		    pctr	= 0.d0
	    endif
    

	END SELECT

	! Set transfer value
	transf	= MAX(pctr * ltuit , 0.0D0)

END FUNCTION transf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

REAL(long) FUNCTION transf2(ledu,lage,lwealth,lmedwealth,ldraw, ltuit,leligible)

	use global1
	use global2

	use numerical_libraries

	real(long) , intent(in) :: lwealth, lmedwealth, ltuit, ldraw
	integer , intent(in) :: ledu,lage,leligible

	! Locals

	integer		:: i
	real(long)	:: pctr , rnt(1)

	! Initialize
	pctr = 0.0d0


	SELECT CASE (subregime)

	CASE (10) ! *******
	! This policy randomizes "in and out" of forced schooling in HS 
	if (ledu==1 .and. leligible==1) then
        if (ldraw<=schshare(1)) then !
		pctr = 1.d-8 ! very small transfer to select into "positive" schship
	    else
        pctr =  0.d0
		endif
	else
		pctr =  0.d0
	endif
	
	END SELECT

	! Set transfer value
	transf2	= MAX(pctr * ltuit , 0.0D0)

END FUNCTION transf2
