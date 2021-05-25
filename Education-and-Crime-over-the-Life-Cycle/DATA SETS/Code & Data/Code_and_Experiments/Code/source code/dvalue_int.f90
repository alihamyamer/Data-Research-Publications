! Set of functions that return the expected marginal utility of consumption conditional
! on the current income realization
! i.e. before uncertainty over avictimization and 
! apprehension (if criminal) is resolved

! DVfunction interpolates over both wealth AND other continuous state variables
! DVfunction_CA interpolates ONLY over wealth
! DVfunction_D does not interpolate at all (at nodes for all variables)

REAL(long) FUNCTION dVfunction (ledu,lage,lwealth,i_zet,i_alp,lstate,l_crim)
	use global1
	use global2
	use procs
	implicit none
	
	real(long) , intent(in)	:: lwealth, i_alp,l_crim
	integer, intent (in)	:: lage, ledu, i_zet, lstate

	integer		:: i_al,i_ah,i_cl,i_ch,i_fl,i_fh,jail_l,jail_h,l_stind,fzet,i
	real(long)	:: a_l,a_h,c_l,c_h,f_l,f_h,t,v,z,taj,dcondval(NGPST-1),putil
	real(long)  :: valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount



        if (lage>lifet) then
	    dVfunction = 0.0d0

        else    ! lage<=lifet
	
	
	! Figure out what grid points over wealth we are between
	CALL sial (lwealth, lage, i_al)
	
	IF (i_al<1) THEN ; PRINT *, "ial too low";	PRINT *, "wealth", lwealth
	PRINT *, "zeta", i_zet	; PRINT *, "age", lage
	PRINT *, "previous consumption", consumption;	PRINT *, "stop"
	i_al = 1
	ELSEIF (i_al>ngpa-1) THEN;	PRINT *, "ial too high";	
        ENDIF

    i_ah = i_al+1
	a_l = agrid(i_al, lage)
	a_h = agrid(i_ah, lage)
	t = (max(lwealth,agrid(1, lage))-a_l)/(a_h-a_l)

	if (lage>=retage) then !ability and crim_fixed are irrelevant for pensioners
	    v = 0.d0   
	    z = 0.d0
	    i_fl = 1
	    i_fh = i_fl+1
	    jail_l = 1
	    jail_h = jail_l+2
	    continue
	else

	    ! Figure out what grid points on the persistent shocks grid should be interpolated
	    CALL sifl (i_alp, i_fl)
	    i_fh = i_fl+1

	    f_l = fgrid(i_fl)
	    f_h = fgrid(i_fh)

	    v = (i_alp-f_l)/(f_h-f_l)
    
        ! Figure out what grid points on the crime  grid should be interpolated
	    CALL sicl (l_crim, i_cl)
	    i_ch = i_cl+1

	    c_l = cgrid(i_cl)
	    c_h = cgrid(i_ch)
	    ! Finds jail index counterpart (for somebody OUT OF JAIL) for crime index
        jail_l = i_cl*2-1
	    jail_h = i_ch*2-1

        if (cgrid(1)==cgrid(maxjail/2)) then
            z = 1.d0
        else
	        z = (l_crim-c_l)/(c_h-c_l)
	    endif
    endif
	if (lstate>=0) then ! Non-Students
	do l_stind=2,NGPST	
    
    ! Derivative of value function for somebody NOT in jail in the current period
	dcondval(l_stind-1) =z*( (1.d0-t)*(1.d0-v)*dval(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_h) + &
							(1.d0-t)*v*dval(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_h) + &
						t*(1.d0-v) * dval(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_h) + &
						t*v * dval(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_h) ) + &
						(1.d0-z)* ( (1.d0-t)*(1.d0-v)*dval(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_l) + &
							(1.d0-t)*v*dval(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_l) + &
						t*(1.d0-v) * dval(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_l) + &
						t*v * dval(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_l) )

	continue
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		dVfunction = PI_V(2)*dcondval(3) + PI_V(1)*dcondval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals IN JAIL have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Derivative of value function for somebody in jail 
     !in the current period (jail_l and jail_h are EVEN)
    piece = z*( (1.d0-t) * (1.d0-v) * dval(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h+1) + &
          	(1.d0-t) * v * dval(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h+1) + &
		t * (1.d0-v) * dval(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h+1) + &
		t * v * dval(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h+1) ) + & 
            (1.d0-z)*( (1.d0-t) * (1.d0-v) * dval(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l+1) + &
          	(1.d0-t) * v * dval(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l+1) + &
		t * (1.d0-v) * dval(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l+1) + &
		t * v * dval(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l+1) )

        fullterm: if (prisT>0) then

      	dVfunction = PI_A(2,lstate)*(PI_V(2)*dcondval(l_stind-1)+PI_V(1)* &
             dcondval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        dVfunction = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*dcondval(l_stind-1)+PI_V(1)*dcondval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	dVfunction =	z*( (1.d0-t)*(1.d0-v)* dval(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h) + &
						   (1.d0-t) * v * dval(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h) + &
						t * (1.d0-v) * dval(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h) + &
						t * v * dval(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h)) + & 
				(1.d0-z)*( (1.d0-t)*(1.d0-v)* dval(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l) + &
						   (1.d0-t) * v * dval(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l) + &
						t * (1.d0-v) * dval(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l) + &
						t * v * dval(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l))		
    continue
    endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

dVfunction = PI_V(2)*dcondval(3)+PI_V(1)*dcondval(4)

else  ! This is retirement age and pensioners cannot be robbed

dVfunction = dcondval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION dVfunction
!----------------------------------------------------------------------------

REAL(long) FUNCTION dVfunctionCA (ledu,lage,lwealth,li_zet,li_alp,lstate,lcr_st)
	use global1
	use global2
	use procs
	implicit none
	  
	real(long) , intent(in)	:: lwealth
	integer, intent (in)	:: lage, ledu,li_zet,li_alp,lstate,lcr_st

	integer		:: i_al, i_ah,l_stind, fzet,i
	real(long)	:: a_l,a_h,t,dcondval(NGPST-1),putil,valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount



        if (lage>lifet) then
	    dVfunctionCA = 0.0d0

        else    ! lage<=lifet
	
	! Figure out what grid points over wealth we are between
	CALL sial (lwealth, lage, i_al)
	
	IF (i_al<1) THEN ; PRINT *, "ial too low";	PRINT *, "wealth", lwealth
	PRINT *, "zeta", li_zet	; PRINT *, "age", lage
	PRINT *, "previous consumption", consumption;	PRINT *, "stop"
	i_al = 1
	ELSEIF (i_al>ngpa-1) THEN;	PRINT *, "ial too high";	
    ENDIF

    i_ah = i_al+1
	a_l = agrid(i_al, lage)
	a_h = agrid(i_ah, lage)

	t = (max(lwealth,agrid(1, lage))-a_l)/(a_h-a_l)
	
	if (lstate>=0) then ! Non-Students
	do l_stind=2,NGPST	
    
    ! Derivative of value function for somebody NOT in jail in the current period
	dcondval(l_stind-1) =	(1.d0-t)*dval(ledu, lage, i_al, li_zet, li_alp, l_stind,lcr_st*2-1) + &
							t*dval(ledu, lage, i_ah,li_zet,li_alp, l_stind,lcr_st*2-1)
	continue
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		dVfunctionCA = PI_V(2)*dcondval(3) + PI_V(1)*dcondval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals IN JAIL have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Derivative of value function for somebody in jail in the current period
     piece = (1.d0-t)*dval(ledu,lage,i_al,li_zet,li_alp,l_stind,lcr_st*2) + &
		t * dval(ledu,lage,i_ah,li_zet,li_alp,l_stind,lcr_st*2)

        fullterm: if (prisT>0) then

      	dVfunctionCA = PI_A(2,lstate)*(PI_V(2)*dcondval(l_stind-1)+PI_V(1)* &
             dcondval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        dVfunctionCA = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*dcondval(l_stind-1)+PI_V(1)*dcondval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


  	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	dVfunctionCA =	(1.d0-t) * dval(ledu,lage,i_al,li_zet,li_alp,l_stind,lcr_st*2-1) + &
						   t * dval(ledu,lage,i_ah,li_zet,li_alp,l_stind,lcr_st*2-1)
    continue
    endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

dVfunctionCA = PI_V(2)*dcondval(3)+PI_V(1)*dcondval(4)

else  ! This is retirement age and pensioners cannot be robbed

dVfunctionCA = dcondval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION dVfunctionCA
!----------------------------------------------------------------------------
REAL(long) FUNCTION dVfunctionD (ledu,lage,li_we,li_zet,li_alp,lstate,lcr_st)
	use global1
	use global2
	use procs
	implicit none
	
	integer, intent (in)	:: lage, ledu,li_we,li_zet,li_alp,lstate,lcr_st

	integer		:: l_stind, fzet,i
	real(long)	:: taj,dcondval(NGPST-1),putil,valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount

   dcondval =0.d0 

   if (lage>lifet) then
	    dVfunctionD = 0.0d0

    else    ! lage<=lifet
	
	
    if (lstate>=0) then ! Non-Students
	do l_stind=2,NGPST	
        ! Derivative of value function for somebody NOT in jail in the current period
	    dcondval(l_stind-1) =	dval(ledu, lage,li_we, li_zet, li_alp, l_stind,lcr_st*2-1)
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		dVfunctionD = PI_V(2)*dcondval(3) + PI_V(1)*dcondval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals IN JAIL have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Derivative of value function for somebody in jail in the current period
     piece = dval(ledu,lage,li_we,li_zet,li_alp,l_stind,lcr_st*2)
     
        fullterm: if (prisT>0) then

      	dVfunctionD = PI_A(2,lstate)*(PI_V(2)*dcondval(l_stind-1)+PI_V(1)* &
             dcondval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        dVfunctionD = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*dcondval(l_stind-1)+PI_V(1)*dcondval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


  	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind = 1
	dVfunctionD =	dval(ledu,lage,li_we,li_zet,li_alp,l_stind,lcr_st*2-1) 
    continue
    endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

dVfunctionD = PI_V(2)*dcondval(3)+PI_V(1)*dcondval(4)

else  ! This is retirement age and pensioners cannot be robbed

dVfunctionD = dcondval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION dVfunctionD

