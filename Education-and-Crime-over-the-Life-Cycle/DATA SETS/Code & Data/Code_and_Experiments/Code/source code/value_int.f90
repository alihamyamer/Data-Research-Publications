! Set of functions that return the expected value function conditional
! on the current income realization 
! i.e. before uncertainty over avictimization and 
! apprehension (if criminal) is resolved

! Vfunction interpolates over both wealth AND other continuous state variables
! Vfunction_CA interpolates ONLY over wealth
! Vfunction_D does not interpolate at all (at nodes for all variables)

REAL(long) FUNCTION Vfunction (ledu,lage,lwealth,i_zet,i_alp,lstate,l_crim)
	use global1
	use global2
	use procs
	implicit none


	! This is the routine that interpolates the value function over shocks AND assets
	! It returns the value function given a set of states

	real(long) , intent(in)	:: lwealth, i_alp,l_crim
	integer, intent (in)	:: lage, ledu, i_zet, lstate

	integer		:: i_al,i_ah,i_cl,i_ch,i_fl,i_fh,jail_l,jail_h,l_stind,fzet,i
	real(long)	:: a_l,a_h,c_l,c_h,f_l,f_h,t,v,z,taj,condval(NGPST-1),putil
	real(long)  :: valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount

    condval = 0.d0

        if (lage>lifet) then
	    Vfunction = 0.0d0

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

    ! Value function for somebody out of jail in the current period
	condval(l_stind-1) =z*( (1.d0-t)*(1.d0-v)*val(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_h) + &
							(1.d0-t)*v*val(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_h) + &
						t*(1.d0-v) * val(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_h) + &
						t*v * val(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_h) ) + &
						(1.d0-z)* ( (1.d0-t)*(1.d0-v)*val(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_l) + &
							(1.d0-t)*v*val(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_l) + &
						t*(1.d0-v) * val(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_l) + &
						t*v * val(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_l) )
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		Vfunction = PI_V(2)*condval(3) + PI_V(1)*condval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Value function for somebody in jail in the current period (jail_l and jail_h are EVEN)
     piece = z*( (1.d0-t) * (1.d0-v) * val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h+1) + &
          	(1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h+1) + &
		t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h+1) + &
		t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h+1) ) + & 
            (1.d0-z)*( (1.d0-t) * (1.d0-v) * val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l+1) + &
          	(1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l+1) + &
		t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l+1) + &
		t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l+1) ) 
    
        fullterm: if (prisT>0) then

      	Vfunction = PI_A(2,lstate)*(PI_V(2)*condval(l_stind-1)+PI_V(1)* &
             condval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        Vfunction = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*condval(l_stind-1)+PI_V(1)*condval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


	
	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	Vfunction =	z*( (1.d0-t)*(1.d0-v)* val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h) + &
						   (1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h) + &
						t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h) + &
						t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h)) + & 
				(1.d0-z)*( (1.d0-t)*(1.d0-v)* val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l) + &
						   (1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l) + &
						t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l) + &
						t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l))		
	if (lage==1.or.lage==eduyr(2)+1) then
	CALL FTHETA (lage=lage,lialp=i_alp,lst_ind=l_stind,ltravaglio=travaglio)
	Vfunction = Vfunction + travaglio
    endif
             endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

Vfunction = PI_V(2)*condval(3)+PI_V(1)*condval(4)

else  ! This is retirement age and pensioners cannot be robbed

Vfunction = condval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION Vfunction
!----------------------------------------------------------------------------

REAL(long) FUNCTION VfunctionCA (ledu,lage,lwealth,li_zet,li_alp,lstate,lcr_st)
	use global1
	use global2
	use procs
	implicit none
	
	! This is the routine that interpolates the value function over assets ONLY. 
	! The ability fixed effect and the flow of utility from crime take values only 
	! on the node of their grid. 
    ! The routine returns the value function given a set of conditional on the 
    ! current income realization


	real(long) , intent(in)	:: lwealth
	integer, intent (in)	:: lage,ledu,li_zet,li_alp,lstate,lcr_st

	integer		:: i_al,i_ah,l_stind, fzet,i
	real(long)	:: a_l,a_h,t,condval(NGPST-1),putil,valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount



        if (lage>lifet) then
	    VfunctionCA = 0.0d0

        else    ! lage<=lifet
	
	! Figure out what grid points over wealth we are between
	CALL sial (lwealth, lage, i_al)
	
	
	IF (i_al<1) THEN ; PRINT *, "ial too low";	PRINT *, "wealth", lwealth
	PRINT *, "zeta", li_zet	; PRINT *, "age", lage
	i_al = 1
	ELSEIF (i_al>ngpa-1) THEN;	PRINT *, "ial too high";	
        ENDIF

    i_ah = i_al+1
	a_l = agrid(i_al, lage)
	a_h = agrid(i_ah, lage)

	t = (max(lwealth,agrid(1, lage))-a_l)/(a_h-a_l)
	
	if (lstate>=0) then ! Non-Students
	do l_stind=2,NGPST	

    ! Value function for somebody out of jail in the current period
	condval(l_stind-1) =	(1.d0-t)*val(ledu, lage,i_al,li_zet,li_alp, l_stind,lcr_st*2-1) + &
						    t* val(ledu, lage,i_ah,li_zet,li_alp,l_stind,lcr_st*2-1)
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		VfunctionCA = PI_V(2)*condval(3) + PI_V(1)*condval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Value function for somebody in jail in the current period
     piece =    (1.d0-t)*val(ledu, lage,i_al,li_zet,li_alp, l_stind,lcr_st*2) + &
			    t* val(ledu, lage,i_ah,li_zet,li_alp,l_stind,lcr_st*2) 

        fullterm: if (prisT>0) then

      	VfunctionCA = PI_A(2,lstate)*(PI_V(2)*condval(l_stind-1)+PI_V(1)* &
             condval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        VfunctionCA = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*condval(l_stind-1)+PI_V(1)*condval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


	
	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	VfunctionCA =    (1.d0-t)*val(ledu, lage,i_al,li_zet,li_alp, l_stind,lcr_st*2-1) + &
				    t* val(ledu, lage,i_ah,li_zet,li_alp,l_stind,lcr_st*2-1) 
	if (lage==1.or.lage==eduyr(2)+1) then
	CALL FTHETA (lage=lage,lialp=fgrid(li_alp),lst_ind=l_stind,ltravaglio=travaglio)
	VfunctionCA = VfunctionCA + travaglio
    endif
             endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

VfunctionCA = PI_V(2)*condval(3)+PI_V(1)*condval(4)

else  ! This is retirement age and pensioners cannot be robbed

VfunctionCA = condval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION VfunctionCA

!----------------------------------------------------------------------------

REAL(long) FUNCTION VfunctionD (ledu,lage,li_we,li_zet,li_alp,lstate,lcr_st)
	use global1
	use global2
	use procs
	implicit none
	
		
	! This is the routine that interpolates the value function over shocks and assets
	! It returns the value function given a set of states


	integer, intent (in)	:: lage,ledu,li_we,li_zet,li_alp,lstate,lcr_st

	integer		:: l_stind, fzet,i
	real(long)	:: condval(NGPST-1),putil,valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount



        if (lage>lifet) then
	    VfunctionD = 0.0d0

        else    ! lage<=lifet
	
	if (lstate>=0) then ! Non-Students
	do l_stind=2,NGPST	

    ! Value function for somebody out of jail in the current period
	condval(l_stind-1) =	val(ledu, lage,li_we,li_zet,li_alp, l_stind,lcr_st*2-1) 
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		VfunctionD = PI_V(2)*condval(3) + PI_V(1)*condval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Value function for somebody in jail in the current period
     piece = val(ledu, lage,li_we,li_zet,li_alp, l_stind,lcr_st*2)
     
        fullterm: if (prisT>0) then

      	VfunctionD = PI_A(2,lstate)*(PI_V(2)*condval(l_stind-1)+PI_V(1)* &
             condval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        VfunctionD = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*condval(l_stind-1)+PI_V(1)*condval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


	
!   PI_A(1,lstate)= probability of ending up in jail, PI_A(2,lstate)= 1-PI_A(1,lstate)

	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	VfunctionD =	val(ledu, lage,li_we,li_zet,li_alp, l_stind,lcr_st*2-1)
	 
	if (lage==1.or.lage==eduyr(2)+1) then
	CALL FTHETA (lage=lage,lialp=fgrid(li_alp),lst_ind=l_stind,ltravaglio=travaglio)
	VfunctionD = VfunctionD + travaglio
    endif
             endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

VfunctionD = PI_V(2)*condval(3)+PI_V(1)*condval(4)

else  ! This is retirement age and, for the time being, pensioners cannot be robbed

VfunctionD = condval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION VfunctionD

!----------------------------------------------------------------------------

REAL(long) FUNCTION VfunctionENRL (ledu,lage,lwealth,i_zet,i_alp,lstate,l_crim)
	use global1
	use global2
	use procs
	implicit none
	
	
	! This is the routine that interpolates the value function over shocks and assets
	! It returns the value function given a set of states

	real(long) , intent(in)	:: lwealth, i_alp,l_crim
	integer, intent (in)	:: lage, ledu, i_zet, lstate

	integer		:: i_al,i_ah,i_cl,i_ch,i_fl,i_fh,jail_l,jail_h,l_stind,fzet,i
	real(long)	:: a_l,a_h,c_l,c_h,f_l,f_h,t,v,z,taj,condval(NGPST-1),putil
	real(long)  :: valafterjail,chunk(2),piece,lwg,travaglio
	real(long), external :: discount



    if (lage>lifet) then
	VfunctionENRL = 0.0d0

        else    ! lage<=lifet
	
	
	! Figure out what grid points over wealth we are between
	CALL sial (lwealth, lage, i_al)
	
	IF (i_al<1) THEN ; PRINT *, "ial too low";	PRINT *, "wealth", lwealth
	    PRINT *, "zeta", i_zet	; PRINT *, "age", lage; PRINT *, "edu", ledu; PRINT *, "ialp", i_alp; PRINT *, "# crim", lstate
	   
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

    ! Value function for somebody out of jail in the current period
	condval(l_stind-1) =z*( (1.d0-t)*(1.d0-v)*val(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_h) + &
							(1.d0-t)*v*val(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_h) + &
						t*(1.d0-v) * val(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_h) + &
						t*v * val(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_h) ) + &
						(1.d0-z)* ( (1.d0-t)*(1.d0-v)*val(ledu, lage, i_al, i_zet, i_fl, l_stind,jail_l) + &
							(1.d0-t)*v*val(ledu, lage, i_al, i_zet, i_fh, l_stind,jail_l) + &
						t*(1.d0-v) * val(ledu, lage, i_ah, i_zet, i_fl, l_stind,jail_l) + &
						t*v * val(ledu, lage, i_ah, i_zet, i_fh, l_stind,jail_l) )
	enddo
	endif

if (lage<retage-1) then 


	if (lstate==0) then	! **************** NO CRIME IS COMMITTED

		VfunctionENRL = PI_V(2)*condval(3) + PI_V(1)*condval(4) ! 3 and 4 are st_ind=4 and 5 (NO CRIME)

	else if (lstate>0) then	! **************** CRIME IS COMMITTED

     ! Loot depends on intensity of crime (criminals have no labor income, so no difference
     ! between st_ind= 2 and 3, or 6 and 7, and so on. Here we find the right st_ind 
     if (lstate==1) then
     l_stind = 2     
     else 
     l_stind = lstate*2+2    ! This finds the right st_ind (by crime intensity) 
     endif

     ! Value function for somebody in jail in the current period (jail_l and jail_h are EVEN)
    piece = z*( (1.d0-t) * (1.d0-v) * val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h+1) + &
          	(1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h+1) + &
		t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h+1) + &
		t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h+1) ) + & 
            (1.d0-z)*( (1.d0-t) * (1.d0-v) * val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l+1) + &
          	(1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l+1) + &
		t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l+1) + &
		t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l+1) ) 

        fullterm: if (prisT>0) then

      	VfunctionENRL = PI_A(2,lstate)*(PI_V(2)*condval(l_stind-1)+PI_V(1)* &
             condval(l_stind)) + PI_A(1,lstate)*piece
     

        else    ! fullterm: prisT < 1

        VfunctionENRL = ( PI_A(2,lstate)+PI_A(1,lstate)*(1-prisTres) )* &
             (PI_V(2)*condval(l_stind-1)+PI_V(1)*condval(l_stind)) + &
             PI_A(1,lstate)*prisTres*piece

        endif fullterm


	
!   PI_A(1,lstate)= probability of ending up in jail, PI_A(2,lstate)= 1-PI_A(1,lstate)

	else if (lstate==-1) then	! ***** STUDENT *****

	l_stind=1
	VfunctionENRL =	z*( (1.d0-t)*(1.d0-v)* val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_h) + &
						   (1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_h) + &
						t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_h) + &
						t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_h)) + & 
				(1.d0-z)*( (1.d0-t)*(1.d0-v)* val(ledu,lage,i_al,i_zet,i_fl,l_stind,jail_l) + &
						   (1.d0-t) * v * val(ledu,lage,i_al,i_zet,i_fh,l_stind,jail_l) + &
						t * (1.d0-v) * val(ledu,lage,i_ah,i_zet,i_fl,l_stind,jail_l) + &
						t * v * val(ledu,lage,i_ah,i_zet,i_fh,l_stind,jail_l))		

             endif	! ****************

else  if (lage==retage-1)  then ! Last year of working life: we bar people from commiting crime in this period

VfunctionENRL = PI_V(2)*condval(3)+PI_V(1)*condval(4)

else  ! This is retirement age and, for the time being, pensioners cannot be robbed

VfunctionENRL = condval(3)

endif    !end if lage


	endif

	continue

ENDFUNCTION VfunctionENRL


