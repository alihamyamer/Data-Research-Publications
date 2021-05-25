MODULE PROCS

	USE global1
	USE global2

	IMPLICIT NONE

    real(long), external :: ir

CONTAINS 

! IN ALPHABETIC ORDER:

! A: 
! B: 
! C: CFUNC, CON_INTERP
! D: DISTSTAT,
! E: EMUC_EVALUE, EVALUE, ERRORCHECK
! F: FTHETA
! H:
! I: INCDIST
! L: LOAD_VECTOR,LOAD_MATRIX,LOAD_MATRIX_2,LOCALEGM 
! M: MUOC
! N: 
! O:  
! P: PINV_UTIL,PUTILITY, PUTILITY_ARR
! Q:
! R: 
! S: save_vector,save_vector_integer, SIAL, SICL, SIEL, SAVE_COLUMNS 
! T:
! U:
! V: VAL_INTERP, VFUNC
! W: WAGEDIST
! X:
! Y: YOUNGWEALTH 
! Z:

!************************************************
! A

!************************************************


!**********************************************************
! C
SUBROUTINE CFUNC (ledu, lage, lwealth, lizet, i_alp, l_stind, l_crim, cons)
! ROUTINE FOR LINEAR INTERPOLATION OF CONSUMPTION POLICY 

	INTEGER, INTENT (in)		:: lage, ledu, l_stind,lizet
	REAL (long), INTENT (in)	:: i_alp, lwealth,l_crim
	REAL (long), INTENT (out)	:: cons
	! Locals
	INTEGER		:: i_al,i_cl,i_el,i_fl,jail_l,i_ah,i_ch,i_eh,i_fh,jail_h,j1,j2,J3,J4
	REAL (long) :: a_l,a_h,c_l,c_h,e_l,e_h,f_l,f_h,t,u,v,z

	! First need to figure out what grid points over wealth and zeta we are between
	CALL sial (lwealth, lage, i_al)
	
	IF (i_al<1) THEN ; PRINT *, "ial too low" ; PRINT *, "wealth", lwealth
	PRINT *, "age", lage; PRINT *, "consumption", consumption
	PRINT *, "stop"	; 
	i_al = 1
	ELSEIF (i_al>ngpa-1) THEN ; PRINT *, "ial too high"; 
	ENDIF

    i_ah = i_al+1
	a_l = agrid(i_al, lage)
	a_h = agrid(i_ah, lage)
	t = (max(lwealth,agrid(1, lage))-a_l)/(a_h-a_l)
	
	if (lage>=retage) then !ability and crim_fixed are irrelevant for pensioners
	    v = 0.d0   
	    z = 0.d0
        jail_l = 1
        jail_h = jail_l + 2
        i_fl = 1
        i_fh = i_fl+1
	else
	    ! Figure out what grid points on the fixed effect should be interpolated
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
	    
	    ! Finds jail index counterpart for crime index
        jail_l = i_cl*2-1
	    jail_h = i_ch*2-1
	    
	    z = (l_crim-c_l)/(c_h-c_l)
	
	
	endif
    
		cons =  z*( (1.d0-t) * (1.d0-v) * con(ledu, lage, i_al, lizet, i_fl, l_stind,jail_h) + &
				(1.d0-t) * v * con(ledu, lage, i_al, lizet, i_fh, l_stind,jail_h) + &
				t * (1.d0-v) * con(ledu, lage, i_ah, lizet, i_fl, l_stind,jail_h) + &
				t * v * con(ledu, lage, i_ah, lizet, i_fh, l_stind,jail_h) ) + &
				(1.d0-z)*( (1.d0-t) * (1.d0-v) * con(ledu, lage, i_al, lizet, i_fl, l_stind,jail_l) + &
				(1.d0-t) * v * con(ledu, lage, i_al, lizet, i_fh, l_stind,jail_l) + &
				t * (1.d0-v) * con(ledu, lage, i_ah, lizet, i_fl, l_stind,jail_l) + &
				t * v * con(ledu, lage, i_ah, lizet, i_fh, l_stind,jail_l) )

    
	CONTINUE

ENDSUBROUTINE CFUNC

!-----------------------------
SUBROUTINE CON_INTERP (x,f1_x,xp,length_x,interp_value)
!   SUBROUTINE TO INTERPOLATE VECTORS OF CONSUMPTION IN THE END. GRID METHOD 
!   VECTORS HAVE TO BE INCREASING, BUT NOT STRICTLY SO.
    REAL (long), DIMENSION (length_x), INTENT(in):: x, f1_x
    REAL (long), DIMENSION (:), INTENT(in):: xp
    REAL (long), DIMENSION (size(xp)), INTENT(out):: interp_value
    REAL (long), EXTERNAL:: discount

	INTEGER, DIMENSION(1) :: x_min
	INTEGER :: index_k, x_index,length_x,i,j,problem,flag
	REAL (long) :: t,f_x(length_x)
    
    f_x = f1_x
    f_x(1) = x(1) - agrid(1,age+1)  ! Replace the first consumption node with that of
                                    ! of a borrowing constrained. 

    problem = 0     !flag for not-strictly-increasing vectors
    
    if (any(x(1:length_x-1)==x(2:length_x))) problem = 1 !if some ascissas coincide problem=1		
	DO index_k = 1,size(xp)
	    flag = 1        !flag for selecting appropriate interpolating algorithm
        IF ( xp(index_k) .le. x(1) ) THEN
! 		If xp is below the asset level (on the endogenous grid) associated with being
! 		borrowing constrained, the agent is borrowing constrained (no interpolation) 
            interp_value(index_k)=wg+(1.d0+ir(age))*xp(index_k)-agrid(1,age+1)
        ELSE
!       Locate position of FIRST closest node (if some nodes are identical minloc
!       reports the position of the first one)        
       	x_min = MINLOC(ABS(x-xp(index_k)))
		x_index = x_min(1)
        
!       If coincident ascissas and extra/inter-polation using node x(x_index) and 
!       the next one to its right, x(x_index+1)=x(x_index) cannot be ruled out.
!       Case(2) finds the first node x(x_xindex+n)>x(x_index)
        
        if (problem==1 .and. (xp(index_k)>= x(x_index) .or. x_index==1)) flag = 2
        select case (flag)
            case (1)
!			Interpolation (interior) between two points with different ascissas 
            if (xp(index_k)<x(x_index)) x_index = x_index-1
            x_index = MAX(1,x_index)
            x_index = MIN(x_index,length_x-1)
            case (2)
!			Extra/interpolation between the selected point and one to its right. 
!			Search for the first pair which bounds a non-empty interval
		    x_index = MIN(x_index,length_x-1)
            j = x_index-1 !Initializes interpolating node to the first to the left of x_index
            do i = x_index+1,length_x
                if (x(i)>x(x_index)) then
                    j=i-1 ! Updates interpolating node if there is i>x_index such that x(i)>x(x_index)
                    exit
                end if
            end do
            x_index = j
        end select  	
		t = (f_x(x_index+1)-f_x(x_index))/(x(x_index+1)-x(x_index))
		interp_value(index_k) = t*(xp(index_k)-x(x_index))+f_x(x_index)
        end if
	ENDDO

END SUBROUTINE CON_INTERP

!*********************************************
! D
SUBROUTINE DISTSTAT (n,year,vdist,gini,w15p,pwza,wpnw,lavwealth,lmedwealth,lvarwealth)
! Get wealth statistics
	USE global1
	USE global2
!	INPUTS
	INTEGER, INTENT (in) :: n, year
	REAL (long), DIMENSION (n), INTENT (in) :: vdist
!	OUTPUTS
	REAL (long), INTENT (out) :: gini, w15p, pwza, wpnw, lavwealth, lmedwealth, lvarwealth
!	LOCAL VARIABLES
	REAL (long), DIMENSION (n) :: wdinc, wdsc, wdscr
	REAL (long) :: wr40, wr20, wr10, wr5, wr1, wp40, wp20, wp10, wp5, wp1, &
	temp, aucd, nlzr, wrguy, wpguy, r7525, pw15, r9010
	INTEGER :: i, temp_2, nlz
	CHARACTER(100) :: namedata, namecommand 

!	Sort wealth in ascending order
	CALL DSVRGN (n, vdist, wdinc) 
!	COMPUTE CUMULATIVE DISTRIBUTION OF ASSETS
	temp=0.0
	DO i=1,n
	wdsc(i)=temp+wdinc(i)
	temp=wdsc(i)
	ENDDO

	IF (SUM(vdist)>0.d0) THEN
	wdscr=wdsc/SUM(vdist)
!	Gini computation
	aucd=SUM(wdscr)/n
	gini=2.0*(0.5-aucd)
	ENDIF

!	COMPUTE VARIOUS PERCENTILES OF DISTRIBUTION
	!Computes aggregate wealth holdings of percentiles 0.15d0-0.02 to 0.15d0+0.02 in wealth dist
	w15p= SUM( wdinc( NINT((0.15d0-0.02d0)*n) : NINT((0.15d0+0.02d0)*n) )) / (0.04d0*n)
	wp40=100.d0*SUM(wdinc(1:NINT(0.4d0*n))) / SUM(vdist)
	wp20=100.d0*SUM(wdinc(1:NINT(0.2d0*n))) / SUM(vdist)
	wp10=100.d0*SUM(wdinc(1:NINT(0.1d0*n))) / SUM(vdist)
	wp5=100.0*SUM(wdinc(1:NINT(0.05d0*n))) / SUM(vdist)
	wp1=100.0*SUM(wdinc(1:NINT(0.01d0*n))) / SUM(vdist)
	wr40=100.0*SUM(wdinc(1+NINT(0.6d0*n):n))/SUM(vdist)
	wr20=100.0*SUM(wdinc(1+NINT(0.8d0*n):n))/SUM(vdist)
	wr10=100.0*SUM(wdinc(1+NINT(0.9d0*n):n))/SUM(vdist)
	wr5=100.0*SUM(wdinc(1+NINT(0.95d0*n):n))/SUM(vdist)
	wr1=100.0*SUM(wdinc(1+NINT(0.99d0*n):n))/SUM(vdist)


	wrguy=MAXVAL(vdist)
	wpguy=MINVAL(vdist)

	r9010=wdinc(NINT(9.0*n/10.0))/wdinc(NINT(1.0*n/10.0))
	r7525=wdinc(NINT(7.5*n/10.0))/wdinc(NINT(2.5*n/10.0))

!	COMPUTE FRACTION WHICH IS BORROWING CONSTRAINED
	temp_2 = 0;	nlz = 0
	DO i=1,n
		IF (wdinc(i)<=critw) THEN
		nlz=temp_2+1
		temp_2=nlz
		ELSE
		EXIT
		ENDIF
	ENDDO
	nlzr=REAL(nlz,long)
	pwza=REAL(nlzr/n,long)
	wpnw = SUM(wdinc(1:nlz)) / real(nlz)
	! AVERAGE WEALTH
	lavwealth =	SUM(wdinc) / REAL(n,long)
	! VARIANCE
	lvarwealth = SUM(wdinc**2.d0)/REAL(n,long) - lavwealth**2.d0
	! MEDIAN WEALTH
	lmedwealth =	wdinc(n/2)

ENDSUBROUTINE DISTSTAT

!*********************************************
! E

SUBROUTINE EMUC_EVALUE (ledu,lage,li_we,li_zet,lialp,ljail,emucp,evaluep)
!ROUTINE FOR COMPUTING EXPECTED CONTINUATION VALUE AND ITS WEALTH DERIVATIVE IN NEXT PERIOD GIVEN
!NEXT PERIOD WEALTH - Note that: age, ialp, fixedeffect, st_ind are all known globals here!
!it is called only for agents who can optimize (mod(jail,2)==1) so, unlike EVALUE, does not 
!have to keep track of fractional prison terms etc
USE numerical_libraries
INCLUDE 'link_fnl_shared.h'
!DEC$ OBJCOMMENT LIB:'libiomp5md.lib'
IMPLICIT NONE

	REAL (long), INTENT (out)	:: emucp,evaluep
	INTEGER, INTENT (in) :: ledu,lage,lialp,ljail,li_we,li_zet 
	! Locals
	INTEGER :: i, cloc(ngpz),j,cr_state,i_zet
	REAL(long), EXTERNAL :: vfunctiond, dvfunctiond,vfunctionca,dvfunctionca,transf,discount
	REAL(long) :: chunk_muc(ngpz),chunk_val(ngpz),valwork,lstcash,lschship,lpvtuit
	REAL(long) :: lschshare,aprime,valvec(maxcrim+1)

	EMUCP = 0.0d0  ! initialize output variables
	EVALUEP = 0.0d0
    
    ! cr_state selects the appropriate quasilinear utility of engaging                                 ! in crime
	! cr_state= 1 if jail=1,2, cr_state=2 if jail=3,4, etc
    cr_state = INT((ljail+1)/2) 
    
    IF (st_ind/=1) THEN ! NON STUDENTS

	 IF (lage>=retage-1) THEN !retired at age+1: no crime nor victimization
	 emucp = dVfunctionD(ledu,lage+1,li_we,li_zet,lialp,0,cr_state)
	 evaluep = VfunctionD(ledu,lage+1,li_we,li_zet,lialp,0,cr_state)
	 ELSE                   ! working-age agents
		DO i_zet=1,ngpz
            DO j=0,maxcrim
                valvec(j+1) = VfunctionD (ledu,lage+1,li_we,i_zet,lialp,j,cr_state)
            ENDDO
        cloc(i_zet) = MAXLOC(valvec,1)-1 ! optimal crime intensity
		chunk_muc(i_zet) = dVfunctionD(ledu,lage+1,li_we,i_zet,lialp,cloc(i_zet),cr_state)
		chunk_val(i_zet) = VfunctionD(ledu,lage+1,li_we,i_zet,lialp,cloc(i_zet),cr_state)
		ENDDO
        ! Expectations over future i_zet conditional on current li_zetdo
        emucp = dot_product(ztrans(li_zet,:,lage+1,ledu),chunk_muc(:))
	    evaluep = dot_product(ztrans(li_zet,:,lage+1,ledu),chunk_val(:))
	 ENDIF

	ELSE IF (st_ind==1) THEN ! STUDENTS 
    
    aprime = agrid(li_we,lage+1)
    
    ! Notice: for age==sum(eduyr(:)) EMUC_EVALUE is never called 
    if (lage==eduyr(2)) then   ! HS -  LAST YEAR
	
        lPVtuit=tuit(ledu+1) *(1.d0+DISCOUNT(ir(lage),eduyr(ledu+1)-1,1,2))

	    lstcash=aprime-lPVtuit	! ******* STUDENT CASH (BEFORE ANY SUBSIDY) ******

        ! Expectation over value of work
        ! Shock is white noise for people entering the labour market
        valwork = dot_product(zstat(:,ledu+1),eval(ledu+1,lage+1,li_we,:,lialp,ljail))
            
        IF (subregime==1) THEN
        lschshare=schshare(ledu+1)
        ELSE
        lschshare=0.d0
        ENDIF
		
	    IF (valwork>=lschshare*VfunctionCA(ledu+1,lage+1,lstcash+lschship,1,lialp,-1,&
        cr_state)+(1.d0-lschshare)*VfunctionCA(ledu+1,lage+1,lstcash,1,lialp,-1,&
        cr_state) ) THEN
	    ! Choosing NOT to continue education
        emucp = dot_product(zstat(:,ledu+1),edval(ledu+1,lage+1,li_we,:,lialp,ljail)) 
	    evaluep = valwork
	    
	    ELSE          ! Choosing to continue education - valwork<VALSTUDY

	    emucp = lschshare*dVfunctionCA(ledu+1,lage+1,lstcash+lschship,1,lialp,-1,&
	    cr_state)+ (1-lschshare)*dVfunctionCA(ledu+1,lage+1,lstcash,1,lialp,-1,&
        cr_state)   
        evaluep = lschshare*VfunctionCA(ledu+1,lage+1,lstcash+lschship,1,lialp,-1,&
		    cr_state)+ (1-lschshare)*VfunctionCA(ledu+1,lage+1,lstcash,1,lialp,-1,&
		    cr_state)
	    ENDIF   ! Choosing NOT to continue education
	
	else    ! ALL OTHER STUDENTS (NOT IN LAST YEAR OF EDU CYCLE)
	    emucp = dVfunctionD(ledu,lage+1,li_we,1,lialp,-1,cr_state)
	    evaluep = VfunctionD(ledu,lage+1,li_we,1,lialp,-1,cr_state)
	endif     

	ENDIF   ! st_ind/=1

ENDSUBROUTINE EMUC_EVALUE

!**********************************************************
! F
SUBROUTINE FTHETA (lage, lconsumption, wage, lialp, lst_ind, ltravaglio)
! ROUTINE FOR COMPUTING UTILITY COSTS OF SCHOOLING

	USE global1
	USE global2
	!use imsl

	INTEGER, OPTIONAL, INTENT (in) ::	lage 
	INTEGER, OPTIONAL, INTENT (in) :: lst_ind
	REAL (long), INTENT (in), OPTIONAL :: lialp, lconsumption , wage 
	REAL (long), INTENT (out) :: ltravaglio
!	Local
	REAL(long) :: tempor, tt
	INTEGER :: I_FL, I_FH, i, ledu

	CALL sifl (lialp, i_fl)
	i_fh=i_fl+1

	IF (FIXIND==1) THEN	! CONTINUOUS FIXED EFFECTS
        IF (lage < eduyr(2)+1) THEN
        	ltravaglio = leisure(i_fl)
		ELSE
        	ltravaglio = leisure(ngpf-1+i_fl)
		ENDIF

	ELSE IF (FIXIND==2) THEN	! DISCRETE BINS
				IF (lage < eduyr(2)+1) THEN
	ltravaglio = leisure(i_fl)
				ELSE
	ltravaglio = leisure(ngpf-1+i_fl)
				ENDIF
	ENDIF


END SUBROUTINE FTHETA

!*****************************************
!*****************************************
! I
SUBROUTINE INCDIST (n,year,vdist,lavinc,lmedinc,lvarinc)
! Income distribution for the whole living population in a given year

	USE global1
	USE global2
	!USE IMSL

!	INPUTS
	INTEGER, INTENT (in) :: n, year
	REAL (long), INTENT (in) :: vdist(n)
!	OUTPUTS
	REAL (long), INTENT (out) :: lavinc, lmedinc, lvarinc
!	LOCAL VARIABLES
	REAL(long) :: rr(n) , rrsort(n), temp(n) , vdinc(n), denomin(n), useobs(n)
	INTEGER :: i,i1,j,j1, temp_2, nlz
	CHARACTER(100) :: namedata, namecommand

!	CALL DRNUN(n,rr)
	!Sort in ascending order
	CALL DSVRGN (n,vdist,vdinc)

	WHERE (vdinc(:) > -1.d5)
	temp(:) = vdinc(:)
	denomin(:) = 1.0d0
	ELSEWHERE
	temp(:) = 0.d0
	denomin(:) = 0.0d0
	endwhere

	IF (SUM(denomin(:))>0.d0) THEN
	lavinc  = SUM(temp)/SUM(denomin)
	ENDIF

	lvarinc = SUM(temp**2.d0)/SUM(denomin)-lavinc**2.d0

	i1=1
	DO j1=1,n
		IF (vdinc(j1)>-1.d5) THEN
		useobs(i1) = vdinc(j1)
		i1=i1+1
		ENDIF
	ENDDO

	lmedinc = useobs(NINT((i1-1)/2.d0))


END SUBROUTINE INCDIST

!********************************************************
! L
SUBROUTINE load_vector(name,f,n1,vect)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1
	REAL(long),INTENT(out)		:: vect(n1)

	INTEGER::i1,i2
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='READ',position='REWIND')
	
	DO i1=1,n1
		READ(f,*)vect(i1)
	END DO

	CLOSE(f)

END SUBROUTINE load_vector

!*************************
SUBROUTINE load_matrix(name,f,n1,n2,mat)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1,n2
	REAL(long),INTENT(out)		:: mat(n1,n2)

	INTEGER::i1,i2
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='READ',position='REWIND')
	
	DO i2=1,n2
		READ(f,*)mat(:,i2)
	END DO

	CLOSE(f)

END SUBROUTINE load_matrix

!*************************
SUBROUTINE load_matrix_2(name,f,n1,n2,n3,mat)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1,n2,n3
	REAL(long),INTENT(out)		:: mat(n1,n2,n3)

	INTEGER::i1,i2,i3
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='READ',position='REWIND')
	DO i1=1,n1
		DO i2=1,n2
			DO i3=1,n3
				READ(f,*)mat(i1,i2,i3)
			END DO
		END DO
	END DO

	CLOSE(f)

END SUBROUTINE load_matrix_2

!*******************
SUBROUTINE LOCALEGM(li_we,util,c_sol,z_sol)
! Solves for consumption and cash at hand satisfying the Euler equation
! and the resource constraint given a choice of future wealth
 
  INTEGER, INTENT (in)		:: li_we
  REAL(long), INTENT (in)	:: util
  REAL(long), INTENT (out)	:: z_sol, c_sol

  !Invert the Euler equation
  IF (age .ne. beqage) THEN 
     c_sol = util**(-1.d0/sigma)
     z_sol = c_sol+agrid(li_we,age+1)
  ELSE
     ! At age=beqage inter-vivos transfer b
     ! z = c + a' + bequest 
     c_sol = (util)**(-1.d0/sigma)
     z_sol = c_sol + agrid(li_we,age+1) + &
        MAX(0.d0,gamma_1**(1.d0/eta)*c_sol**(sigma/eta)-gamma_2)
  ENDIF
   
endsubroutine LOCALEGM


!********************************************
! M
SUBROUTINE MUOC (l_age,l_consumption,muc)
!	ROUTINE FOR COMPUTING marginal utility of C 

	REAL (long), INTENT (in) :: l_consumption
	INTEGER, INTENT(in)		 :: l_age
	REAL (long), INTENT (out) :: muc
	REAL(long) :: l_hours

    IF (l_consumption>CLOW) THEN
       muc =  l_consumption**(-sigma)	! Leisure equal to 1
    ELSE
	    muc = INADERIV	! Either leisure or consumption equal to 0
	ENDIF

	muc=MIN(muc,INADERIV)	! restrict this marginal utility to be less than MUC(0)

END SUBROUTINE MUOC

!*********************************************
! P
SUBROUTINE PINV_UTIL (util,consumption)
! SUBROUTINE for computing the inverse of the felicity value
 REAL (long), INTENT (in) :: util
 REAL (long), INTENT (out) :: consumption


 IF (sigma == 1.d0) THEN ! LOG UTILITY CASE
		consumption = exp(util)
    ELSE ! NON-LOG UTILITY
     consumption = ((1-sigma)*util)**(1/(1-sigma))
 ENDIF

END SUBROUTINE PINV_UTIL

!******************************************************
SUBROUTINE PUTILITY (lage, lconsumption,lstind,ljail,utils)
! SUBROUTINE for computing period utility
	INTEGER, INTENT (in)		:: lage,lstind,ljail
	REAL (long), INTENT (in)	:: lconsumption
	REAL (long), INTENT (out)	:: utils
    REAL (long)					:: lbequest

    utils = inada
    
    IF (lconsumption > CLOW) THEN
        IF (sigma == 1.d0) THEN ! LOG UTILITY CASE
            utils  = LOG(lconsumption)
        ELSEIF (sigma /= 1.d0) THEN ! NON-LOG UTILITY
            utils  = (1.d0/(1.d0-sigma))*(lconsumption)**(1.d0-sigma)
        ENDIF
        if (lage==beqage .and. mod(jail,2)==1) then
            ! Age= beqage and out of jail. Inter-vivos transfer of:
            ! Using intra-temporal opt. condition for bequest
            lbequest =  MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
            ! Warm glow utility term 
            IF (eta == 1.d0) THEN  
                utils = utils + gamma_1*LOG(gamma_2+lbequest) 
            ELSE
                utils = utils + gamma_1*(gamma_2+lbequest)**(1.d0-eta)/(1.d0-eta)
            END IF
          
        ENDIF
    ENDIF
       
    utils = MAX(utils,inada) ! LOWER BOUND ON PERIOD UTILITY

END SUBROUTINE PUTILITY

!******************************************************

SUBROUTINE PUTILITY_ARR (lage,lconsumption,lstind,ljail,utils)
! SUBROUTINE for computing period utility
	INTEGER, INTENT (in)		:: lage,lstind,ljail
	REAL (long), DIMENSION(:), INTENT (in)	:: lconsumption
	REAL (long), DIMENSION(SIZE(lconsumption)), INTENT (out)	:: utils
    REAL (long), DIMENSION(SIZE(lconsumption))	:: lbequest
    
    utils = inada 
    
    IF (sigma == 1.d0) THEN ! LOG UTILITY CASE
        WHERE (lconsumption > CLOW) 
            utils  = LOG(lconsumption)
        endwhere   
    ELSE  ! NON-LOG UTILITY
         WHERE (lconsumption > CLOW) 
             utils  = (1.d0/(1.d0-sigma))*(lconsumption)**(1.d0-sigma)
         endwhere   
    ENDIF
    
    if (lage==beqage .and. mod(jail,2)==1) then
        ! Age= beqage and out of jail. Inter-vivos transfer of:
        ! Using intra-temporal opt. condition for bequest     
        ! Warm glow utility term 
        IF (eta == 1.d0) THEN  
            where (lconsumption > CLOW)
               lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
               utils = utils + gamma_1*LOG(gamma_2+lbequest) 
            endwhere
        ELSE
            where (lconsumption > CLOW)
               lbequest = MAX(0.d0,gamma_1**(1.d0/eta)*lconsumption**(sigma/eta)-gamma_2)
               utils = utils + gamma_1*(gamma_2+lbequest)**(1.d0-eta)/(1.d0-eta)
            endwhere
        END IF
    ENDIF
       
    
    utils = MAX(utils,inada) ! LOWER BOUND ON PERIOD UTILITY

END SUBROUTINE PUTILITY_ARR

!************************************************
! S

subroutine save_columns(name,f,n1,n2,mat)

! THIS SUBROUTINE PRINTS SEPARATE COLUMNS (UP TO 20), WITH EACH COLUMN BEING A DIFFERENT 
! VALUE FOR A STATE VARIABLE X - WHICH CAN TAKE N1 VALUES.
! FOR EACH ELEMENT OF X(N1) THE COLUMN REPORTS  (VERTICALLY) ALL THE VALUES ASSOCIATED TO CHANGES IN
! ANOTHER STATE Y, WHICH CAN TAKE N2 VALUES.
! NOTICE: BOTH N1 AND N2 CAN BE SUBSETS OF THE TOTAL DIMENSION OF A GIVEN STATE: FOR EXAMPLE, IF WE HAVE 
! A VECTOR V(15,15,15,15) and we want to study variation in the first 2 dimensions (but only up to 10), we can 
! do the following call: call save_columns(name,f,n1=10,n2=2,5,5)

	use global1
	use global2

	character(100),intent(in)	:: name
	integer,intent(in)			:: f,n1,n2
	real(long),intent(in)		:: mat(n1,n2)

	integer::i1,i2,i3,i4
	
	open(unit=f,file=trim(drive)//trim(path1)//trim(parg)//name,action='WRITE',position='REWIND')
	
	do i1=1,n1
    	write(f,'(20F10.3)') (mat(i1,i2),i2=1,n2) 
	end do

	close(f)

end subroutine save_columns

!************************************************

SUBROUTINE save_matgnu(name,f,n1,n2,mat)
! Saves matrix in rectangular format readable by Gnuplot
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1,n2
	REAL(long),INTENT(in)		:: mat(n1,n2)
    REAL :: mats(n1,n2)
	INTEGER::i1,i2

    mats=mat    ! Assign to single precision
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='WRITE',position='REWIND')

    DO i1 = 1, n1
    WRITE ( f, * ) mats(i1,1:n2)
    END DO

	CLOSE(f)

END SUBROUTINE save_matgnu

!-------------------------------------------
SUBROUTINE save_matrix(name,f,n1,n2,mat)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1,n2
	REAL(long),INTENT(in)		:: mat(n1,n2)

	INTEGER::i1,i2
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='WRITE',position='REWIND')
	DO i2=1,n2
		WRITE(f,*) mat(:,i2)
	END DO

	CLOSE(f)

END SUBROUTINE save_matrix

!----------------------------
SUBROUTINE save_matrix_2(name,f,n1,n2,n3,mat)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1,n2,n3
	REAL(long),INTENT(in)		:: mat(n1,n2,n3)

	INTEGER::i1,i2,i3
	
	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='WRITE',position='REWIND')
	DO i1=1,n1
		DO i3=1,n3
			WRITE(f,*)mat(i1,:,i3)
		END DO
		write (f,*), '                            '		
	END DO

	CLOSE(f)

END SUBROUTINE save_matrix_2

!-------------------------------------
SUBROUTINE save_vector(name,f,n1,vect)
	USE global1
	USE global2

	CHARACTER(100),INTENT(in)	:: name
	INTEGER,INTENT(in)			:: f,n1
	REAL(long),INTENT(in)		:: vect(n1)

	INTEGER::i1,i2
	
 	OPEN(unit=f,file=TRIM(drive)//TRIM(path1)//TRIM(parg)//name,action='WRITE',position='REWIND')

	DO i1=1,n1
		WRITE(f,*) vect(i1)
	END DO

	CLOSE(f)
END SUBROUTINE save_vector

!-------------------------------------
SUBROUTINE SIAL (lwealth, lage, ial)
!ROUTINE FOR FIGURING OUT POSITION IN ASSET GRID
REAL (long), INTENT (in) :: lwealth
INTEGER, INTENT (in) :: lage
INTEGER, INTENT (out) :: ial
INTEGER :: jl, ju, jm

jl=0
ju=ngpa+1
DO
	IF (ju-jl<=1) EXIT
	jm=(ju+jl)/2
       
		IF (lwealth>=agrid(jm, lage)) THEN
		jl=jm
		ELSE
		ju=jm
		ENDIF
ENDDO
     
	IF (lwealth<=agrid(1, lage)) THEN
	ial=1
	ELSEIF (lwealth>=agrid(ngpa, lage)) THEN
	ial=ngpa-1
	ELSE
	ial=jl
	ENDIF

ENDSUBROUTINE SIAL

!-------------------------------------------

SUBROUTINE SICL (lcrim, icl)
!ROUTINE FOR FIGURING OUT POSITION on Fixed Effects Grid
REAL (long), INTENT (in) :: lcrim
INTEGER, INTENT (out) :: icl

!LOCAL VARIABLES
INTEGER :: jl, ju, jm

jl=0
ju=maxjail/2+1
DO
	IF (ju-jl<=1) EXIT
	jm=(ju+jl)/2
	IF (lcrim>=cgrid(jm)) THEN
		jl=jm
	ELSE
		ju=jm
	ENDIF

ENDDO
     
IF (lcrim<=cgrid(1)) THEN
	icl=1
ELSEIF (lcrim>=cgrid(maxjail/2)) THEN
	icl=maxjail/2-1
ELSE
	icl=jl
ENDIF

ENDSUBROUTINE SICL

!-------------------------------------------

SUBROUTINE SIEL (lzeta, lage, ledu, iel)
!ROUTINE FOR FIGURING OUT POSITION IN ETA GRID
REAL (long), INTENT (in) :: lzeta
INTEGER, INTENT (in) :: lage , ledu
INTEGER, INTENT (out) :: iel
!LOCAL VARIABLES
INTEGER :: jl, ju, jm

jl=0
ju=ngpz+1
DO
	IF (ju-jl<=1) EXIT
	jm=(ju+jl)/2
        
	IF (lzeta>=zgrid(ledu,jm, lage)) THEN
		jl=jm
	ELSE
		ju=jm
	ENDIF

ENDDO
     
IF (lzeta<=zgrid(ledu, 1, lage)) THEN
	iel=1
ELSEIF (lzeta>=zgrid(ledu,ngpz, lage)) THEN
	iel=ngpz-1
ELSE
	iel=jl
ENDIF

ENDSUBROUTINE SIEL

!-------------------------------------------

SUBROUTINE SIFL (lfix, ifl)
!ROUTINE FOR FIGURING OUT POSITION on Fixed Effects Grid
REAL (long), INTENT (in) :: lfix
INTEGER, INTENT (out) :: ifl

!LOCAL VARIABLES
INTEGER :: jl, ju, jm

jl=0
ju=ngpf+1
DO
	IF (ju-jl<=1) EXIT
	jm=(ju+jl)/2
	IF (lfix>=fgrid(jm)) THEN
		jl=jm
	ELSE
		ju=jm
	ENDIF

ENDDO
     
IF (lfix<=fgrid(1)) THEN
	ifl=1
ELSEIF (lfix>=fgrid(ngpf)) THEN
	ifl=ngpf-1
ELSE
	ifl=jl
ENDIF

ENDSUBROUTINE SIFL

!--------------------------------------------------
!*******************************************
! V
SUBROUTINE  VAL_INTERP (x,v_x,xp,length_x,c_x1,c_xp,interp_value)
!   SUBROUTINE TO INTERPOLATE VECTORS OF VALUE FUNCTIONS IN THE END. GRID METHOD 
!   VECTORS HAVE TO BE INCREASING, BUT NOT STRICTLY SO.
    REAL (long), DIMENSION (length_x), INTENT(in):: x, v_x
    REAL (long), DIMENSION (:), INTENT(in):: xp
    REAL (long), DIMENSION (size(xp)), INTENT(in):: c_xp
    REAL (long), DIMENSION (size(xp)), INTENT(out):: interp_value
    REAL (long), EXTERNAL:: discount
    
	INTEGER, DIMENSION(1) :: x_min
	INTEGER :: index_k, x_index,length_x,i,j,problem,flag
	REAL (long) :: t,c_x1,fel1,fel2
	
	problem = 0     !flag for not-strictly-increasing vectors
    
    if (any(x(1:length_x-1)==x(2:length_x))) problem = 1 !if some ascissas coincide problem=1
    		
	CALL  PUTILITY (age,c_x1,st_ind,jail,fel1)	
	DO index_k = 1,size(xp)
	    flag = 1        !flag for selecting appropriate interpolating algorithm
        IF ( xp(index_k) < x(1) ) THEN
        ! If  xp is below the asset level (on the endogenous grid) associated with being
        ! borrowing constrained, the agent is borrowing constrained (no
        ! interpolation). Adding to the value fn on the end. grid point
        ! the felicity from borrowing constrained consumption at the
        ! ex. grid point and subtracting that from consuming at the end. grid
        ! point recovers the value function at the  exogenous grid point
        ! c_x1 is consumption at the first endogenous grid point
        ! c_xp is consumption at the exogenous grid point 
            CALL  PUTILITY (age,c_xp(index_k),st_ind,jail,fel2)
            interp_value(index_k)=v_x(1)+min(fel1,fel2)-fel1
        ELSE
!       Locate position of FIRST closest node (if some nodes are identical minloc
!       reports the position of the first one)        
       	x_min = MINLOC(ABS(x-xp(index_k)))
		x_index = x_min(1)
	
!       If coincident ascissas and extra/inter-polation using node x(x_index) and 
!       the next one to its right, x(x_index+1)=x(x_index) cannot be ruled out.
!       Case(2) finds the first node x(x_xindex+n)>x(x_index)
        if (problem==1 .and. (xp(index_k)>= x(x_index) .or. x_index==1)) flag = 2
        select case (flag)
            case (1)
!           Interpolation (interior) between two DIFFERENT nodes
            if (xp(index_k)<x(x_index)) x_index = x_index-1
            x_index = MAX(1,x_index)
            x_index = MIN(x_index,length_x-1)
            case (2)
!           Extra/inter-polation between the selected point and one to its right. 
!           Search for the first pair which bounds a non-empty interval
		    x_index = MIN(x_index,length_x-1)
		    j = x_index-1 !Initializes interpolating node to the first to the left of x_index
            do i = x_index+1,length_x
                if (x(i)>x(x_index)) then
                    j = i-1 ! Updates interpolating node if there is i>x_index such that x(i)>x(x_index)
                    exit
                end if
            end do
            x_index = j
        end select  	
		t = (v_x(x_index+1)-v_x(x_index))/(x(x_index+1)-x(x_index))
		interp_value(index_k) = t*(xp(index_k)-x(x_index))+v_x(x_index)
        end if
	ENDDO

END SUBROUTINE VAL_INTERP

!********************************

SUBROUTINE VFUNC (ledu,lage,iwe,izet,lialp,lstind,ljail,LVAL)
! ROUTINE FOR COMPUTATION OF VALUE FUNCTIONS 
! NOTE: USED ONLY (!!!!) FOR PEOPLE IN JAIL (NOT OPTIMIZING AGENTS)

	IMPLICIT NONE

	INTEGER, INTENT (in)		:: lage, lialp , ledu , lstind, iwe,izet, ljail
	REAL (long), INTENT (out)	:: lval
	REAL(long)	:: lbequest,lconsump,futval,assetprime 
	
	REAL(long), EXTERNAL :: discount

	lval = 0.0d0
	futval = 0.0d0

	lconsump	= con(ledu,lage,iwe,izet,lialp,lstind,ljail)

	CALL  PUTILITY (lage,lconsump,lstind,ljail,lval)
        
    ! mod(ljail,2)==0 People who enter jail this period
    assetprime = sav(ledu,lage,iwe,izet,lialp,lstind,ljail)*(1.d0+ir(lage))**(MAX(1,prisT)-1)

    IF (lage+prisT>lifet) THEN
        futval = 0.d0
    ELSE     
        CALL evalue(ledu,lage,assetprime,izet,ljail,lialp,futval) 
    ENDIF   ! if lage+prisT>lifet
    lval = lval*discount(bita,prisT,lage,1)+(bita**prisT)*discount(1.d0,prisT,lage,3)*futval
    if (lage==beqage) then 
        ! Bequest when in jail ~ 0
        lbequest = 1.d-7
        if (eta==1) then 
            lval = lval + gamma_1*LOG(gamma_2+lbequest)
        else
            lval = lval + gamma_1/(1.d0-eta)*(gamma_2+lbequest)**(1.d0-eta)
        endif
    endif

CONTAINS

!------------------ EVALUE  -------------------------------------------

SUBROUTINE EVALUE (ledu,lage,aprime,lizet,ljail,lialp,evaluep)
!ROUTINE FOR COMPUTING EXPECTED value IN NEXT PERIOD GIVEN
!NEXT PERIOD WEALTH - Note that age, ialp, fixedeffect, st_ind are all known globals here
    IMPLICIT NONE

	REAL (long), INTENT (in)	:: aprime
	REAL (long), INTENT (out)	:: evaluep
	INTEGER, INTENT (in) :: ledu,lage,ljail,lialp,lizet 
	INTEGER :: i,increment,j,cr_state

	REAL(long) :: evint,evres,ucbar

	evaluep = 0.0d0	! initialize output variable
    evint = 0.0d0	! initialize intermediate output variable for integer prison term
    evres = 0.0d0	! initialize intermediate output variable for fractional prison term

    ! Agents who enter jail this period (they remain in jail for at least MAX(1,prisT))
    ! Their jail equals ljail-1 (currently even) when coming out of jail
    call sub_myinterp1(agrid(:,lage+MAX(1,prisT)),eval(ledu,lage+MAX(1,prisT),:,(ngpz+1)/2,lialp,ljail-1),aprime,evint)

            
    IF (prisT==0) THEN 
    evaluep = evint
    ELSE    ! prisT>0
        IF (prisTres>0)  THEN   
			if (lage+prisT<lifet) then 
			    call sub_myinterp1(agrid(:,lage+prisT+1),eval(ledu,lage+prisT+1,:,(ngpz+1)/2,lialp,ljail-1),&
    				aprime*(1.d0+ir(lage+prisT)),evres)
    			CALL PUTILITY (lage,cbar,lstind,ljail,ucbar)  ! Dummy argument for hours
				evres = ucbar + bita*surprob(lage+prisT)*evres
				if (lage+prisT==beqage) then
				    ! Bequest when in jail ~ 0
                    lbequest = 1.d-7
                    if (eta==1) then 
                        evres = evres + gamma_1*LOG(gamma_2+lbequest)
                    else
                        evres = evres + gamma_1/(1.d0-eta)*(gamma_2+lbequest)**(1.d0-eta)
                    endif
				endif
			else !Individuals is dead by the end of the integer part of the sentence
				evres = 0.d0 ! 
			endif		
        ENDIF
	evaluep=(1-prisTres)*evint+prisTres*evres
	ENDIF    
       

ENDSUBROUTINE EVALUE

!------------------ sub_myinterp1 -------------------------------------
subroutine sub_myinterp1(x,f_x,xp,interp_value)
					 
	implicit none

	real(long), dimension(:), intent(in)    :: x, f_x
	real(long), intent(in)                  :: xp
	real(long), intent(out) :: interp_value
	
	integer, dimension(1) :: x_min
	integer :: x_index
	real (long) :: t
	
	
	
		x_min = minloc(abs(x-xp))
		x_index = x_min(1)
	
		if (xp<x(x_index)) x_index = x_index-1
		x_index = max(x_index,1)
		x_index = min(x_index,size(x)-1)
		t = (f_x(x_index+1)-f_x(x_index))/(x(x_index+1)-x(x_index))
		interp_value = t*(xp-x(x_index))+f_x(x_index)
	
end subroutine sub_myinterp1


ENDSUBROUTINE VFUNC

!********************************************
! W
SUBROUTINE WAGEDIST (n,year,vdist,lavwage,lmedwage,lvarwage,lwage10,lwage90)
! Get wealth statistics
	USE global1
	USE global2
	
!	INPUTS
	INTEGER, INTENT (in) :: n, year
	REAL (long), DIMENSION (n), INTENT (in) :: vdist
!	OUTPUTS
	REAL (long), INTENT (out) :: lavwage,lmedwage,lvarwage,lwage10,lwage90
!	LOCAL VARIABLES
	REAL (long), DIMENSION (n) :: wdinc, wdsc
	REAL (long) :: 	temp
	INTEGER :: i, temp_2, nlz
	CHARACTER(100) :: namedata, namecommand 

!	Sort wealth in ascending order
	CALL DSVRGN (n, vdist, wdinc) 
!	COMPUTE CUMULATIVE DISTRIBUTION OF ASSETS
	temp=0.0
	DO i=1,n
	wdsc(i)=temp+wdinc(i)
	temp=wdsc(i)
	ENDDO

	
!	COMPUTE VARIOUS PERCENTILES OF DISTRIBUTION
	
	lwage10=wdinc(NINT(1.0*n/10.0))
	lwage90=wdinc(NINT(9.0*n/10.0))
	! AVERAGE WEALTH
	lavwage =	SUM(wdinc) / REAL(n,long)
	! VARIANCE
	lvarwage = SUM(wdinc**2.d0)/REAL(n,long) - lavwage**2.d0
	! MEDIAN WEALTH
	lmedwage =	wdinc(nint(n/2.d0))

ENDSUBROUTINE WAGEDIST


!********************************************
! Y
SUBROUTINE YOUNGWEALTH (exend, n, year, vdist, w_age1,lreswealth)
! Routine for computing initial wealth distribution of youngest age group
! NOTICE: if gensize=X and maxage=Y, then n (dimension of vdist) is approximately X*Y/20.
! For example, with gensize=10000 and Y=99, n is slightly larger than 50000.
! We need to assign only 10000 wealth values to the newborns, in a random way. However, vdist's values
! are stored sequentially by age and we don't sample over the whole of vdist we might be oversampling
! assets for lower age groups. We use a simple random selection to control for this problem.

	USE global1
	USE global2
	USE numerical_libraries
	INCLUDE 'link_fnl_shared.h'	
	!DEC$ OBJCOMMENT LIB:'libiomp5md.lib'
	
    !INPUTS
	INTEGER, INTENT (in) :: exend, n, year
	REAL (long), INTENT (in) :: vdist(n)
	!OUTPUTS
	REAL (long), INTENT (out) :: w_age1(gensize), lreswealth
	!LOCAL VARIABLES
	REAL(long) :: rj1(1), rj2(n), COUNTER, temp(n), temp1(gensize), wdinc(gensize), nlzr, pwza, avminus, &
	avplus, avcens, percnt
	INTEGER :: i,j,k,l, temp2, nlz
	CHARACTER(100) :: namedata, namecommand

	nlz=SIZE(vdist)
	IF (exend==1) RETURN	! **** If exend==1, young wealth dist is exogenous and this subroutine gives
							! control back to the calling program
	w_age1 = 0.0d0
	j=1 ; l=1
	DO WHILE (j<gensize+1)
		CALL DRNUN(1,rj1(1))
		l=NINT(rj1(1)*n)
		l=MIN(MAX(l,1),n)
		w_age1(j)=vdist(l)
		j=j+1
		IF (j>gensize) EXIT
	CONTINUE
	ENDDO

	! SUM OF BEQUATHED WEALTH
	avcens = SUM(vdist)

	! **** Compute the average for the negative (and +tive) parts of the distribution
	temp(:)=0.d0
	WHERE (vdist(:) < 0.d0)
	temp(:)=1.d0
	endwhere
	counter=SUM(temp)
	avminus=SUM(temp(:)*vdist(:))/counter	! Average among negatives
	avplus=(AVCENS-counter*avminus)/(REAL(nlz)-counter) ! Average among positives


	! Compute the average accidental bequest
	lreswealth=AVCENS/REAL(nlz,long)

	! CHECK THAT SUM OF ALL BEQUESTS IS POSITIVE 

	! LOCATION OF THE INITIAL ASSETS DISTRIBUTION
	! the new mean should be such that all bequitted wealth is given to newborns
	IF (avcens>0.d0) THEN
	WHERE (w_age1(:) > 0.d0)
	w_age1(:) = MAX(w_age1(:)-avplus &
	+ lreswealth*REAL(nlz,long)/(REAL(nlz,long)-counter),0.d0)	
	ELSEWHERE
	w_age1(:)=0.d0
	endwhere
	ELSE
	w_age1=0.d0
	ENDIF


	!Sort w_age1 in ascending order
	CALL DSVRGN (gensize, w_age1, wdinc) 
	!COMPUTE FRACTION WHICH IS BORROWING CONSTRAINED
	temp2 = 0;	nlz = 0
	DO i=1,gensize
		IF (wdinc(i)<=critw) THEN
		nlz=temp2+1
		temp2=nlz
		ELSE
		EXIT
		ENDIF
	ENDDO
	nlzr=REAL(nlz)
	pwza=nlzr/REAL(gensize)

	! Newborns wealth as a percentage of total wealth
	percnt=(SUM(w_age1)/REAL(gensize,long))/(avwealth*SUM(popsize))


    ! meanstestw1 = wealth threshold for means-testing in social programmes
    ! we set it equal to one of the available percentiles of the wealth distribution of newborns.
    ! To do this we use a "character" variable with the name of the appropriate percentile.
	meanstestw1 = wdinc(NINT((wpercent*gensize)))

PRINT *, "***************************************************************"
PRINT '(A50,F14.4)'," % of youngest age group with wealth <=0  ", pwza
PRINT '(A50,F14.4)'," Average assets in youngest age group     ", SUM(w_age1)/REAL(gensize,long)
PRINT '(A50,F14.4)'," Assets SD in youngest age group          ", (SUM(w_age1**2.d0)/REAL(gensize,long)-(SUM(w_age1)/REAL(gensize,long))**2.d0)**.5d0
PRINT '(A50,F14.4)'," Median  assets in youngest age group     ", wdinc(gensize/2)
PRINT '(A50,F14.4)'," Min asset level in youngest age group    ", wdinc(1)
PRINT '(A50,F14.4)'," Max asset level in youngest age group    ", wdinc(gensize)
PRINT '(A50,F14.4)'," % of tot. wealth held by youngest        ", percnt
PRINT *, '	'

OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND') 
WRITE(38,*),' '
WRITE(38,*),'***********************************'
WRITE (38,'(A40,F14.5)'), " % of youngest age group with wealth <=0  ", pwza
WRITE (38,'(A40,F14.5)'), " Average assets in youngest age group     ", SUM(w_age1)/REAL(gensize,long)
WRITE (38,'(A40,F14.5)'), " Assets SD in youngest age group          ", (SUM(w_age1**2.d0)/REAL(gensize,long)-(SUM(w_age1)/REAL(gensize,long))**2.d0)**.5d0
WRITE (38,'(A40,F14.5)'), " Median  assets in youngest age group     ", wdinc(gensize/2)
WRITE (38,'(A40,F14.5)'), " Min asset level in youngest age group    ", wdinc(1)
WRITE (38,'(A40,F14.5)'), " Max asset level in youngest age group    ", wdinc(gensize)
WRITE (38,'(A40,F14.5)'), " % of tot. wealth held by youngest        ", percnt 
CLOSE(unit=38)

		namedata='wealthy.txt'
		CALL save_vector(namedata,40,size(w_age1),w_age1)

END SUBROUTINE Youngwealth

!***************************************************


ENDMODULE PROCS
