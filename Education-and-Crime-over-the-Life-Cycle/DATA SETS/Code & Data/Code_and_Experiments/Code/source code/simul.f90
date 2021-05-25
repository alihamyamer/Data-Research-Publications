SUBROUTINE Simul(ftracking,vtracking) ! Simulate Economy

  use global1
  use global2
  use global3
  use procs
  use decisions
  use schoolchoice
  use READ_parameters
  USE int_nodes
  USE ZREAL_INT
  USE RANKS_INT
  USE RLSE_INT
  USE qsort_c_module
  implicit none
  INCLUDE 'link_fnl_shared.h' 
  !DEC$ OBJCOMMENT LIB:'libiomp5md.lib'

  ! intent(out)
  real(long),intent(out),dimension(gensize) :: ftracking,vtracking    

  ! Local COUNT AND UTILITY Variables
  integer ::	id,qwzero,lage,ledu,lstind,i,j,i1,i2,i3,ii,j1,j2,j3,jr,cloc,&
  diml,itera,out(1),s_index,regpos(4), topcode_loc, maxzero_loc, q_size
  real(long) :: in(1)
  integer, dimension (gensize) ::	sw,ljail,noljail,lcr_st,&
       eligible,treated,takeup,zet_id,zetlag,res_id,res_id_old,ind
  real (long), dimension(gensize) :: initialw,r0,r1,r2,r3,r4,rsamp,&
       wealthi,inci,incipt,consi,houri,temp,tempbis,denomin,inter_vivos, &
       inter_vivos_sort

  ! TRACKING variables	
  integer, dimension (gensize) ::	etracking,stracking,rtracking,ctracking,atracking,&
       atracking_old,atracking_old2,BCtracking,intctracking,recidtracking,sample_track,forcedtrack
  real(long), dimension(gensize) :: ttracking,vdiftracking,wtracking
  real(long), dimension(retage-1) :: numapprtot2, numrecid2

  ! DISTRIBUTION STATISTICS
  real (long), dimension(ngpf-1) :: numabil
  ! by EDU group
  real (long), dimension(nedu) :: grpincw,uncondedu,numcrimtot,numworktot,numrecidtot,&
       numapprtot,numunapprtot,teenjailed,jailedbyedu,avabibyedu
  ! by EDU,STATUS,ABILITY,PRISON STATUS
  real (long), dimension(nedu,lifet,ngpst,ngpf-1,2) :: numheads_age ! General count variable    
  ! by EDU,STATUS,ABILITY,PRISON STATUS
  real (long), dimension(nedu,ngpst,ngpf-1,2) :: numheads ! General count variable
  ! by EDU group and STATUS
  real (long), dimension(nedu,ngpst) :: gaggeffs, eagginc, avgconspreret, &
       avgabil,avgwage,sdwage,lavgwage,lsdwage,avgearn,gaggtransf,avtransfrec	
  ! by EDU group, AGE and STATUS
  real (long), dimension(nedu,lifet,ngpst) :: transfshare
  ! by EDU group, AGE and ABILITY group
  real (long), dimension(nedu,lifet,ngpf-1) :: edudist,numwork,numjailed
  ! by EDU group, ABILITY and STATUS
  real (long), dimension(nedu,ngpf-1,ngpst-1) :: avgconspret_abedu ! No students
  real (long), dimension(nedu,ngpf-1,ngpst) :: avginc	
  ! by EDU group and ABILITY
  real (long), dimension(nedu,ngpf-1) :: condedu
  ! by EDU and AGE
  real (long), dimension(nedu,lifet) :: numjailedtot ! Number of inmates (headcount)
  ! by AGE and STATUS
  real (long), dimension(LIFET,ngpst) :: statushare
  real (long), dimension(nedu,maxcrim)		:: int_cstat   ! Number of criminals (headcount)
  ! for each level of crime intensity

  REAL (long):: ygini, pw15, yrwza, ywpnw, yavwealth, ymedwealth, yavwealth0, ymedwealth0, yvarwealth, intensity16
  ! WEALTH, WAGE and INCOME statistics and utility variables
  real (long), dimension(ss, lifet) :: wealthdistss, incdistss,wgidistss
  real (long), dimension(ss*lifet)  :: wdistinc_2,wdistwgi,wdist3
  real (long), dimension(nedu,gensize) :: wgi
  real (long), dimension (ss) :: rv,wdist_temp
  integer, dimension(ss) :: dead
  real (long), dimension(ngpst,lifet,nedu) :: bcpeople
  real(long) :: ztcum(ngpz,ngpz,nedu),zstatcum(ngpz,nedu)

  ! External Functions
  real(long), external :: con_beq,Vfunction,TRANSF,TRANSF2,discount

  ! TREATMENT STATISTICS
  real(long) :: pcteligible, pcttreatedcond, pcttreated, pcttakeup(ngpf-1)   ! Shares of eligble people, share treated conditional on being
  ! eligible, share treated unconditional on being eligible, share of take-up of programme conditional on treatment
  real (long), dimension(LIFET,4,7) :: stats_pr   ! Statistics to study effect of programme/intervention
  real (long), dimension(LIFET,2,3) :: arrest_pr
  ! for each age, there are 4 groups 1=(eligible,treated,takeup),2=(eligible,treated,notakeup),
  ! 3=(eligible,untreated),4=(non eligible). For each of the 4 groups we save:
  ! 1=average wealth,2=average wage,3=average ability,4=average income and more...
  real (long), dimension(4,ngpf-1,nedu) :: edushare_pr   ! Edu shares by ability of different groups after means-tested programme
  real(long) :: aggstats_pr(4,7) ! integration over age of stats_pr

  ! Vector Storage variables
  real(long) :: valvec(ngpz,maxcrim+1) ! vector storing vfuction for each value of criminal choice 
  real(long) :: valstore  ! stores the expected value - storage variable 

  !	CRIME STATISTICS and VARIABLES
  REAL (long), DIMENSION(nedu,retage-1,ngpf-1) :: numcriminal,numcrimes,numappr,numappr2
  real (long), dimension(1:retage-1) :: numapprage	
  ! Abs.numb. of criminals, number of crimes,absolute number of criminals apprehended in the current period
  REAL (long), DIMENSION(nedu,retage-1) :: numrecid, numunempappr
  ! Absolute number of recidivists, absolute number of apprehended who were unemployed
  REAL (long), DIMENSION(nedu,retage-1,ngpf-1) :: ccrime		! crime rates by (edu,age,ability)
  REAL (long), DIMENSION(nedu,retage-1)		:: ccrime_intab ! crime rates by (edu,age)
  REAL (long), DIMENSION(retage-1,ngpf-1)	:: ccrime_intedu ! crime rates by (age,ability)
  real (long), dimension(nedu,ngpf-1)		:: ccrime_intage, numcriminal_abiedu,avgwageABEDU ! crime rates by (edu,ability)
  real (long), dimension(nedu)			:: ccrime_intabage	! crime rates by (edu)
  REAL (long), DIMENSION(retage-1)		:: ccrime_intabedu	! crime rates by (age)
  real (long), dimension(ngpf-1)			:: ccrime_intagedu,criminal_byabi	! crime rates by (ability)
  real (long), dimension(ngpf-1)			:: numcrimes_intagedu	! # of crimes by (ability)
  real (long), dimension(maxjail/2-1,nedu,ngpf-1)  :: ccrime_cgrid,selec_cgrid ! Crime part. rates by cgrid,edu,abi
  REAL (long), DIMENSION(maxjail/2-1,nedu,retage-1,ngpf-1)  :: criminal_cgrid,work_cgrid ! # criminals and workers part. rates by cgrid,edu,abi
  real (long)								:: ccrime_agg	! aggregate crime rate
  real (long)								:: victim_agg	! aggregate victimization rate
  real (long)								:: teenjailed_agg	!Share of prisoners with age<19
  real (long)								:: LTHSjailed_agg	!Share of prisoners with LTHS
  real (long)								:: recidiv_agg      ! Recidivist as a share of apprehended
  real (long)                             :: recidiv_agg2     ! Recidivist as a share of people released from jail in the previous period
  real (long)								:: unempappr_agg      ! Unemployed as a share of apprehended
  REAL (long), DIMENSION(retage-1)		:: piece1 , piece2
  real (long), dimension(ngpf-1)			:: pieceA , pieceB
  real (long), dimension(nedu)			:: pieceAlpha , pieceBeta

  ! Various Stats and Variables
  real (long) ::	aggwealth,agginc,aggeffs,wealthzero,wealthzeroW,&
       wginiss,w9010ss,w7525ss,fwlzss,wzss,stdvchss,stdvcwss,corrdhdw,avghw,corrhw,avghc,&
       corrhc,avghlc,corrhlc,stcash,schship,emucp,muc,gap,PVtuit,checkvar,tempvar1,tempvar2,&
       tempvar3,tempvar4,incrementvar1,incrementvar2,incrementvar3,devl,devl_old,&
       meanwage,medwage,varwage,wage10,wage90,x_temp(1),x_guess(1),totw16_24,totw25plus, &
       avgearn_drop16,ind_drop16 

  real(8):: time0,time1,time2,delta_t,rtc

  ! Allocatables. Characters.
  REAL(long),ALLOCATABLE :: errvec(:),errvec_old(:),randvar(:),abirank(:),&
  earnreg(:),wreg(:), ctrackreg(:),idreg(:),matreg(:,:),coefreg(:),strackreg(:)
  character(100) :: series1, plot, hist, namedata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! END OF DEFINITIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!	LEGENDA		!!!!!!!!!!!!!!!!!!!!!!!!!
  ! INCI is income, INCIPT is pre-tax income                      !
  ! WGI is wage     !
  ! AGGEFFS is aggregate effective hours                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  diml=2*(ngpf-1)
  ALLOCATE (errvec(diml),errvec_old(diml))

  errvec = 0.d0
  errvec_old = 0.d0
  itera=0

  
101 itera=itera+1	
  IF (itera>200) THEN !Exit if quasi-linear education terms do not converge
     failed_conv = 1
     DEALLOCATE(errvec,errvec_old)
     DEALLOCATE(eval,edval,seval,sedval)
     GOTO 1001
  elseif (itera>100) then 
    pctval = 0.01d0
  endif

  do id = 1,gensize
     sw(id) = 0				! everybody has at least compulsory schooling 
  enddo

  TUIT_LAG = TUIT ! SAVE TUITIONS FROM LAST ITERATION
  continue

  ! ***** SET CONSTANT SEED FOR SIMULATION  *****
  CALL RNSET(iseed)

  ! Compute some cost parameters: EMME, TUIT(),... SAVE USEFUL VARS.

  ! ***** TUITION COSTS *****
  if (tuitquant==0) then
     ! As a share of average earnings
     TUIT (1) = MAX(0.01d0 * UNCavgearn , 0.0D0) !	TUIT (1) = MAX(0.015d0 * MEDINC , 0.0D0)
     TUIT (2) = MAX(0.097d0  * UNCavgearn , 0.0D0)	! Main benchmark tuitions 
  endif
  ! EMME - offsetting term
  ! Here we set emme - the offsetting term which allows 
  ! cbar+emme=(1+0.18d0/19*12)*0.67*UNCavgearn (incarceration + policing costs)
  if (emmeflag==1) then
     emme = (1+0.18d0/19*12)*0.67d0*UNCavgearn - cbar 
  endif
  ! SAVE: (1) WEALTH DIST. OF YOUNG; (2) EDU FREQUENCY BY (AGE,STATUS); (3) Tuition Costs
  series1='wealthy.txt'
  CALL save_vector(series1,40,size(wealthi_age1),wealthi_age1) ! Save current young wealth
  series1='tuitions.txt'
  CALL save_vector(series1,42,NEDU,tuit) ! Save current young wealth

  wealthzero = 0.d0       ! Share of total population with wealth less or equal critw
  wealthzeroW = 0.d0      ! Share of NON STUDENTS with wealth less or equal critw
  totw16_24 = 0.d0        ! Total wealth age 16-24 
 incrementvar1 = 0.0d0    ! Cumulative sum of NON STUDENTS in the economy
  recidtracking = 0       ! Number of spells in jail

  !LJAIL and NOLJAIL are used to determine hours as a function of being in 
  !or out of jail (hours are indepedent of utility from crime)
  ljail=2     
  noljail=ljail-1

  ! Initialize recursive targets
  moms(3) = 0.d0 
  moms(4) = 0.d0 

  ! NOTICE: Cgrid nodes vary between 1,2, ..., maxjail/2. 
  !CRIM_FIXED IS SET IN MAIN.F90 (BY RANDOM DRAW) AND IT DETERMINES THE "Quasilinear utility term from CRIME". 
 
  
  !**********************************************************************************************
  ! **** START AGE LOOP ****
  do LAGE = 1,LIFET	 

     ! ******* DRAWS FOR RANDOM SHOCKS ******* 

     res_id_old=res_id ! Save last draw before updating
     CALL DRNUN (gensize,r1)	! UNIFORM DRAW FOR ADDITIONAL YEAR IN PRISON
     WHERE (R1<prisTres)
        res_id = 1           ! Spends additional year in jail
     ELSEWHERE
        res_id = 2
     ENDWHERE

     CALL DRNUN (gensize,r2)	! UNIFORM DRAW FOR EMPLOYMENT SHOCK
     !Notice:the right zet_id depend on age and edu. Hence, they are assigned within the DO GENSIZE

     CALL DRNUN (gensize,r3)	! UNIFORM DRAW FOR ROBBED/NOT ROBBED
     WHERE (R3<PI_V(1))
        robbed = 1
     ELSEWHERE
        robbed = 2
     ENDWHERE

     CALL DRNUN (gensize,r4)	! UNIFORM DRAW FOR jail/NOT jail (apprehened or not)
     if (prisT==0 .and. prisTres>0) then
	    do i1=1,maxcrim
           WHERE (R4< PI_A(1,i1) .and. R1<prisTres)
              inmate(:,i1) = 1
           ELSEWHERE
              inmate(:,i1) = 2
           ENDWHERE
        enddo
     else
	    do i1=1,maxcrim
           WHERE (R4< PI_A(1,i1))
              inmate(:,i1) = 1
           ELSEWHERE
              inmate(:,i1) = 2
           ENDWHERE
        enddo
     endif
     ! ******* END OF DRAWS FOR RANDOM SHOCKS ******* 

     if (lage<retage) then 
        ! Compute cumulative probs from ztrans() conditional on ledu
        do ledu=1,nedu
           ztcum(:,1,ledu) = ztrans(:,1,lage,ledu)
           zstatcum(1,ledu) = zstat(1,ledu)
           do i1=2,ngpz
              ztcum(:,i1,ledu)=ztcum(:,i1-1,ledu)+ztrans(:,i1,lage,ledu)
              if (lage==1) then
                zstatcum(i1,ledu)=zstatcum(i1-1,ledu)+zstat(i1,ledu)
              endif
           enddo ! i1 - ngpz
         enddo ! enddo ledu
     endif

     atracking_old2 = atracking_old
     atracking_old = atracking ! SAVE THE LAST RECORD OF APPREHENSION


     ! Initialize share of people with wealth below a certain limit (critw)
     qwzero    = 0	! Number of individuals with wealth less than zero

     ! IF LAGE == 1 *******
     if (lage==1) then   

	    wealthi = wealthi_age1
	    if (unc_qop==1) then 
           ! Give EVERYBODY an unconditional transfer of same size as QOP
           wealthi = wealthi_age1 + propsub*tuit(1)*(1.d0+DISCOUNT(ir(lage),eduyr(2)-1,lage,2))
        endif
        where (wealthi>wmax0)
           wealthi = wmax0
        endwhere

        INITIALW = wealthi    ! Save the start-of-life wealth distribution to build statistics
    ! meanstestw1 = wealth threshold for means-testing in social programmes
   	CALL DSVRGN (gensize,INITIALW,temp) 
	meanstestw1 = temp(NINT((wpercent*gensize)))

		aggwealth= (SUM(wealthi)/gensize)*popsize(lage)	! initialize aggregate wealth at age 1 
		wealthdistss(:,lage) = wealthi(1:ss)			! wealth distribution at age 1

  ! VARIABLES INITIALIZED AT LAGE==1
        agginc= 0.0d0;			aggeffs = 0.0d0	;	avgwage = 0.0d0;    sdwage=0.0d0
        lavgwage=0.d0;          lsdwage=0.d0;    
        avgconspreret= 0.0;		avgabil = 0.0d0;	numheads= 0.0d0;    numheads_age= 0.0d0
        ABIFRAC= 0.0d0;			avginc	= 0.0d0	;	edufractot = 0.0d0
        gaggeffs= 0.0d0;		eagginc	 = 0.0d0;	avgconspret_abedu = 0.0d0;
        TAXrev	= 0.0d0;		transfshare = 0.d0;	avtransfrec = 0.0d0;zet_id = (ngpz+1)/2
        gaggtransf = 0.0d0;		etracking=0;		ftracking = 0.d0;	statushare(:,1)=0.d0
        statushare(:,2)=0.d0;	stracking=0;		ttracking = 0.d0;	edufrac = 0.0d0;
        atracking=0;			ctracking=0;		rtracking=0;		ABIFRACW= 0.0d0;
        numcriminal=0.d0;		ccrime=0.d0;		ccrime_intedu=0.d0; atracking_old=0.d0
        ccrime_intab=0.d0;		ccrime_intage=0.d0;	ccrime_intabedu=0.d0; ccrime_intagedu=0.d0;
        ccrime_intabage=0.d0;	ccrime_agg=0.d0;	numwork =0.0d0;		numappr = 0.0d0;
        numrecid=0.d0;          numunempappr=0.d0;  numrecidtot=0.d0;   numunapprtot=0.d0;
        numjailed=0.0d0;		BCtracking=0;		vdiftracking=-1.d5;	PVtuit = 0.0d0;
        wtracking=0.0d0;		aveabi=0.0d0;		avewealth=0.0d0;    avgearn=0.0d0;
        eligible=0;             treated=0;          takeup=0;           numapprtot=0.d0
        numappr2 = 0.0d0;       numapprtot2=0.d0;   numrecid2=0.d0;		work_cgrid=0.d0
        pcteligible=0.d0;       pcttreatedcond=0.d0;pcttreated=0.d0;    pcttakeup=0.d0;
        numcrimtot=0.0d0;       numworktot=0.0d0;   intctracking=0;     numcrimes=0.d0;
        victim_agg=0.d0;        numcrimes_intagedu=0.d0;                numjailedtot=0.d0;
        teenjailed=0.d0;        jailedbyedu=0.d0;   teenjailed_agg=0.d0; LTHSjailed_agg=0.d0;
        abifracwf=0.d0;         avgwageABEDU=0.d0;  criminal_cgrid=0.d0; int_cstat = 0.d0;
        numapprage = 0.d0;      avgearn_drop16=0.d0; ind_drop16=0.d0;   forcedtrack = 0

        incdistss = -1.d5 ! Initialize to very low value to distinguish elements that may remain unchanged
        wgidistss = -1.d5
     endif
     ! END IF LAGE == 1 *******

      ! FIND QUASILINEAR TERMS FOR EDUCATION ENROLMENT (LEISURE)
     if (lage==1) then
        leisure_save=leisure		
		CALL ENROLMENT(ledu=1,lage=lage,lwealthi=wealthi,lifixed=i_fixed,ldraw=ldrawHS,&
             lstracking=stracking,lcr_st=crim_fixed)
		continue
     else if (lage==eduyr(2)+1) then
		CALL ENROLMENT(ledu=2,lage=lage,lwealthi=wealthi,lifixed=i_fixed,ldraw=ldrawC,&
             lstracking=stracking,lcr_st=crim_fixed)
		errvec_old = errvec
		errvec=dabs( (leisure-leisure_save)/leisure_save )
        IF (  ANY( errvec(1:9)> 2.d-1 .OR. errvec(10)>2.d-1) ) THEN ! 1.d-3
  		i = maxloc(errvec,1)
  		devl = errvec(i)
          PRINT *, "Iteration and max deviation", itera, devl,i, leisure(i),leisure_save(i)
          IF ( ANY(errvec > errvec_old) .AND. itera>1) THEN 
              ALLOCATE(randvar(itera))
              CALL DRNUN(itera,randvar)
              PRINT *, "Random increment",(randvar(itera)-0.5)*.4
             pctval = MAX(0.1,MIN(pctval,pctval + &
                   (randvar(itera)-0.5d0)*.4d0 ) )
              DEALLOCATE(randvar)
           ENDIF
          WHERE( errvec(1:9) > 2.d-2) 
              leisure(1:9)=leisure(1:9)*pctval+leisure_save(1:9)*(1.d0-pctval)
          endwhere
          IF (errvec(10) > 1.d-1) THEN
              leisure(10)=leisure(10)*pctval+leisure_save(10)*(1.d0-pctval)
          ENDIF
               
            CALL save_leisure
          
            
            CALL DECISIONS_ST
            GOTO 101
         ELSE
            CALL write_enrolment(ledu=1)
            CALL write_enrolment(ledu=2)
           DEALLOCATE(errvec,errvec_old)
            DEALLOCATE(eval,edval,seval,sedval)
         ENDIF
         CONTINUE
     endif
     ! END OF PROCEDURE TO FIND QUASILINEAR TERMS FOR EDUCATION ENROLMENT (LEISURE)
     time0=rtc( )
     time1=rtc( )

     ! ********************* BEGINNING OF "DO GENSIZE" ********************************
     ! LOOP over ALL INDIVIDUALS WITHIN AN AGE GROUP

     DO id = 1,GENSIZE
        ! Save initial wealth for each agent (i.e., wealth at beginning of period)
        wealth = wealthi(id)
        
        ! ASSIGN WAGE (DEPENDENT EDUCATION AND AGE)
        do ledu = 1,nedu		
           if (lage<retage) then
              wgi(ledu,id)   = wagetge(ledu) * exp(eff(ledu,lage) + abweight(ledu)*i_fixed(id))
           else
              wgi(ledu,id)=0.d0
           endif
        enddo   ! enddo over ledu


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Compute optimal consumption given realization of shocks for agent id - consi(id)
        ! Also computed: (1) Hours worked or studied - houri(id)
        !                (2) after tax income - inci(id)
        !                (3) pretax income - incipt(id)
        !                (4) substitute current wealth with future wealth - wealthi(id)

        ! ******** SELECT and ASSIGN EDUCATION GROUP and INCOME SHOCK ***************
            in(1)=R2(id)
		    if (sw(id) < eduyr(2)) then
               ledu=1
               if (lage==1) then 
                   ! First working year draw is from white noise distribution
                  call int_node(zstatcum(:,ledu),in,out)
               else 
                  call int_node(ztcum(zet_id(id),1:ngpz,ledu),in,out)
               endif 
		    elseif (sw(id)>eduyr(2)-1 .and. sw(id)<sum(eduyr)) then
               ledu=2
               if (lage==eduyr(2)+1) then 
                   ! First working year draw is from white noise distribution
                  call int_node(zstatcum(:,ledu),in,out)
               else 
                  call int_node(ztcum(zet_id(id),1:ngpz,ledu),in,out)
               endif 
		    elseif (sw(id)>sum(eduyr)-1) then
               ledu=3
               if (lage==sum(eduyr(:))+1) then 
                   ! First working year draw is from white noise distribution
                  call int_node(zstatcum(:,ledu),in,out)
               else 
                  call int_node(ztcum(zet_id(id),1:ngpz,ledu),in,out)
               endif 
		    endif
		    zet_id(id)=out(1)
		
  !***********************************************************
        
        OUTER_LAGE:	if (lage==1) then   !***********************************

           wtracking(id)=wealthi(id)
           ledu=1
           
           ! MEANS-TESTED programme variables ***
           if (geneq==2) then
            if (rndtuit2(id)<=1.0d0) eligible(id)=1 ! random eligibility
           else 
            if (wealthi(id)<=meanstestw1)    eligible(id)=1 ! Eligible target population
           end if ! geneq==2

           if (subregime<10) then           
           schship=transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))
           else if (subregime==10) then ! This is used to force people into HS
           schship=transf2(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu),eligible(id))
           endif    ! transf function selection

           if (schship>0.d0)    treated(id)=1   ! Treated population
           PVtuit=tuit(ledu) *(1.d0+DISCOUNT(ir(1),eduyr(2)-1,1,2))
           ! SUBSIDIES PAID UPFRONT IN PDV IN THE FIRST PERIOD  (no myopia)
           do i=1,eduyr(2)-1
              schship=schship+transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))/((1.d0+ir(lage))**i)
           enddo

           stcash=wealthi(id)+schship-PVtuit	! ******* STUDENT CASH (CONDITIONAL ON BEING A STUDENT)

           ! COMPARE EXPECTED VALUE OF CURRENTLY WORKING TO VALUE OF CURRENTLY STUDYING

           do i=1,ngpz
              do j=0,maxcrim  ! different crime intensities
                 valvec(i,j+1) = Vfunction(ledu,lage,wealthi(id),i,i_fixed(id),j,crim_fixed(id))
              enddo
           enddo

           ! Agents choose whether to study or not before observing the realization of zet_id
           valstore = 0.d0
           do i=1,ngpz
           valstore = valstore+zstat(i,ledu)*maxval(valvec(i,:))
           enddo
           ! The following if sets the value of work to -INFINITY for treated(id)=1
           ! This is equivalent to forcing all people who receive a policy (treatment) to take it up 
           if (forcepol==1 .and. treated(id)==1)  then
            if (valstore > Vfunction(ledu,lage,stcash,1,i_fixed(id),-1,crim_fixed(id))+ldrawHS(id)) then
            forcedtrack(id) = 1
            end if ! working better than studying -- but will be forced to study!
           valstore=-1.d18
           endif ! (forcepol==1 .and. treated(id)==1)

           IF_HS: IF (valstore > Vfunction(ledu,lage,stcash,1,i_fixed(id),-1,crim_fixed(id))+ldrawHS(id)) then

              sw(id) = sw(id) ! not studying

              ! Ingredients to compute average dropout earnings at age 1 
              avgearn_drop16  = avgearn_drop16 + wgi(ledu,id)
              ind_drop16 = ind_drop16 + 1.d0
              
              if (forceHS1==1) then
                 sw(id)=sw(id)-1 ! set sw(id)=-1 so that it will be equal to zero after one year of edu (and the agent will drop out)
                 goto 201 
              endif !forceHS1==1
              continue

              ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME

              cloc = maxloc(valvec(zet_id(id),:),1)-1 ! CHOOSE OPTIMAL CRIME INTENSITY

              ! Save value function for this agent
              vtracking(id) = valvec(zet_id(id),cloc+1)

              if (cloc==0) then    ! Assign st_ind according to crime intensity (# OF CRIMES)
                 st_ind=4
              else if (cloc==1) then
                 st_ind=2
              else
                 st_ind = (cloc*2)+2
              endif

              AGE1CRIME: if (cloc > 0) then     

                 ctracking(id) = 1	! Record of criminal activity
                 intctracking(id) = cloc	! Record of crime intensity (how many crimes he did)

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                 AGE1jail: if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                    atracking(id) = 1	! Record of being apprehended
                    recidtracking(id) = recidtracking(id) + 1   !One more prison spell 	
                    rtracking(id) = 0
                    st_ind=st_ind	
                    consi(id) = cbar
                    houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

		            wealthi(id)= wealthi(id) + inci(id)

                 else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL (or, if prisT=0, apprehended and released)
                    atracking(id) = 0	! Record of being a criminal NOT apprehended

                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = net labour and capital income (income from crime is not taxed)
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif AGE1jail  ! Inmate =1 or 2

              else  ! no crime

                 ctracking(id) = 0	! Record of NO criminal activity
                 intctracking(id) = 0 ! Record of NO crime intensity

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                 if (robbed(id)==1) then	! ROBBED
                    rtracking(id) = 1	! Record of being victimized
                    st_ind=st_ind+1
                 else if (robbed(id)==2) then ! NOT ROBBED
                    rtracking(id) = 0	! Record of NOT being victim.
                    st_ind=st_ind
                 endif	! End if robbed/not robbed

                 CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                 houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                 inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)

                 incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                 ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                 if (wealthi(id) < agrid(1,lage) + 0.001) then
                    bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                 endif

                 wealthi(id)= wealthi(id) + inci(id) - consi(id)

              endif AGE1CRIME	! Close Crime/no crime 'if' 

              !     Updating remaining tracking variables
              etracking(id) = LEDU
              stracking(id) = st_ind
              ftracking(id) = i_fixed(id)
              ttracking(id) = 0.0d0


           ELSE	! CURRENT STUDENT VALUE HIGHER THAN WORKER'S VALUE

              ! Save value function for this agent
201           vtracking(id) = Vfunction(ledu,lage,stcash,1,i_fixed(id),-1,crim_fixed(id))+ldrawHS(id)

              wealth = stcash ! update initial wealth to consider subs and costs of edu
              st_ind=1
              houri(id) = 0.5d0	! Used only to compute edufrac

              sw(id)=sw(id)+1 ! STUDYING

              ! Count how many STUDENTS have INITIAL wealth on the borrowing limit
              if (stcash < agrid(1,lage) + 0.001) then
                 bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
              endif

              CALL CFUNC(ledu,lage,stcash,zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))

              ! Scholarship (schship) is added to labor income WHEN paid upfront for the whole study period.
              ! In other periods it will appear as wealth.
              inci(id)   = ir(lage)*wealthi(id) + schship
              incipt(id) = ir(lage)*wealthi(id)/(1.d0-taxk) + schship
              wealthi(id)= wealthi(id) + inci(id) -&
                   & consi(id) - PVtuit*(1.d0 + ir(lage))
              etracking(id) = LEDU
              stracking(id) = st_ind
              ftracking(id) = i_fixed(id)
              ttracking(id) = transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))

              ctracking(id) = 0	! A student never commits a crime
              intctracking(id) = 0 ! Record of NO crime intensity
              atracking(id) = 0	! A student is never apprehended
              rtracking(id) = 0	! A student is never robbed

           endif IF_HS	! END OF CURRENT STUDENT VERSUS CURRENT WORKER 'IF'

           ! PROGRAM EVALUATION BOOK-KEEPING
           ! ********************* Count the TAKE-UP of the PROGRAMME ******************
           if (eligible(id)==1 .and. treated(id)==1 .and. stracking(id)==1) then
              takeup(id)=1    ! eligible,treated population who takes up programme
           else if (eligible(id)==1 .and. treated(id)==1 .and. stracking(id)>1) then 
              takeup(id)=2    !  eligible,treated population who DOES NOT takes up programme
           else if (eligible(id)==1 .and. treated(id)==0 ) then 
              takeup(id)=3    !  eligible, untreated
           else if (eligible(id)==0) then
              takeup(id)=4   ! non eligible population
           endif
           ! END OF PROGRAM EVALUATION BOOK-KEEPING
           continue


           ! NEW LAGE ********************************* 


		else if (lage>1 .and. lage<eduyr(2)+1) then ! **** AGES FROM 2 TO END-OF-HIGH-SCHOOL ****
           wtracking(id)=wealthi(id)

           ! ASSIGNMENT FOR FINAL YEAR(S) OF HIGH SCHOOL
           HScredit: if (sw(id)>0 .and. sw(id)<eduyr(2)) then
              st_ind=1
              houri(id) = 0.5d0	! Used only to compute edufrac
              sw(id)=sw(id)+1
              if (wealthi(id) < agrid(1,lage) + 0.001) then
                 bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
              endif
              CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
              inci(id)   = ir(lage)*wealthi(id)
              incipt(id) = ir(lage)*wealthi(id)/(1.d0-taxk)
              wealthi(id)= wealthi(id) + inci(id) - consi(id)
              etracking(id) = LEDU
              stracking(id) = stracking(id)
              ftracking(id) = ftracking(id)
              ttracking(id) = ttracking(id)	

              ctracking(id) = 0	! A student never commits a crime
              intctracking(id) = 0 ! Record of NO crime intensity

              atracking(id) = 0	! A student is never apprehended
              rtracking(id) = 0	! A student is never robbed

           else	! HScredit: SOLVE THE WORKERS PROBLEM FOR THOSE WHO LEFT SCHOOL EARLIER



              ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME

              ! Store values for different crime intensities (0 to maxcrim)
              do j=0,maxcrim
                 valvec(zet_id(id),j+1) = Vfunction(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),j,crim_fixed(id))
              enddo

              cloc = maxloc(valvec(zet_id(id),:),1)-1

              if (cloc==0) then    ! Assign st_ind according to crime intensity
                 st_ind=4
              else if (cloc==1) then
                 st_ind=2
              else
                 st_ind = (cloc*2)+2
              endif

              atrack0 : if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) &
                   then	! This individual was not in jail at beginning of current period

                 atracking(id)=0
        if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

                 CRIME_n2: if (cloc>0) then

                    ctracking(id) = 1	! Record of criminal activity
                    intctracking(id) = cloc	! Record of crime intensity (how many crimes he did)

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                    IFjail_n2: if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                       atracking(id) = 1	! Record of being apprehended
                       recidtracking(id) = recidtracking(id) + 1   !One more prison spell	
                       rtracking(id) = 0
                       st_ind=st_ind	
                       consi(id) = cbar
                       houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id)

                    else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                       atracking(id) = 0	! Record of being a criminal NOT apprehended

                       if (robbed(id)==1) then	! ROBBED
                          rtracking(id) = 1	! Record of being victimized
                          st_ind=st_ind+1
                       else if (robbed(id)==2) then ! NOT ROBBED
                          rtracking(id) = 0	! Record of NOT being victim.
                          st_ind=st_ind
                       endif	! End if robbed/not robbed

                       CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                       houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id) - consi(id)

                    endif IFjail_n2   ! Inmate =1 or 2

                 else  ! no crime

                    ctracking(id) = 0	! Record of NO criminal activity
                    intctracking(id) = 0

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif CRIME_n2	! Close Crime/no crime 'if' 

                 etracking(id) = LEDU
                 stracking(id) = st_ind
                 ftracking(id) = i_fixed(id)
                 ttracking(id) = 0.0d0

              else ! Individuals who have atracking(id)>0 and are in jail this period

                 atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

                 houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
                 inci(id)  = wealthi(id)*ir(lage)
                 consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
                 wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

                 etracking(id) = LEDU
                 stracking(id) = 4	! no crime, not robbed when in jail
                 ttracking(id) = 0.0d0

                 ctracking(id) = 0	! An inmate never commits a crime
                 intctracking(id) = 0
                 rtracking(id) = 0	! An inmate is never robbed	

              endif atrack0	! End if atracking>0

           endif HScredit	! END OF THE "if (sw(id)>0 .and. sw(id)<eduyr(2))" 



           ! NEW LAGE ********************************* 



		else if (lage==eduyr(2)+1) then	! AGE of FIRST COLLEGE year ONLY
           wtracking(id)=wealthi(id)

           if (ledu==2.and.sw(id)==eduyr(2)) then  
              schship=transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))
              PVtuit=tuit(ledu) *(1.d0+DISCOUNT(ir(eduyr(2)+1),eduyr(3)-1,eduyr(2)+1,2))
              ! ACTIVATE THE FOLLOWING DO IF ALL SUBSIDIES TO EDU ARE PAID UPFRONT (IN THE FIRST PERIOD)- NO MYOPIA
              do i=1,eduyr(3)-1
                 schship=schship+transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))/((1.d0+ir(lage))**i)
              enddo
           else
              schship=0.d0
           endif

           stcash=wealthi(id)+schship-PVtuit	! ******* STUDENT CASH (CONDITIONAL ON BEING A STUDENT) ******

           if (stcash < agrid(1,lage) + 0.001) then
              BCtracking(id) = 1
           endif

           ! Assign Vfunction for different crime intensities BEFORE conditioning on atracking
           do i=1,ngpz
              do j=0,maxcrim
                 valvec(i,j+1) = Vfunction(ledu,lage,wealthi(id),i,i_fixed(id),j,crim_fixed(id))
              enddo
           enddo

           cloc = maxloc(valvec(zet_id(id),:),1)-1

           if (cloc==0) then    ! Assign st_ind according to crime intensity
              st_ind=4
           else if (cloc==1) then
              st_ind=2
           else
              st_ind = (cloc*2)+2
           endif

           ! In this case we have to condition on NOT being in jail before the study/work decision
           if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) &
                then	! This individual was not in jail at beginning of current period
              atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

              IF (ledu>1 .and. sw(id)>eduyr(2)-1) then	! ONLY PEOPLE WITH HS DEGREE HAVE ACCESS TO COLLEGE
                 valstore = 0.d0
                 do i=1,ngpz 
                    valstore = valstore+zstat(i,ledu)*maxval(valvec(i,:))
                 enddo
           
                 vdiftracking(id) = valstore-(Vfunction(ledu,lage,stcash,1,i_fixed(id),-1,crim_fixed(id))+ldrawC(id)) 

                 ! COMPARE EXPECTED VALUE OF CURRENTLY WORKING TO CURRENTLY STUDYING
                 IF (vdiftracking(id) > 0.0d0) then

                    sw(id) = sw(id)


                    ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME

                    if (cloc>0) then

                       ctracking(id) = 1	! Record of criminal activity
                       intctracking(id) = cloc	! Record of crime intensity (how many crimes he did)

                       !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                       if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                          atracking(id) = 1	! Record of being apprehended
                          recidtracking(id) = recidtracking(id) + 1   !One more prison spell
                          rtracking(id) = 0
                          st_ind=st_ind	
                          consi(id) = cbar
                          houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                          inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                          ! incipt = inci gross (before taxes) - income from committing crime
                          incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                          ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                          if (wealthi(id) < agrid(1,lage) + 0.001) then
                             bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                          endif

                          wealthi(id)= wealthi(id) + inci(id)


                       else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL 
                          atracking(id) = 0	! Record of being a criminal NOT apprehended

                          if (robbed(id)==1) then	! ROBBED
                             rtracking(id) = 1	! Record of being victimized
                             st_ind=st_ind+1
                          else if (robbed(id)==2) then ! NOT ROBBED
                             rtracking(id) = 0	! Record of NOT being victim.
                             st_ind=st_ind
                          endif	! End if robbed/not robbed

                          CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                          houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                          inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                          ! incipt = inci gross (before taxes) - income from committing crime
                          incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                          ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                          if (wealthi(id) < agrid(1,lage) + 0.001) then
                             bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                          endif

                          wealthi(id)= wealthi(id) + inci(id) - consi(id)

                       endif   ! Inmate =1 or 2

                    else  ! no crime

                       ctracking(id) = 0	! Record of NO criminal activity
                       intctracking(id) = 0

                       !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                       if (robbed(id)==1) then	! ROBBED
                          rtracking(id) = 1	! Record of being victimized
                          st_ind=st_ind+1
                       else if (robbed(id)==2) then ! NOT ROBBED
                          rtracking(id) = 0	! Record of NOT being victim.
                          st_ind=st_ind
                       endif	! End if robbed/not robbed

                       CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                       houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id) - consi(id)

                    endif	! Close Crime/no crime 'if' 

                    etracking(id) = LEDU
                    stracking(id) = st_ind
                    ftracking(id) = i_fixed(id)
                    ttracking(id) = 0.0d0

                 else	! Current student value is larger than current expected worker value

                    wealth = stcash ! update initial wealth to consider subs and costs of edu
                    st_ind=1
                    houri(id) = 0.5d0	! Used only to compute edufrac
                    sw(id)=sw(id)+1

                    if (stcash < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    CALL CFUNC(ledu,lage,stcash,zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))


                    ! Scholarship (schship) is added to labor income when it paid upfront for the whole study period.
                    ! In other periods it will appear as wealth.
                    inci(id)   = ir(lage)*wealthi(id) + schship
                    incipt(id) = ir(lage)*wealthi(id)/(1.d0-taxk) + schship
                    wealthi(id)= wealthi(id) + inci(id) - consi(id) - PVtuit*(1.d0 + ir(lage))
                    etracking(id) = LEDU
                    stracking(id) = st_ind
                    ftracking(id) = i_fixed(id)
                    ttracking(id) = transf(ledu,lage,wealthi(id),medwealth,rndtuit(id),tuit(ledu))

                    ctracking(id) = 0	! A student never commits a crime
                    intctracking(id) = 0
                    atracking(id) = 0	! A student is never apprehended
                    rtracking(id) = 0	! A student is never robbed		

                 endif	! END OF CURRENT STUDENT VERSUS CURRENT WORKER 'IF'

              ELSE IF (ledu<2.and.sw(id)<eduyr(2)) then	! PEOPLE WHO ARE HS DROPOUTS - THEY ONLY HAVE THE OPTION TO WORK

                 sw(id) = sw(id)

                 ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME

                 if (cloc>0) then

                    ctracking(id) = 1	! Record of criminal activity
                    intctracking(id) = cloc

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                    if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                       atracking(id) = 1	! Record of being apprehended
                       recidtracking(id) = recidtracking(id) + 1   !One more prison spell
                       rtracking(id) = 0
                       st_ind=st_ind	
                       consi(id) = cbar
                       houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id)


                    else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                       atracking(id) = 0	! Record of being a criminal NOT apprehended

                       if (robbed(id)==1) then	! ROBBED
                          rtracking(id) = 1	! Record of being victimized
                          st_ind=st_ind+1
                       else if (robbed(id)==2) then ! NOT ROBBED
                          rtracking(id) = 0	! Record of NOT being victim.
                          st_ind=st_ind
                       endif	! End if robbed/not robbed

                       CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                       houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id) - consi(id)

                    endif   ! Inmate =1 or 2

                 else  ! no crime

                    ctracking(id) = 0	! Record of NO criminal activity
                    intctracking(id) = 0

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif	! Close Crime/no crime 'if' 

                 etracking(id) = LEDU
                 stracking(id) = st_ind
                 ftracking(id) = i_fixed(id)
                 ttracking(id) = 0.0d0

              ENDIF	! END OF school credits if:  "IF HS GRADUATE OR HIGH SCHOOL DROPOUT"


           else ! Individuals IN jail at beginning of current period
              atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

              houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
              inci(id)  = wealthi(id)*ir(lage)
              consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
              wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

              etracking(id) = LEDU
              stracking(id) = 4	! no crime, not robbed when in jail
              ttracking(id) = 0.0d0

              ctracking(id) = 0	! An inmate never commits a crime
              intctracking(id) = 0
              rtracking(id) = 0	! An inmate is never robbed	

           endif	! End if: : atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT



           ! NEW LAGE ********************************* 



		else if (lage > eduyr(2)+1 .and. lage<sum(eduyr)+1) then ! ages from after 1st College year up to last college age
           wtracking(id)=wealthi(id)


           ! ASSIGNMENT FOR FINAL YEAR(S) OF COLLEGE
           if (sw(id)>eduyr(2) .and. sw(id)<SUM(eduyr)) then
              st_ind=1
              houri(id) = 0.5d0	! Used only to compute edufrac
              sw(id)=sw(id)+1
              if (wealthi(id) < agrid(1,lage) + 0.001) then
                 bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
              endif
              CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
              inci(id)   = ir(lage)*wealthi(id)
              incipt(id) = ir(lage)*wealthi(id)/(1.d0-taxk)
              wealthi(id)= wealthi(id) + inci(id) - consi(id)
              etracking(id) = LEDU
              stracking(id) = stracking(id)
              ftracking(id) = ftracking(id)
              ttracking(id) = ttracking(id)	

              ctracking(id) = 0	! A student never commits a crime
              intctracking(id) = 0
              atracking(id) = 0	! A student is never apprehended
              rtracking(id) = 0	! A student is never robbed

           else	! SOLVE THE WORKERS PROBLEM FOR THOSE WHO LEFT SCHOOL EARLIER


              ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME
              do j=0,maxcrim
                 valvec(zet_id(id),j+1) = Vfunction(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),j,crim_fixed(id))
              enddo

              cloc = maxloc(valvec(zet_id(id),:),1)-1

              if (cloc==0) then    ! Assign st_ind according to crime intensity
                 st_ind=4
              else if (cloc==1) then
                 st_ind=2
              else
                 st_ind = (cloc*2)+2
              endif

              if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) &
                   then	! This individual was not in jail in the current period

                 atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

                 if (cloc>0) then

                    ctracking(id) = 1	! Record of criminal activity
                    intctracking(id) = cloc


                    if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                       atracking(id) = 1	! Record of being apprehended
                       recidtracking(id) = recidtracking(id) + 1   !One more prison spell    
                       rtracking(id) = 0
                       st_ind=st_ind	
                       consi(id) = cbar
                       houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id)


                    else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                       atracking(id) = 0	! Record of being a criminal NOT apprehended

                       if (robbed(id)==1) then	! ROBBED
                          rtracking(id) = 1	! Record of being victimized
                          st_ind=st_ind+1
                       else if (robbed(id)==2) then ! NOT ROBBED
                          rtracking(id) = 0	! Record of NOT being victim.
                          st_ind=st_ind
                       endif	! End if robbed/not robbed

                       CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                       houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id) - consi(id)

                    endif   ! Inmate =1 or 2

                 else  ! no crime

                    ctracking(id) = 0	! Record of NO criminal activity
                    intctracking(id) = 0

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif	! Close Crime/no crime 'if' 

                 etracking(id) = LEDU
                 stracking(id) = st_ind
                 ftracking(id) = i_fixed(id)
                 ttracking(id) = 0.0d0

              else    ! Individuals IN jail at beginning of current period
                 atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

                 houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
                 inci(id)  = wealthi(id)*ir(lage)
                 consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
                 wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

                 etracking(id) = LEDU
                 stracking(id) = 4	! no crime, not robbed when in jail
                 ttracking(id) = 0.0d0
                 ctracking(id) = 0	! An inmate never commits a crime
                 intctracking(id) = 0
                 rtracking(id) = 0	! An inmate is never robbed	


              endif	! End if atracking>0

           endif	! END OF THE "if (sw(id)>0 .and. sw(id)<eduyr(2))" 



           ! NEW LAGE ********************************* 



		else if (lage > sum(eduyr) .and. lage<min(retage-1,lifet)) then 
           wtracking(id)=wealthi(id)
           sw(id) = sw(id)

           do j=0,maxcrim
              valvec(zet_id(id),j+1) = Vfunction(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),j,crim_fixed(id))
           enddo

           cloc = maxloc(valvec(zet_id(id),:),1)-1

           if (cloc==0) then    ! Assign st_ind according to crime intensity
              st_ind=4
           else if (cloc==1) then
              st_ind=2
           else
              st_ind = (cloc*2)+2
           endif

           if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) & 
                then	! This individual was not in jail in the current period


              atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

              ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME
              if (cloc>0) then

                 ctracking(id) = 1	! Record of criminal activity
                 intctracking(id) = cloc

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                 if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
		            atracking(id) = 1	! Record of being apprehended
                    recidtracking(id) = recidtracking(id) + 1   !One more prison spell
	                rtracking(id) = 0
                    st_ind=st_ind	
                    consi(id) = cbar
                    houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
            		! incipt = inci gross (before taxes) - income from committing crime
		            incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

              ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

		            wealthi(id)= wealthi(id) + inci(id)

                 else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                    atracking(id) = 0	! Record of being a criminal NOT apprehended

                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif   ! Inmate =1 or 2

              else  ! no crime

                 ctracking(id) = 0	! Record of NO criminal activity
                 intctracking(id) = 0

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                 if (robbed(id)==1) then	! ROBBED
                    rtracking(id) = 1	! Record of being victimized
                    st_ind=st_ind+1
                 else if (robbed(id)==2) then ! NOT ROBBED
                    rtracking(id) = 0	! Record of NOT being victim.
                    st_ind=st_ind
                 endif	! End if robbed/not robbed

                 CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                 houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                 inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                 ! incipt = inci gross (before taxes) - income from committing crime
                 incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                 ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                 if (wealthi(id) < agrid(1,lage) + 0.001) then
                    bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                 endif

                 wealthi(id)= wealthi(id) + inci(id) - consi(id)

              endif	! Close Crime/no crime 'if' 

              etracking(id) = LEDU
              stracking(id) = st_ind
              ftracking(id) = i_fixed(id)
              ttracking(id) = 0.0d0

           else        ! This individual WAS IN jail at beginning of current period
              atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

              houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
              inci(id)  = wealthi(id)*ir(lage)
              consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
              wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

              etracking(id) = LEDU
              stracking(id) = 4	! no crime, not robbed when in jail
              ttracking(id) = 0.0d0

              ctracking(id) = 0	! An inmate never commits a crime
              intctracking(id) = 0
              rtracking(id) = 0	! An inmate is never robbed	

           endif	! End if atracking>0


        else if (lage==retage-1 .and. lage<lifet) then    ! Last period of WORKING life    
           wtracking(id)=wealthi(id)
           sw(id) = sw(id)

           do j=0,maxcrim
              valvec(zet_id(id),j+1) = Vfunction(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),j,crim_fixed(id))
           enddo

           cloc = maxloc(valvec(zet_id(id),:),1)-1

           if (cloc==0) then    ! Assign st_ind according to crime intensity
              st_ind=4
           else if (cloc==1) then
              st_ind=2
           else
              st_ind = (cloc*2)+2
           endif

           if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) &
                then  !This individual was not in jail in the current period

              atracking(id)=0
        if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

              ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME
              if (cloc>0) then

                 ctracking(id) = 1	! Record of criminal activity
                 intctracking(id) = cloc

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                 if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                    atracking(id) = 1	! Record of being apprehended
                    recidtracking(id) = recidtracking(id) + 1   !One more prison spell
                    rtracking(id) = 0
                    st_ind=st_ind	
                    consi(id) = cbar
                    houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
            		! incipt = inci gross (before taxes) - income from committing crime
		            incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

              ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

		            wealthi(id)= wealthi(id) + inci(id)

                 else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                    atracking(id) = 0	! Record of being a criminal NOT apprehended

                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif   ! Inmate =1 or 2

              else  ! no crime

                 ctracking(id) = 0	! Record of NO criminal activity
                 intctracking(id) = 0

                 !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                 if (robbed(id)==1) then	! ROBBED
                    rtracking(id) = 1	! Record of being victimized
                    st_ind=st_ind+1
                 else if (robbed(id)==2) then ! NOT ROBBED
                    rtracking(id) = 0	! Record of NOT being victim.
                    st_ind=st_ind
                 endif	! End if robbed/not robbed

                 CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                 houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                 inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                 ! incipt = inci gross (before taxes) - income from committing crime
                 incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                 ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                 if (wealthi(id) < agrid(1,lage) + 0.001) then
                    bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                 endif

                 wealthi(id)= wealthi(id) + inci(id) - consi(id)

              endif	! Close Crime/no crime 'if' 

              etracking(id) = LEDU
              stracking(id) = st_ind
              ftracking(id) = i_fixed(id)
              ttracking(id) = 0.0d0

           else 	! This individual WAS IN jail at beginning of current period
              atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

              houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
              inci(id)  = wealthi(id)*ir(lage)
              consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
              wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

              etracking(id) = LEDU
              stracking(id) = 4	! no crime, not robbed when in jail
              ttracking(id) = 0.0d0

              ctracking(id) = 0	! An inmate never commits a crime
              intctracking(id) = 0
              rtracking(id) = 0	! An inmate is never robbed	

           endif	! End if atracking>0




           ! NEW LAGE ********************************* 



		else if (lage > retage-1 .and. lage<lifet) then   

           wtracking(id)=wealthi(id)
           sw(id) = sw(id)

           if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) &
                then	! This individual was not in jail at start of current period
              atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

              ctracking(id) = 0	! Record of NO criminal activity when in retirement
              intctracking(id) = 0
              rtracking(id) = 0	! Record of NOT being victimized because NO LABOR INCOME
              st_ind=4

              IF (lage==51) THEN 
                 CONTINUE
              END IF

              CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))

              if (wealthi(id) < agrid(1,lage) + 0.001) then
                 bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
              endif

              houri(id) = hour(lage,zet_id(id),noljail(id),ledu) ! Keep positive hours for constructing some statistics
              ! But be careful not to give labor income !

              inci(id)=penv(lage)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
              ! incipt = inci gross (before taxes) - income from committing crime
              incipt(id)=penv(lage)+ir(lage)*wealthi(id)/(1.d0-taxk)

              wealthi(id)= wealthi(id) + inci(id) - consi(id)

              etracking(id) = LEDU
              stracking(id) = st_ind
              ftracking(id) = i_fixed(id)
              ttracking(id) = 0.0d0

           else 	! This individual WAS in jail at start of current period
              st_ind=4
              atracking(id) = atracking(id) + 1

              houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
              inci(id)  = ir(lage)*wealthi(id)+penv(lage)
              consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
              wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

              etracking(id) = LEDU
              stracking(id) = st_ind	! Not crime, not robbed when in jail
              ttracking(id) = 0.0d0

              ctracking(id) = 0	! An inmate never commits a crime
              intctracking(id) = 0	
              rtracking(id) = 0	! An inmate is never robbed	

           endif	! atracking==0			



           ! NEW LAGE ********************************* 



		else	! ***** This is lage=lifet
           wtracking(id)=wealthi(id)

           sw(id) = sw(id)

           if (lage<retage) then	! Case with NO RETIREMENT in the model

              do j=0,maxcrim
                 valvec(zet_id(id),j+1) = Vfunction(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),j,crim_fixed(id))
              enddo

              cloc = maxloc(valvec(zet_id(id),:),1)-1

              if (cloc==0) then    ! Assign st_ind according to crime intensity
                 st_ind=4
              else if (cloc==1) then
                 st_ind=2
              else
                 st_ind = (cloc*2)+2
              endif

              if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) &
                   then	! This individual was not in jail at start of current period
    
                 atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

                 ! FOR WORKERS ONLY: COMPARE VALUE OF CRIME TO VALUE OF NOT CRIME
                 if (cloc>0) then

                    ctracking(id) = 1	! Record of criminal activity
                    intctracking(id) = cloc

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED

                    if (inmate(id,cloc)==1) then	! APPREHENDED CRIMINAL
                       atracking(id) = 1	! Record of being apprehended
                       recidtracking(id) = recidtracking(id) + 1   !One more prison spell
                       rtracking(id) = 0
                       st_ind=st_ind	
                       consi(id) = cbar
                       houri(id) = hour(lage,zet_id(id),ljail(id),ledu)   ! i_zet irrelevant. Hours zero when jail=2
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id)

                    else if (inmate(id,cloc)==2)	then	! NOT APPREHENDED CRIMINAL
                       atracking(id) = 0	! Record of being a criminal NOT apprehended

                       if (robbed(id)==1) then	! ROBBED
                          rtracking(id) = 1	! Record of being victimized
                          st_ind=st_ind+1
                       else if (robbed(id)==2) then ! NOT ROBBED
                          rtracking(id) = 0	! Record of NOT being victim.
                          st_ind=st_ind
                       endif	! End if robbed/not robbed

                       CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                       houri(id) = hour(lage,zet_id(id),noljail(id),ledu)
                       inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                       ! incipt = inci gross (before taxes) - income from committing crime
                       incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                       ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                       if (wealthi(id) < agrid(1,lage) + 0.001) then
                          bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                       endif

                       wealthi(id)= wealthi(id) + inci(id) - consi(id)

                    endif   ! Inmate =1 or 2

                 else  ! no crime

                    ctracking(id) = 0	! Record of NO criminal activity
                    intctracking(id) = 0

                    !  ASSIGN ST_IND DEPENDING ON WHETHER ROBBED OR NOT ROBBED
                    if (robbed(id)==1) then	! ROBBED
                       rtracking(id) = 1	! Record of being victimized
                       st_ind=st_ind+1
                    else if (robbed(id)==2) then ! NOT ROBBED
                       rtracking(id) = 0	! Record of NOT being victim.
                       st_ind=st_ind
                    endif	! End if robbed/not robbed

                    CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))
                    houri(id) = hour(lage,zet_id(id),noljail(id),ledu) 
                    inci(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                    ! incipt = inci gross (before taxes) - income from committing crime
                    incipt(id)=rescale(st_ind)*wgi(ledu,id)*houri(id)/(1.d0-taxn)+ir(lage)*wealthi(id)/(1.d0-taxk)

                    ! Count how many WORKERS have INITIAL wealth equal to the borrowing limit
                    if (wealthi(id) < agrid(1,lage) + 0.001) then
                       bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                    endif

                    wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 endif	! Close Crime/no crime 'if' 

                 etracking(id) = LEDU
                 stracking(id) = st_ind
                 ftracking(id) = i_fixed(id)
                 ttracking(id) = 0.0d0

              else    ! This individual WAS IN jail at beginning of current period
                 atracking(id) = atracking(id)+1 ! Increments atracking counter for the new period

                 houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	! Hours supplied while in jail (set to zero)
                 inci(id)  = wealthi(id)*ir(lage)
                 consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
                 wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

                 etracking(id) = LEDU
                 stracking(id) = 4	! no crime, not robbed when in jail
                 ttracking(id) = 0.0d0

                 ctracking(id) = 0	! An inmate never commits a crime
                 intctracking(id) = 0
                 rtracking(id) = 0	! An inmate is never robbed	

              endif	! End if atracking>0

           else if (lage > retage-1) then	! Case with RETIREMENT in the model

              st_ind=4

              if (atracking(id)==0 .or. (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT ) &
                   then	! This individual was not in jail at start of current period
	
                 atracking(id)=0
    if ( (atracking(id)==prisT .and. res_id(id)==2) .or. atracking(id)>prisT) zet_id(id)=(ngpz+1)/2

                 ctracking(id) = 0	! Record of NO criminal activity when in retirement
                 intctracking(id) = 0
                 rtracking(id) = 0	! Record of NOT being victimized because NO LABOR INCOME


                 CALL CFUNC(ledu,lage,wealthi(id),zet_id(id),i_fixed(id),st_ind,crim_fixed(id),consi(id))

                 if (wealthi(id) < agrid(1,lage) + 0.001) then
                    bcpeople(st_ind,lage,ledu) = bcpeople(st_ind,lage,ledu) + 1.d0
                 endif

                 houri(id) = hour(lage,zet_id(id),noljail(id),ledu) ! Keep positive hours for constructing some statistics
                 ! But be careful not to give labor income !

                 inci(id)=penv(lage)+shrob(st_ind)*UNCavgearn+ir(lage)*wealthi(id)
                 ! incipt = inci gross (before taxes) - income from committing crime
                 incipt(id)=penv(lage)+ir(lage)*wealthi(id)/(1.d0-taxk)

                 wealthi(id)= wealthi(id) + inci(id) - consi(id)

                 etracking(id) = LEDU
                 stracking(id) = st_ind
                 ftracking(id) = i_fixed(id)
                 ttracking(id) = 0.0d0

              else ! This individual WAS in jail at start of current period
                 atracking(id) = atracking(id) + 1

                 houri(id) = hour(lage,zet_id(id),ljail(id),ledu)	
                 inci(id)  = ir(lage)*wealthi(id)+penv(lage)
                 consi(id) = cbar	! This agent is going to stay in prison at least one more period. hence consi=cbar
                 wealthi(id)= wealthi(id)+inci(id)	! Wealth grows in the bank, while in prison

                 etracking(id) = LEDU
                 stracking(id) = st_ind	! Not crime, not robbed when in jail
                 ttracking(id) = 0.0d0

                 ctracking(id) = 0	! An inmate never commits a crime
                 intctracking(id) = 0
                 rtracking(id) = 0	! An inmate is never robbed	

              endif	! if atracking>0

           endif	! End if "model with retirement or model with NO retirment" for period lage=lifet

		endif OUTER_LAGE	!****** end of if over lage
  ! **************************************************************************************** 


  ! ******* CHECK THAT WEALTH DOES NOT EXCEED BORROWING LIMIT and adjust if necessary ******* 
        if (lage<lifet .and. lage.ne.beqage) then
           if (wealthi(id) < agrid(1,lage+1)) then
              if (atracking(id)==0) then  ! adjust consumption, when possible
                 consi(id) = max(0.d0,consi(id) - dabs(agrid(1,lage+1)-wealthi(id)))
              endif
              wealthi(id)= agrid(1,lage+1)    ! set wealth at lower bound
           endif
        endif !	endif	! this refers to if (lage<lifet) 


        ! WEALTH LESS THAN CRTIW CONSTRAINT - COUNT HOW MANY AGENTS !
        IF (wealthi(id) <= critw) THEN
           qwzero = qwzero + 1 !Zero or negative wealth
        ENDIF


     ENDDO   ! *********************** ENDDO GENSIZE ********************************************

     ! Store inter-vivos transfer
     if (lage == beqage) then
		WHERE (atracking==0)
     !Golden glow bequest
           inter_vivos = MAX(0.d0,gamma_1**(1.d0/eta)*consi**(sigma/eta)-gamma_2)
           wealthi = wealthi - inter_vivos
           !Add bequests a la Storesletten to obtain total bequest
           inter_vivos = inter_vivos + (1.d0-psi)*consi   
        ELSEWHERE
           inter_vivos = 0.d0
    	END WHERE

     ! Check that borrowing limit is respected after bequests 
        do i=1,gensize 
           if (wealthi(i).lt.agrid(1,lage+1) .and. atracking(i)==0) then
              const_beq=wealthi(i)+inter_vivos(i)+consi(i)-agrid(1,lage+1)
              x_guess = consi(i)
              call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                   xguess=x_guess)
              consi(i)=x_temp(1)
              inter_vivos(i) = MAX(0.d0,gamma_1**(1.d0/eta)*consi(i)**(sigma/eta)-gamma_2)
              wealthi(i)= agrid(1,lage+1)
           endif
        ENDDO
        ind= (/ (i,i=1,gensize) /)
        ! Sort inter vivos
        CALL QsortC(inci,ind)
        inter_vivos_sort = inter_vivos(ind)
                
        CALL DISTSTAT(gensize,1,inter_vivos,ygini, pw15, yrwza, ywpnw, yavwealth, ymedwealth, yvarwealth) 
        PRINT *, "***************************************************************"
        PRINT '(A50,F14.4)'," 15th wealth percentile of youngest       ", pw15
        PRINT '(A50,F14.4)'," % of youngest age group with wealth <=0  ", yrwza
        PRINT '(A50,F14.4)'," Avg wealth of youngest with wealth <=0   ", ywpnw
        PRINT '(A50,F14.4)'," Average assets in youngest age group     ", yavwealth
        PRINT '(A50,F14.4)'," Assets SD in youngest age group          ", (yvarwealth)**.5d0
        PRINT '(A50,F14.4)'," Median  assets in youngest age group     ", ymedwealth
        PRINT '(A50,F14.4)'," Min asset level in youngest age group    ", minval(inter_vivos)
        PRINT '(A50,F14.4)'," Max asset level in youngest age group    ", maxval(inter_vivos)
        PRINT '(A50,F14.4)'," % of tot. wealth held by youngest        ", (SUM(inter_vivos)/REAL(gensize,long))/(avwealth*SUM(popsize))
        PRINT '(A50,F14.4)'," Share of wealth age 18-24                ", moms(3)
        PRINT '(A50,F14.4)'," Average dropout earnings at age 16       ", avgearn_drop16/ind_drop16 
        PRINT *, '	'
                !	namedata='youngwealth.txt' ; namecommand='youngwealthcom.txt'
        !	CALL histograms (gensize,150,wdinc,namedata,namecommand)

        OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND') 
        WRITE(38,*),' '
        WRITE(38,*),'***********************************'
        WRITE (38,'(A40,F14.5)'), " 15th wealth percentile of youngest       ", pw15 
        WRITE (38,'(A40,F14.5)'), " % of youngest age group with wealth <=0  ", yrwza
        WRITE (38,'(A40,F14.5)'), " Average assets in youngest age group     ", SUM(inter_vivos)/REAL(gensize,long)
        WRITE (38,'(A40,F14.5)'), " Assets SD in youngest age group          ", (SUM(inter_vivos**2.d0)/REAL(gensize,long)-(SUM(inter_vivos)/REAL(gensize,long))**2.d0)**.5d0
        WRITE (38,'(A40,F14.5)'), " Median  assets in youngest age group     ", ymedwealth
        WRITE (38,'(A40,F14.5)'), " Min asset level in youngest age group    ", minval(inter_vivos)
        WRITE (38,'(A40,F14.5)'), " Max asset level in youngest age group    ", maxval(inter_vivos)
        WRITE (38,'(A40,F14.5)'), " % of tot. wealth held by youngest        ", (SUM(inter_vivos)/REAL(gensize,long))/(avwealth*SUM(popsize))
        WRITE (38,'(A40,F14.5)'), " Share of wealth age 18-24                ", moms(3)
        WRITE (38,'(A40,F14.5)'), " Average dropout earnings at age 16       ", avgearn_drop16/ind_drop16 
        CLOSE(unit=38)
     END IF  ! lage==beqage

    IF (lage==40) then
        CALL DISTSTAT(gensize,1,wealthi,ygini, pw15, yrwza, ywpnw, yavwealth, ymedwealth, yvarwealth) 
        OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"wealth-40.txt",action='write',position='rewind') 
        WRITE(38,*),' '
        WRITE(38,*),'***********************************'
        WRITE (38,'(A40,F14.5)'), " 15th wealth percentile          ", pw15 
        WRITE (38,'(A40,F14.5)'), " % of age group with wealth <=0  ", yrwza
        WRITE (38,'(A40,F14.5)'), " Average assets      ", SUM(wealthi)/REAL(gensize,long)
        WRITE (38,'(A40,F14.5)'), " Assets SD           ", (SUM(wealthi**2.d0)/REAL(gensize,long)-(SUM(wealthi)/REAL(gensize,long))**2.d0)**.5d0
        WRITE (38,'(A40,F14.5)'), " Median  assets      ", ymedwealth
        WRITE (38,'(A40,F14.5)'), " Min asset level     ", minval(wealthi)
        WRITE (38,'(A40,F14.5)'), " Max asset level     ", maxval(wealthi)
        WRITE (38,'(A40,F14.5)'), " % of tot. wealth    ", (SUM(wealthi)/REAL(gensize,long))/(avwealth*SUM(popsize))
        CLOSE(unit=38)
    ENDIF ! age == 40

    if (calibselect==1) then          
     ! In calibration mode, save sample of agents aged 18-23 and 
     ! "indirect inference" calibration regression

     if (lage==3) then
        jr=0
        sample_track=0
        OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"lochner.txt",action='write',position='REWIND')  
        WRITE (38,'(9A10)'), 'ctracking', 'etracking', 'i_fixed', &
             'inci', 'wealth','crimfix',&
             'stracking','id', 'age'
        close (unit=38)
     endif

     if (lage>2 .and. lage<9) then 		
        ! Sample randomly to avoid time dependence 
        ! (NLSY79 1980 wave is a cross-section not a panel)
		call drnun(gensize,rsamp) 
		do i=1,gensize
           if (rsamp(i)<.1d0/(1.d0-.1d0*(lage-3)) .and. atracking(i)==0 &
                .and. sample_track(i)==0) then 
              sample_track(i)=1
              jr=jr+1
              OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"lochner.txt",action='write',position='APPEND')  
              WRITE (38,'(9F10.3)'), real(ctracking(i)), real(etracking(i)), i_fixed(i), &
                   wgi(etracking(i),i)*houri(i), wtracking(i), crim_fixed(i), &
                   real(stracking(i)),real(i), real(lage)
              close (unit=38)
           endif
		enddo
     endif
     
    
     if (lage==9) then 
    
         ! Read samples of variables into vectors to run regression within the Fortran code
         allocate(ctrackreg(jr),strackreg(jr),earnreg(jr),wreg(jr),idreg(jr),abirank(jr),matreg(jr,5),coefreg(6)) ! allocate vectors for reg
         OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"lochner.txt",action='read',position='REWIND')  
         READ (38,*)
         do i=1,jr
            READ (38,'(9F10.3)'), ctrackreg(i), matreg(i,1), matreg(i,2), &
            earnreg(i), wreg(i), matreg(i,3),strackreg(i),idreg(i),matreg(i,4)
            ! generate dummy for education = HS or higher
            if (matreg(i,1)==1) then
            matreg(i,1)=0
            else
            matreg(i,1)=1
            endif
            ! generate dummy for student vis-a-vis worker (1=student,0=worker)
            if (strackreg(i)>1) strackreg(i)=0
         end do ! i=1,jr
         close (unit=38)
          
         call ranks(matreg(:,2),abirank(:)) ! generate ranking of ability (similar to percentiles 1-99 in Stata)
         matreg(:,2)=real(INT(99*abirank(:)/jr),long)
         matreg(:,4)= matreg(:,4)+15  ! rescale age to make it comparable to data 
         matreg(:,5)= strackreg 
         

          regpos(1)=1; regpos(2)=2; regpos(3)=4; regpos(4)=5
         s_index = count(strackreg(:)<1.d0)
         j=0
         do i=1,jr
         if (strackreg(i) .ne. 1.d0) then
            j=j+1
            matreg(j,:) = matreg(i,:)
            ctrackreg(j) = ctrackreg(i)
         endif ! strackreg>1
         end do
    
        ! Run calibration regression
         call RLSE(ctrackreg(1:s_index),matreg(1:s_index,regpos(1:3)),coefreg(1:4))
         OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"calibration_regression.txt",action='write',position='APPEND')  
              WRITE (38,*), "Calibration regression"
              do i=1,4
              WRITE (38,*), coefreg(i)
              enddo ! i
         WRITE (38,*), "                "
         close (unit=38)

         deallocate(ctrackreg,strackreg,earnreg,wreg,idreg,abirank,matreg)
     endif 
    endif

        
!***********************************************************************************

     ! Save sample of agents aged 18-65 to run robustness regressions

     if (lage==3) then
        OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"QOP.txt",action='write',position='REWIND')  
        WRITE (38,'(15A10)'), 'age','ctracking', 'intctracking', 'eligible', 'treated', 'forcedtr' , 'takeup', 'stracking', &
             'atracking', 'etracking', 'id', 'i_fixed', 'crim_fixed', 'wtracking','wageinc'
        close (unit=38)
        do i=1,gensize
           OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"QOP.txt",action='write',position='APPEND')  
           WRITE (38,'(11I10,4F10.3)'), lage,ctracking(i), intctracking(i), eligible(i), treated(i), forcedtrack(i), takeup(i), stracking(i),&
                atracking(i), etracking(i), i, i_fixed(i), crim_fixed(i), wtracking(i),wgi(etracking(i),i)*houri(i)
           close (unit=38)
        enddo
     else if (lage>3 .and. lage<51) then
           do i=1,gensize
           OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"QOP.txt",action='write',position='APPEND')  
           WRITE (38,'(11I10,4F10.3)'), lage, ctracking(i), intctracking(i), eligible(i), treated(i), forcedtrack(i), takeup(i), stracking(i),&
                atracking(i), etracking(i), i, i_fixed(i), crim_fixed(i), wtracking(i),wgi(etracking(i),i)*houri(i)
           close (unit=38)
           enddo
     endif

!************************************************************************************

     ! CONSTRUCT AGGREGATE STATISTICS BY AGE	!
     ! Update aggregate wealth                                  
     ! Select income obs. for workers, flag NON-workers using extremely low initialized value
     i2=1
     i3=1
     do i1=1,gensize
        if (stracking(i1)>1) then
           incdistss(i2,lage) = inci(i1)		! Save the first ss obs. on after-tax income for age lage
           if (lage<retage) then
              wgidistss(i3,lage) = wgi(etracking(i1),i1)
              i3 = i3+1
           endif
           i2=i2+1
        endif
		if (i2==ss+1) then
           exit
		endif
     enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! Aggwealth , Wealthdistss and wealthzero !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     if (LAGE<LIFET) then
        aggwealth = aggwealth + (SUM(wealthi)/gensize)*popsize(lage+1)	! aggregate wealth
        if (lage==18) avwealth35=aggwealth/sum(popsize(1:19))   ! Average wealth of people<36
        wealthdistss(:,lage+1) = wealthi(1:ss)	!Wealth holdings of first ss agents aged lage+1
        !Add to fraction of total population with <= critw (recall qwzero applies to lage+1)
	    if (lage==1) then
           where (initialw <= critw) 
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           wealthzero = wealthzero+(sum(temp)/real(gensize))*popsize(lage)/SUM(popsize(1:lifet))
           wealthzero = wealthzero+(real(qwzero)/real(gensize))*popsize(lage+1)/SUM(popsize(1:lifet))
        else 
           wealthzero = wealthzero+(real(qwzero)/real(gensize))*popsize(lage+1)/SUM(popsize(1:lifet))
	    endif
     else    ! LAGE=lifet
        wealthi_agemax=wealthi
     endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!! COMPUTATION OF AGE-DEPENDENT STATS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



     ! STATUSHARE - SHARE OF POPULATION IN DIFFERENT STATUS=STUDENT/NON STUDENT  BY AGE
     ! Non student includes inmates
     do I=1,ngpst
        where (stracking == I)
           temp = 1.d0
        elsewhere
           temp	= 0.d0
        endwhere
        statushare(lage,I) = sum (temp)/real(gensize,long)
     end do

     !***	AVERAGES AND AGGREGATES BY EDU/STATUS	***
     do J=1,NEDU

        do i=1,maxcrim
           where (intctracking==i .and. etracking==J)
              temp = 1.d0
           elsewhere
              temp = 0.d0	
           endwhere
           int_cstat(J,i) = int_cstat(J,i)+sum(temp)*popsize(lage)/real(gensize,long)
		enddo

        if (lage==1 .and. J==1) then 
        intensity16 = 0.d0 
        do i = 1,maxcrim
            intensity16 = intensity16 + int_cstat(J,i)*real(i,long)/sum(int_cstat(J,:))
        enddo
        
        endif
        
        do I=1,ngpst

           ! EDUFRAC - SHARES by (EDU,STATUS) OF EACH AGE GROUP EXCLUDING INMATES (NON-OPTIMIZERS) 
           where (etracking == J .and. stracking == I .and. &
                atracking==0)
              temp	= 1.d0
           elsewhere
              temp	= 0.d0
           endwhere
           EDUFRAC(J,lage,I) = sum (temp) / real(gensize,long)

           ! EDUFRACTOT - SHARES by (EDU,STATUS) OF EACH AGE GROUP INCLUDING INMATES OVER TOTAL POPULATION
           where (etracking == J .and. stracking == I)
              temp	= 1.d0
           elsewhere
              temp	= 0.d0
           endwhere
           EDUFRACTOT(J,lage,I) = sum (temp) / real(gensize,long)

           ! EAGGINC - Compute aggregate PRETAX income by (edu,status)
           where (etracking==J .and. stracking==I)
              temp = incipt
           elsewhere
              temp = 0.d0
           end where
           EAGGINC(J,I) = EAGGINC(J,I) + (SUM(temp)/ real(gensize,long)) * popsize(lage)
           ! NOTICE: average inc * share in group * popsize = sum incomes * popsize / gensize

           ! GAGGEFFS - AGGREGATE EFFICIENCY UNITS by (EDU,STATUS) group (HOURI(ID)=0 FOR INMATES)
           where (etracking==J .and. stracking==I)
              temp = houri
              denomin = 1.d0
           elsewhere
              temp = 0.d0
              denomin = 0.d0
           end where

           if (lage<retage) then
              gaggeffs(J,I) = gaggeffs(J,I) + SUM(wgi(J,:)*temp(:)/wagetge(J))*popsize(lage)/real(gensize)
           endif

           ! GAGGTRANSF - AGGREGATE EDU TRANSFER by (EDU,STATUS) group
           where (etracking==J .and. stracking==I)
              temp = ttracking
           elsewhere
              temp = 0.d0
           end where
           gaggtransf(J,I) = gaggtransf(J,I) + SUM(temp)*popsize(lage)/real(gensize,long)
           ! NOTICE: average transf * share in group * popsize = sum transfs * popsize / gensize

           ! TRANSFSHARE - SHARE OF Students RECEIVING A TRANSFER, BY (EDU,AGE,STATUS) GROUP 
           where (etracking==J .and. stracking==I .and. ttracking>0.d0)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           end where

           where (etracking==J .and. stracking==I)
              denomin = 1.d0
           elsewhere
              denomin = 0.d0
           end where
           if ( sum(denomin)>0 ) then
              transfshare(J,lage,I) = (SUM(temp)/real(sum(denomin),long))
           endif

           ! For the following averages the denominators are missing and will be computed below.

           ! AVGCONSPRERET(NEDU,NGPST)
           ! COMPUTE TOTAL CONSUMPTION. USED TO DERIVE AVERAGE CONSUMPTION (BELOW) 
           where (etracking==J .and. stracking==I .and. atracking==0)
              temp = consi
           elsewhere
              temp = 0.d0
           end where

           if (lage<retage) then
              avgconspreret(J,I) = avgconspreret(J,I) + SUM(temp)*popsize(lage)
           endif


           ! AVGABIL - TOTAL ABILITY (FIXED EFFECT) by (EDU,STATUS) - USED TO COMPUTE AVERAGES (below)
           where (etracking==J .and. stracking==I .and. &
                atracking==0)
              temp = ftracking
           elsewhere
              temp = 0.d0
           end where
           if (lage<retage) then
              avgabil(J,I) = avgabil(J,I) + SUM(temp)*popsize(lage)
           endif

           ! AVGWAGE & AVGEARN - TOTAL WAGE (earning per hour) and TOTAL EARNINGS (by EDU-STATUS)
           ! USED TO COMPUTE AVERAGES (BELOW)
           where (etracking==J .and. stracking==I .and. &
                atracking==0)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           end where
           if (lage<retage) then
              avgwage(J,I) = avgwage(J,I) + SUM(wgi(J,:)*temp(:))*popsize(lage)
              lavgwage(J,I) = lavgwage(J,I) + SUM(log(wgi(J,:))*temp(:))*popsize(lage)
              sdwage(J,I) = sdwage(J,I) + SUM((wgi(J,:)**2.d0)*temp(:))*popsize(lage)
              lsdwage(J,I) = lsdwage(J,I) + SUM((log(wgi(J,:))**2.d0)*temp(:))*popsize(lage)   			
              avgearn(J,I) = avgearn(J,I) + SUM(wgi(J,:)*houri(:)*temp(:))*popsize(lage)
           endif

        enddo	! I=student/worker, crime/nocrime, robbed/not robbed status
     
          
     enddo	! J=education


     continue


     do j=1,NEDU
        do i=1,NGPF-1
           do lstind=2,ngpst

              ! ABIFRACW - SHARES OF (EDU,ABILITY,STIND) FOR EACH AGE GROUP - excluding students
              where (etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              end where

              where (stracking>1)
                 denomin = 1.d0
              elsewhere
                 denomin = 0.d0
              endwhere

              abifracw(J,lage,I,lstind-1) = sum(temp)/sum(denomin) !real(gensize,long) !

              numwork(J,lage,I) = numwork(J,lage,I)+sum(temp)	! number of (potential) workers by Edu,Age,Ability, including inmates.

              temp = 0.d0

              ! ABIFRACWF - SHARES OF (EDU,AGE,ABILITY)  
              ! - excluding students and excluding inmates (atracking==0)
              where (etracking==J .and. atracking==0 .and. &
                   stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              end where

              where (stracking>1 .and. atracking==0)
                 denomin = 1.d0
              elsewhere
                 denomin= 0.d0
              endwhere

              abifracwf(J,lage,I,lstind-1) = sum(temp)/sum(denomin) !real(gensize,long) !

           enddo   ! END DO lstind=2,ngpst			

           if (lage<retage) then   ! Only for working-age people
              ! AV. WAGES BY (by EDU-ABILITY) - workers only (no students,no inmates)
              where (etracking==J .and. ftracking>=fgrid(I) .and. &
                   ftracking<fgrid(I+1) .and. atracking==0)
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              end where
              avgwageABEDU(J,I) = avgwageABEDU(J,I) + SUM(wgi(J,:)*temp(:))*popsize(lage)	
           end if ! if lage<retage

        enddo   ! NGPF-1
     enddo   ! NEDU

     continue


     ! **** AVERAGES by EDU/ABILITY BINS ****
     do J=1,NEDU
        do I=1,NGPF-1
           do lstind=1,ngpst

              temp = 0.d0; tempbis = 0.d0
              ! NUMHEADS(NEDU,NGPST,NGPF-1,2) : headcount for different sub-groups
              where (etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1) .and. &
                   atracking==0)
                 temp = 1.d0
              endwhere
              where(etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1) .and. &
                   atracking>0)
                 tempbis = 1.d0
              end where

              NUMHEADS_AGE(J,lage,lstind,I,1) = sum(temp)*popsize(lage)
              NUMHEADS_AGE(J,lage,lstind,I,2) = sum(tempbis)*popsize(lage)

              ! ABIFRAC - SHARES OF (EDU,ABILITY,STIND) FOR EACH AGE GROUP,including students/inmates
              where (etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              end where
              abifrac(J,lage,I,lstind) = sum(temp)/real(gensize,long)

              ! AVEABI & AVEWEALTH - TOTAL Ability and wealth by (EDU,ABILITY BIN,STIND) 
              ! FOR EACH AGE GROUP. USED TO COMPUTE AVERAGES (BELOW)
              ! Including students.
              where (etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp=1.d0
              elsewhere
                 temp=0.d0
              end where
              if (sum(temp)>0.d0) then
                 aveabi(J,lage,I,lstind) = sum(temp(:)*ftracking(:))*POPSIZE(LAGE)
                 avewealth(J,lage,I,lstind) = sum(temp(:)*wtracking(:))*POPSIZE(LAGE)
              endif

              ! AVGCONSPRET_ABEDU - TOTAL CONSUMPTION by (EDUCATION,ABILITY,ST_IND)
              ! WORKERS ONLY (NO INMATES) - used to compute average cons
              if (lstind>1) then	! This variable is computed only for workers
                 where (etracking==J .and. stracking==lstind .and. &
                      ftracking>=fgrid(I) .and. ftracking<fgrid(I+1) .and. &
                      atracking==0)
                    temp = 1.d0
                 elsewhere
                    temp = 0.d0
                 end where
                 if (lage<retage) then
                    avgconspret_abedu(J,I,lstind-1) = avgconspret_abedu(J,I,lstind-1) + &
                         SUM(temp(:)*consi(:))*popsize(lage)
                 endif
              endif	! end if lstind>1

           enddo
        enddo
     enddo

     continue

     do J=1,NEDU
        do I=1,NGPF-1
           do lstind=1,ngpst
              ! AVGINC - TOTAL INCOME PRE-RET, INCLUDING STUDENTS (NO inmates)
              ! (includes interests on assets which can be negative)
              ! USED TO COMPUTE AVERAGE (BELOW)
              where (etracking==J .and. stracking==lstind .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1) .and. &
                   atracking==0)
                 temp = inci
              elsewhere
                 temp = 0.d0
              end where

              if (lage<retage) then
                 avginc(J,I,lstind) = avginc(J,I,lstind) + SUM(temp)*popsize(lage)
                 continue
              endif
           enddo
        enddo
     enddo

     continue

     do J=1,NEDU
		do I=1,ngpst
           if (edufrac(J,lage,I)>0) then
              bcpeople(I,lage,J) = bcpeople(I,lage,J) / (edufrac(J,lage,I)*real(gensize,long))
           endif
           continue
		enddo
     enddo


     ! ***** Programme Stats *****

     IF (subregime==1 .or. subregime==10) then  ! These stats are computed only for means tested or randomized programmes

        IF (LAGE==1) then 

           ! Compute share of eligble people
           where(takeup<4)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           pcteligible = SUM(temp)/real(gensize)

           ! Compute share of treated conditional on being eligible
           where(takeup<3)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           if (pcteligible>0.d0) then
              pcttreatedcond = SUM(temp)/(pcteligible*real(gensize))
           endif

           temp = 0.d0
           ! Compute share of treated (unconditional on being eligible)
           where(takeup<3)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           pcttreated = SUM(temp)/real(gensize)

           ! Compute share of takeup by ability (conditional on being eligible and treated)
           DO I=1,NGPF-1

              where(takeup==1 .and. ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              endwhere

              where(takeup<3 .and. ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 denomin = 1.d0
              elsewhere
                 denomin = 0.d0
              endwhere
              pcttakeup(I) = SUM(temp)/SUM(denomin)

           ENDDO

        ENDIF ! Lage==1

        ! STATS ABOUT OUTCOMES OF PROGRAM BEING EVALUATED
        DO J=1,4

           ! Compute average wealth for the 4 programme groups
           where (takeup==J)
              temp = wtracking
              denomin = 1.d0
           elsewhere
              temp = 0.d0
              denomin = 0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,1) = SUM(temp)/SUM(denomin) ! average wealth in the 4 groups
           endif

           ! Compute average consumption for the 4 programme groups
           where (takeup==J)
              temp = consi
              denomin = 1.d0
           elsewhere
              temp = 0.d0
              denomin = 0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,2) = SUM(temp)/SUM(denomin) ! average consumption in the 4 groups
           endif

           if (lage==1) then   ! Just the first age is sufficient, since ability does not change
              ! Compute average ability for the 4 programme groups
              where (takeup==J)
                 temp = ftracking
                 denomin = 1.d0
              elsewhere
                 temp = 0.d0
                 denomin = 0.d0
              endwhere
              if (sum(denomin)>0.d0) then
                 stats_pr(lage,J,3) = SUM(temp)/SUM(denomin) ! average ability in the 4 groups
              endif   ! sum(denomin)
           endif   ! lage=1

           ! Compute average work income for the 4 programme groups
           where (takeup==J .and. stracking>1)
              temp = inci
              denomin = 1.d0
           elsewhere
              temp = 0.d0
              denomin = 0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,4) = SUM(temp)/SUM(denomin) ! average work income in the 4 groups
           endif

           ! Compute average crime rate for the 4 programme groups
           where (takeup==J .and. stracking>1)
              temp = ctracking
              denomin = 1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,5) = SUM(temp)/SUM(denomin) ! average crime rate in the 4 groups
           endif

           ! Compute average crime intensity for the 4 programme groups
           where (takeup==J .and. stracking>1)
              temp = intctracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,6) = SUM(temp)/SUM(denomin) ! average intensity of crime in the 4 groups
           endif

           ! Compute average arrest rate for the 4 programme groups
           where (takeup==J .and. stracking>1)
              temp = atracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              stats_pr(lage,J,7) = SUM(temp)/SUM(denomin) ! average arrest rate in the 4 groups
           endif

           !*********************
           if (lage==sum(eduyr)+1) then   ! Compute this statistic after all schooling is completed (mortality is random, so no effect from that)
              DO I=1,NGPF-1
                 DO I1=1,NEDU
                    ! Share of education by ability for the 4 treatment groups. dimension(4,ngpf-1,nedu) :: edushare_pr
                    where (takeup==J .and. stracking>1 .and. etracking==I1 .and. &
                         ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                       temp = 1.d0
                    elsewhere
                       temp = 0.d0
                    endwhere

                    where (takeup==J .and. stracking>1 .and. &
                         ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                       denomin = 1.d0
                    elsewhere
                       denomin = 0.d0
                    endwhere

                    if (SUM(denomin)>0.d0) then
                       edushare_pr(J,I,I1) = SUM(temp)/SUM(denomin) ! education share in the 4 groups
                    endif

        ! Compute enrolment and arrest rates in eligible and non-eligible groups 
        ! (used to verify effect of HS graduation on arrest rate, as in LochnerMoretti 2004, table 9)
        
        ! First: Share of education by eligibility status
                    where (takeup<4 .and. stracking>1 .and. etracking>1)
                       temp = 1.d0
                    elsewhere
                       temp = 0.d0
                    endwhere
                    where (takeup<4 .and. stracking>1)
                       denomin = 1.d0
                    elsewhere
                       denomin = 0.d0
                    endwhere
                    if (SUM(denomin)>0.d0) then
                       arrest_pr(:,1,1) = SUM(temp)/SUM(denomin) ! HS graduation: eligibles 
                    endif

                    where (takeup==4 .and. stracking>1 .and. etracking>1)
                       temp = 1.d0
                    elsewhere
                       temp = 0.d0
                    endwhere
                    where (takeup==4 .and. stracking>1)
                       denomin = 1.d0
                    elsewhere
                       denomin = 0.d0
                    endwhere
                    if (SUM(denomin)>0.d0) then
                       arrest_pr(:,2,1) = SUM(temp)/SUM(denomin) ! HS graduation: ineligibles
                    endif

                 ENDDO
              ENDDO
           endif ! lage==sum(eduyr)+1


         ! ARREST_PR: compute arrest rates.
         ! Compute average arrest rate for eligibles and ineligibles

         ! *** (HS graduates)
         where (takeup<4 .and. stracking>1 .and. etracking>1)
              temp = atracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              arrest_pr(lage,1,2) = SUM(temp)/SUM(denomin) ! arrest rate HS grads: eligibles
           endif

         where (takeup==4 .and. stracking>1 .and. etracking>1)
              temp = atracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              arrest_pr(lage,2,2) = SUM(temp)/SUM(denomin) ! arrest rate HS grads: ineligibles
           endif
    
         ! *** (HS dropouts)
         where (takeup<4 .and. stracking>1 .and. etracking==1)
              temp = atracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              arrest_pr(lage,1,3) = SUM(temp)/SUM(denomin) ! arrest rate HS d.o.: eligibles
           endif

         where (takeup==4 .and. stracking>1 .and. etracking==1)
              temp = atracking
              denomin=1.d0
           elsewhere
              temp = 0.d0
              denomin=0.d0
           endwhere
           if (sum(denomin)>0.d0) then
              arrest_pr(lage,2,3) = SUM(temp)/SUM(denomin) ! arrest rate HS d.o.: ineligibles
           endif

        ENDDO ! J
     END IF  ! Subregime==1 .or. subregime==10


     !************************************************************************************ 
     !*********************************** Crime Stats ************************************
     !************************************************************************************
     IF (lage<retage) THEN

        ! We define the absolute # of criminals and crimes
        temp = 0.d0
        tempbis = 0.d0
        DO J=1,nedu
           do i=1,NGPF-1
              where(etracking==J .and. stracking>1 .and. ctracking==1 .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
                 tempbis = real(intctracking)
              elsewhere
                 temp = 0.d0
                 tempbis = 0.d0
              endwhere
              numcriminal(J,lage,I) = SUM(temp)
              numcrimes(J,lage,I) = SUM(tempbis)
              ! Define crime rates by (edu,age,ability) bins (denominator: all workers)
              if (numwork(J,lage,I)>0.d0) then
                 ccrime(J,lage,I)=numcriminal(J,lage,I)/numwork(J,lage,I)
              endif
              temp = 0.d0
              do i2=1,maxjail/2-1
                 where(etracking==J .and. stracking>1 .and. ctracking==1 .and. &
                      ftracking>=fgrid(I) .and. ftracking<fgrid(I+1) .and. &
                      crim_fixed>=cgrid(i2) .and. crim_fixed<cgrid(i2+1))
                    temp = 1.d0
                 elsewhere
                    temp = 0.d0
                 endwhere

                 criminal_cgrid(i2,J,lage,I) = SUM(temp)
              enddo
           enddo
        enddo

        tempbis=0.d0	
        temp=0.d0
        do J=1,nedu
           do i=1,NGPF-1

              ! Headcount (absolute #) of criminals apprehended in the current period
              where(etracking==J .and. stracking>1 .and.&
                   atracking==1 .and. &
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              endwhere
              numappr(J,lage,I) = SUM(temp)

              ! Headcount (absolute #) of criminals just out of jail
              where(etracking==J .and. stracking>1 .and.&
                   ((atracking_old==prisT .and. res_id==2) .or. &
                   atracking_old>prisT .or. & 
                   (atracking_old2==prisT .and. res_id_old==2) .or. &
                   atracking_old2>prisT) .and.&
                   ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              endwhere
              numappr2(J,lage,I) = SUM(temp)

              ! Headcount (absolute #) of criminals in jail at end of the current period
              where(etracking==J .and. stracking>1 .and. atracking>0 &
                   .and. ftracking>=fgrid(I) .and. ftracking<fgrid(I+1))
                 temp = 1.d0
              elsewhere
                 temp = 0.d0
              endwhere
              numjailed(J,lage,I) = SUM(temp)

              DO lstind = 2,ngpst
                 DO i2=1,maxjail/2-1
                    WHERE(etracking==J .AND. stracking==lstind .AND. &
                         ftracking>=fgrid(I) .AND. ftracking<fgrid(I+1) .AND. &
                         crim_fixed>=cgrid(i2) .AND. crim_fixed<cgrid(i2+1))
                       temp = 1.d0
                    ELSEWHERE
                       temp = 0.d0
                    endwhere

                    work_cgrid(i2,J,lage,I) = work_cgrid(i2,J,lage,I) + SUM(temp)
                 ENDDO
              ENDDO


           enddo   ! enddo NGPF-1

           if (lage<retage)	numapprage(lage) = numapprage(lage) + sum(numappr(J,lage,:))

           ! Construct absolute count of arrested recidivist as a function of (edu,age)
           where (etracking==J .and. stracking>1 .and. atracking==1 .and. &
                recidtracking>1)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           numrecid(J,lage) = SUM(temp)


           ! Absolute count of unemployed arrested criminals as a function of (edu,age)
           where (etracking==J .and. stracking>1 .and. atracking==1 .and. &
                zet_id==1)
              temp = 1.d0
           elsewhere
              temp = 0.d0
           endwhere
           numunempappr(J,lage) = SUM(temp)

        enddo   ! enddo nedu


        continue

        ! Construct absolute count of arrested recidivist among those just released (func. of age)
        where (atracking==1 .and. &
             ((atracking_old==prisT .and. res_id==2) .or. &
             atracking_old>prisT .or. &
             (atracking_old2==prisT .and. res_id_old==2) .or. &
             atracking_old2>prisT))
           temp = 1.d0
        elsewhere
           temp = 0.d0
        endwhere
        numrecid2(lage) = SUM(temp)


        ! Crime rates as a function of (edu,age) 	
        do J=1,NEDU
           ! (denominator: all workers)	
           if (SUM(numwork(J,lage,:))>0.d0) then
              ccrime_intab(J,lage)=SUM(numcriminal(J,lage,:))/SUM(numwork(J,lage,:))
           endif
        enddo

        ! Crime rates as a function of (age,ability)
        do I=1,NGPF-1

           ! (denominator: all workers)	
           if (SUM(numwork(:,lage,I))>0.d0) then
              ccrime_intedu(lage,I)=SUM(numcriminal(:,lage,I))/SUM(numwork(:,lage,I))
           endif

        enddo   ! endif I=1,NGPF-1

        continue

        ! Crime rates as a function of age only
        piece1=0.d0; piece2=0.d0
        do J=1,NEDU
           piece1(lage) = piece1(lage) + SUM(numcriminal(J,lage,:))
           piece2(lage) = piece2(lage) + SUM(numwork(J,lage,:))
        enddo
        if (piece2(lage)>0.d0) then
           ccrime_intabedu(lage) = piece1(lage)/piece2(lage)
        endif

     ENDIF ! (lage<retage)

     !***********************************************************************
     !*************************** END OF CRIME STATS ************************
     !***********************************************************************


     ! UPDATE THE SHARE OF NON STUDENT WITH WEALTH LESS OR EQUAL ZERO
     !Add to fraction of total NON STUDENT population with wealth<= critw 

     where (wtracking <= critw .and. stracking>1) 
        temp = 1.d0
     elsewhere
        temp = 0.d0
     endwhere
     ! Compute age specfic share times their (non normalised) weights
     tempvar1 = 0.d0
     do I=2,ngpst
	    tempvar1 = tempvar1 + sum(EDUFRACTOT(:,lage,I))
     enddo
     wealthzeroW = wealthzeroW + (sum(temp)/( tempvar1*real(gensize))) * popsize(lage) * tempvar1
     incrementvar1 = incrementvar1 +  popsize(lage) *  tempvar1     ! Total NON STUDENTS of age=lage






  enddo	! **** LIFE DO - enddo over lage between 1 and lifet ************************************
  !********************************************************************************************
  !********************************************************************************************

  ! rescale the count of people appreheneded by age group
  numapprage(1:retage-1) = numapprage(1:retage-1)*popsize(1:retage-1)/real(gensize,long)

  ! GENERAL COUNT VARIABLE - INTEGRATED OVER AGE

  do J=1,NEDU
     do I=1,NGPF-1
        do lstind=1,ngpst    
           NUMHEADS(J,lstind,I,1) = sum(NUMHEADS_AGE(J,1:RETAGE-1,lstind,I,1))
           NUMHEADS(J,lstind,I,2) = sum(NUMHEADS_AGE(J,1:RETAGE-1,lstind,I,2))
        enddo
     enddo
  enddo

  ! Aggregate (means-tested / randomized) programme stats. Only for workers !
  DO J=1,4
     DO I=1,7
        aggstats_pr(J,I) = sum(stats_pr(1:retage-1,J,I)*popsize(1:retage-1)) / sum(popsize(1:retage-1))
     ENDDO
  ENDDO

  ! Normalize the WealthzeroW share dividing it by incrementvar1
  if (incrementvar1>0.d0) then
     wealthzeroW = wealthzeroW / incrementvar1
  else
     print *, "No workers in the model ?? incrementvar1 ==", incrementvar1
  endif


  ! NOTICE: crime rate in data is in fact a victimization rate, meaning that the
  !denominator is the number of ! potential victims. Therefore, since we don't
  !let retired people to be victimized, then we have to exclude them ! from the
  ! denominator for crime rates.

  ! Crime rates as a function of (edu,ability) - computed outside the major age loop because we sum over ages
  do J=1,NEDU
     do I=1,NGPF-1

        ! (denominator: all workers)	
        if (SUM(numwork(J,1:retage-1,I))>0.d0) then
           ccrime_intage(J,I)=SUM(numcriminal(J,1:retage-1,I)*popsize(1:retage-1))/SUM(numwork(J,1:retage-1,I)*popsize(1:retage-1))
           numcriminal_abiedu(J,I)=SUM(numcriminal(J,1:retage-1,I)*popsize(1:retage-1))
        endif
        do i2=1, maxjail/2-1
           if (SUM(work_cgrid(i2,J,1:retage-1,I))>0.d0) then
              ccrime_cgrid(i2,J,I)=SUM(criminal_cgrid(i2,J,1:retage-1,I)*popsize(1:retage-1))/SUM(work_cgrid(i2,J,1:retage-1,I)*popsize(1:retage-1))
              selec_cgrid(i2,J,I)=SUM(work_cgrid(i2,J,1:retage-1,I)*popsize(1:retage-1))
           endif
        enddo
     enddo
  enddo

  continue

  ! Crime rates as a function of ability only
  pieceA=0.d0; pieceB=0.d0
  do I=1,NGPF-1
     do J=1,NEDU
        pieceA(I) = pieceA(I) + SUM(numcriminal(J,1:retage-1,I)*popsize(1:retage-1))
        pieceB(I) = pieceB(I) + SUM(numwork(J,1:retage-1,I)*popsize(1:retage-1))
        numcrimes_intagedu(I)= numcrimes_intagedu(I) + SUM(numcrimes(J,1:retage-1,I) &
             &*popsize(1:retage-1)/real(gensize,long))
     enddo
     if (pieceB(I)>0.d0) then
        criminal_byabi(I) = pieceA(I)
        ccrime_intagedu(I) = pieceA(I)/pieceB(I)
     endif
  enddo

  continue

  ! Crime rates as a function of education only
  pieceAlpha=0.d0; pieceBeta=0.d0; aggprexp = 0.d0
  do J=1,NEDU
     do lage=1,retage-1  !lifet
        pieceAlpha(J) = pieceAlpha(J) + SUM(numcriminal(J,lage,:)*popsize(lage))
        pieceBeta(J)  = pieceBeta(J)  + SUM(numwork(J,lage,:)*popsize(lage))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Calculates absolute number of criminals and workers by education

        numcrimtot(J)=numcrimtot(J)+sum(numcriminal(J,lage,:)*popsize(lage)/real(gensize,long))
        numapprtot(J)=numapprtot(J)+sum(numappr(J,lage,:)*popsize(lage)/real(gensize,long))
        numworktot(J)=numworktot(J)+sum(numwork(J,lage,:)*popsize(lage)/real(gensize,long))
        numrecidtot(J)=numrecidtot(J)+numrecid(J,lage)*popsize(lage)/real(gensize,long)
        numunapprtot(J)=numunapprtot(J)+numunempappr(J,lage)*popsize(lage)/real(gensize,long)
        numjailedtot(J,lage) = sum(numjailed(J,lage,:))*popsize(lage)/real(gensize,long)
        numapprtot2(lage)=numapprtot2(lage)+sum(numappr2(J,lage,:)*popsize(lage)/real(gensize,long))
        continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! Aggregate prison costs (total)
        aggprexp = aggprexp + sum(numjailed(J,lage,:)*popsize(lage)/real(gensize))*(cbar+emme)
     enddo

     if (pieceBeta(J)>0.d0) then
        ccrime_intabage(J) = pieceAlpha(J)/pieceBeta(J)
     endif
  enddo

  continue

  ! Aggregate crime rate
  tempvar1 = SUM(pieceB)	! Total of potential criminals (from summing over all (edu,age,ability) space
  ccrime_agg = sum(numcrimtot)/sum(numworktot)
  victim_agg = SUM(numcrimes_intagedu)/sum(numworktot)
  recidiv_agg = SUM(numrecidtot)/sum(numapprtot)
  unempappr_agg = SUM(numunapprtot)/SUM(numapprtot)


  numrecid2(1:retage-1)= numrecid2(1:retage-1)*popsize(1:retage-1)/real(gensize,long)
  recidiv_agg2 = sum(numrecid2)/sum(numapprtot2)


  continue

  do J=1,NEDU
     do I=1,ngpst
        ! AVTRANSFREC - AVERAGE TRANFER AMONG RECIPIENTS by (EDU,STATUS)
        if (sum(edufrac(J,:,I)*transfshare(J,:,I)*popsize(:)) > 0.d0) then
           avtransfrec(J,I) = gaggtransf(J,I) / sum(edufrac(J,:,I)*transfshare(J,:,I)*popsize(:))
        endif
     enddo

     teenjailed(J) = sum(numjailedtot(J,1:3))   ! # Of prisoners with age<19
     jailedbyedu(J) = sum(numjailedtot(J,:))    ! # Of prisones with LTHS
  enddo

  teenjailed_agg = sum(teenjailed)/sum(jailedbyedu)  ! Share of prisoners with age<19
  LTHSjailed_agg = jailedbyedu(1)/sum(jailedbyedu)      ! Share of prisoners with LTHS

  !	**	COMPUTE UNCAVGEARN AND AGGPENS	**
  UNCavgearn = 0.d0
  tempvar1 = 0.d0
  AGGPENS=0.d0
  do J=1,nedu
     do I=2,ngpst
        UNCavgearn=UNCavgearn+avgearn(J,I)   
        tempvar1=tempvar1+sum(NUMHEADS(J,I,:,1))
        ! Following line if retired prisoners are entitled to pension
        AGGPENS=AGGPENS+sum(edufractot(J,retage:lifet,I)*popsize(retage:lifet))*penv(retage)
     end do
  end do

  UNCavgearn=UNCavgearn/MAX(1.D0,tempvar1)
  
  continue



  continue

  ! BUILD EDUDIST=SHARES OF WORKERS BY (EDU,ABILITY) FOR EACH AGE GROUP 
  EDUDIST = 0.0D0
  DO LAGE=1,LIFET
     DO I=1,NEDU
        do J=1,ngpf-1
           EDUDIST(I,LAGE,J)= SUM(ABIFRACWF(I,LAGE,J,:))
        enddo
     ENDDO
  ENDDO

  ! Income by education group for Workers (no students, no inmates)
  grpincw = 0.d0
  do J=1,nedu
     tempvar1=0.d0
     do I=1,ngpf-1
        do lstind=2,ngpst
           grpincw(J)=grpincw(J)+avginc(J,I,lstind)    !*NUMHEADS(J,lstind,I,1)
           tempvar1=tempvar1+NUMHEADS(J,lstind,I,1)
        enddo
     enddo
     if (tempvar1>0) then
        grpincw(J)=grpincw(J)/tempvar1
     endif
  enddo


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Create CROSS-SECTIONAL DISTRIBUTIONS	!
  wdist = 0.0d0 ; wdist_BEQ = 0.0d0 ;  wdist_2 = 0.0d0;  wdist3 = 0.0d0
  j = 1; 	j1=1;	j2=1;  j3=1
  dead = 1 ! Initialize vector tracking the dead - all alive at the beginning

  DO lage = 1,lifet
     wdist((lage-1)*ss+1:lage*ss) = wealthdistss(:,lage) ! Save ss wealth obs for each lage
     wdist_temp = wdist((lage-1)*ss+1:lage*ss)			! Pass the above to utility vector


     ! Adjust distributions to account for population shrinkage due to mortality
     CALL DRNUN (ss,rv)
     DO i=1,ss
        IF (rv(i) < surprob(lage) .and. dead(i)==1) then	!popsize(lage)) THEN
           if (lage<lifet) then
              wdist_2(j) = wdist_temp(i)
              j = j+1
           endif
           if (incdistss(i,lage)>-1.d5) then
              wdistinc_2(j1) = incdistss(i,lage)    ! Save income observation
              j1=j1+1
           endif
           if (lage<retage .and. wgidistss(i,lage)>-1.d5) then 
              wdistwgi(j3) = wgidistss(i,lage)
              j3 = j3+1
           endif
           
           
           ! SAVE REMAINING ASSETS TO OBTAIN ASSETS DISTRIBUTION FOR YOUNGEST AGE GROUP (NEWBORN IN MODEL)
        ELSE IF (rv(i) >= surprob(lage) .and. dead(i)==1) THEN
           if (lage<lifet) then
              wdist_BEQ(j2) = wdist_temp(i)
              dead(i) = 0
              j2 = j2+1
           endif
           continue
        ENDIF

     ENDDO	! end do ss

  ENDDO ! end lage do
  ! ***** Global Vector containing wealth distribution's sample in the economy ***** 

  continue

  ! ADD WEALTH DISTRIBUTION OF AGE "lifet" PEOPLE (ALL OTHER AGES ARE ALREADY IN)
  DO ii=1,ss
     if (dead(ii)==1) then
        wdist_BEQ(j2) = wealthi_agemax(ii)
        j2 = j2+1
     endif
  ENDDO

  continue

  allocate (wvecglobal(j-1))
  wvecglobal = wdist_2(1:j-1)


  ! ******* STATISTICS for WEALTH and INCOME distributions ******* 

  ! (1)
  CALL DISTSTAT(j-1,1,wdist_2(1:j-1),wginiss,w9010ss,fwlzss,wzss,avwealth,medwealth,varwealth) ! Procs
  CALL WAGEDIST(j3-1,1,wdistwgi(1:j3-1),meanwage,medwage,varwage,wage10,wage90)
  topcode_loc = minloc(inter_vivos_sort,1,inter_vivos_sort>3.17d0*medwage)-1  
  ! For consistency with the data, "topcode" inter vivos transfer
  WHERE (inter_vivos_sort>3.17d0*medwage)
      inter_vivos_sort = 3.17d0*medwage
  ENDWHERE 
  
  CALL DISTSTAT(topcode_loc,1,inter_vivos_sort(1:topcode_loc),ygini, pw15, yrwza, ywpnw, yavwealth, ymedwealth, yvarwealth) 
     q_size=int(topcode_loc/4)
     moms(3) = yrwza ! Share of youngest with wealth = 0
     
     ! Ratio of average inter vivos to median labour earnings       
     moms(4) = yavwealth/uncavgearn ! Ratio of average inter vivos transfer to avg labour earnings       
     moms(11) = yvarwealth**0.5/yavwealth ! Coefficient of variation for inter vivos transfers
  
  OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND') 
        WRITE(38,*),' '
        WRITE(38,*),'***********************************'
        WRITE (38,'(A40,F14.5)'), " Avg. assets in youngest age group cens. ", yavwealth
        CLOSE(unit=38)
  ! (2) - UPDATE YOUNG WEALTH DIST. ONLY IN THE STANDARD CASE - in calibration case, re-use the existing one
  IF (constinwealth == 0) THEN
     !Permute array inter_vivos to avoid correlation across generations
     wealthi_age1=inter_vivos(perm)	
     namedata='wealthy.txt'
     CALL save_vector(namedata,40,size(wealthi_age1),wealthi_age1)
  ENDIF

  ! (3)
  CALL INCDIST(j1-1,1,wdistinc_2(1:j1-1),avinc,medinc,varinc)			! Procs

  !	COMPUTE GOVERNMENT BUDGET
  !	(1) TRANSFEXP - Aggregate transfer expenditure of governemnt
  transfexp = sum(gaggtransf(:,1))
  if (subregime==5) then ! THIS LINE ACCOUNTS FOR EXTRA COLLEGE GRANTS' COSTS (ONLY FOR COLLEGE EXP.)
      numabil=0.d0; condedu=0.d0
      do i2=1,ngpf-1
         do I=1,nedu
            do J=2,ngpst
               do ii=1,2
                  numabil(i2)=numabil(i2)+SUM(NUMHEADS_AGE(I,sum(eduyr)+1:retage-1,J,i2,ii))    
                  condedu(I,i2)=condedu(I,i2)+SUM(NUMHEADS_AGE(I,sum(eduyr)+1:retage-1,J,i2,ii))
               enddo
            enddo
         enddo
      enddo
  UNCONDEDU=0.D0
  UNCONDedu(3)= sum(condedu(3,:))/sum(numabil)
  transfexp = transfexp + (UNCONDedu(3)-0.17)*0.4d0*sum(popsize(3:6))
  
  endif
  
  if (unc_qop==1) then
    !If unc_qop=1 add total value of UNCONDITIONAL transfer expenditure at age  1
    transfexp = transfexp + propsub*tuit(1)*(1.d0+DISCOUNT(ir(1),eduyr(2)-1,1,2))*popsize(1)
  endif  
  !	(2)	CALL BUDGET SUBR.
  CALL govbud(gaggeffs,aggwealth,Transfexp,Aggprexp,AGGPENS,Gexp,TAXrev)

  if (Gexp < 0.0d0) then
     print *, "                GOVERNMENT BUDGET DEFICIT		!!!!!!!             "
     !		print *, "                Execution should be terminated                    "
     !		stop
     print *, "Current expenditure deficit: ", Gexp
  else
     print *, "Current government Gexp:      ", Gexp
     print *, "Current government Transfexp: ", Transfexp
     print *, "Current total prison expend.: ", Aggprexp
  endif

  !! AGGREGATES !!
  ! AGGINC - Aggregate income equals the sum of the eagginc values
  agginc = 0.0d0
  do i=1,ngpst	! Sum over all possible stinds
     agginc = agginc + sum(eagginc(:,i))
  enddo

  ! UPDATE HC AGGREGATES
  HClag = HC
  HC(1) =  max(sum(gaggeffs(1,2:ngpst)),1.d-4) 
  HC(2) =  max(sum(gaggeffs(2,2:ngpst)),1.d-4)
  HC(3) =  max(sum(gaggeffs(3,2:ngpst)),1.d-4)

  ! AGGREGATE LABOR INPUT IN PRODUCTION
  SELECT CASE (techoice)
  CASE (0) ! SIMPLE CES
     NN=0.0d0
     do i=1,nedu
		NN = NN + SH(i)*(HC(i)**jamma)
     enddo
     NN = NN**(1.d0/jamma) ! aggregate HC input - N in model
  CASE (1) ! COBB DOUGLAS
     NN=1.0d0 
     do i=1,nedu
		NN = NN * HC(i)**SH(i)
     enddo	! aggregate HC input - N in model
  END SELECT

  NN = max(NN,1.d-2)

  continue

  ! AGGREGATE PHYSICAL CAPITAL INPUT , OUTPUT and CAPITAL / LABOR ratio
  ! NOTICE THAT:	PRODOUT = CONSTANT * K^THETA*N^(1-THETA)
  ! THEREFORE, IF CONSTANT = 1, WAGE(2) = (1-TAXN)*(1-THETA)*SH(2)*PRODOUT/H(2)
  ! IF WE SET CONSTANT TO THE RECIPROCAL OF W(2),  WE ARE NORMALIZINF W(2) = 1
  ! NOTICE THAT THE CAPITAL/LABOR RATIO IS WRITTEN AS (R/(CONSTANT*THETA))*(1/(THETA-1))
  ! R IS THE MARGINAL PRODUCT OF CAPITAL, AND IRATE = (R-DELTA)*(1-TAXK)
  ! THEREFORE R = (IRATE/(1-TAXK)) + DELTA, HENCE THE EXPRESSION FOR THE CAP/LAB RATIO

  ! Capital markets clearing
  SELECT CASE (ZERONW) 
  CASE (0)
     PhK = max( aggwealth , 1.d-2) ! Endogenous rrate
  CASE (1)	! ZERONW = 1 if interest rates exogenous (zero net wealth)
     PhK = max( NN*((rrate/(1-taxk)+delta)/(theta*constanta))**(1.d0/(theta-1.d0)) , 1.d-2)	! Exogenous rrate
  END SELECT

  prodout_lag = prodout
  prodout = (PhK**theta)*(NN**(1.d0-theta)) ! temporary prod. output with TFP=1

  if (tfpnorm==1) then	! Compute TFP normalizing coefficient based on current equilibrium
     SELECT CASE (techoice)	! Normalization factor (TFP)
     CASE (0) ! SIMPLE CES
		constanta = 1.d0/((1.d0-taxn)*prodout*(1.d0-theta)*sh(2)*(HC(2)**(jamma-1.d0))*(NN**(-jamma)))
     CASE (1) ! COBB DOUGLAS
		constanta = 1.d0/((1.d0-taxn)*prodout*(1.d0-theta)*sh(2)/HC(2))	! Normalizing wage(2) through TFP
     END SELECT
     constanta = constanta*scalewage ! The factor multiplying constanta is equal to the normalized wage
  endif

  prodout = prodout*constanta

  ! DISPLAY ITERATION SUMMARY !
  print *,' '
  print *,'***********************************'
  PRINT '(A40,F14.5)', " Interest rate after tax and depr         ", rrate
  PRINT '(A40,F14.5)', " MRK after tax and depr                   ", MRK		
  PRINT '(A40,F14.5)', " Discount factor                          ", bita
  PRINT '(A40,F14.5)', " Borrowing constraint                     ", wmin
  PRINT '(A40,F14.5)', " % of total popn with wealth <= 0         ", wealthzero
  PRINT '(A40,F14.5)', " % of NON student popn with wealth <= 0   ", wealthzeroW
  if (agginc>0.d0) then
     PRINT '(A40,F14.5)', "Wealth/Income                    ", aggwealth/agginc
  endif
  !		PRINT '(A40,F14.5)', "Wealth/output                    ", aggwealth/prodout
  !		PRINT '(A40,F14.5)', "W15p                             ", W15p
  PRINT '(A40,F14.5)', "Average  wealth                  ", avwealth
  PRINT '(A40,F14.5)', "Av. wealth under 35 / Av. wealth ", avwealth35/avwealth
  PRINT '(A40,F14.5)', "SD wealth                        ", varwealth**.5d0
  PRINT '(A40,F14.5)', "Median   wealth                  ", medwealth
  PRINT '(A40,F14.5)', "Lowest   wealth                  ", minval(wdist_2)
  PRINT '(A40,F14.5)', "Highest  wealth                  ", maxval(wdist_2)
  PRINT '(A40,F14.5)', "Share of students on B.C., edu 1 ", bcpeople(1,1,1) 
  PRINT '(A40,F14.5)', "Share of students on B.C., edu 2 ", bcpeople(1,3,2) 
  print*,'***********************************'
  PRINT '(A40,F14.5)', "Aggregate wealth                 ", aggwealth
  PRINT '(A40,F14.5)', "Aggregate capital stock          ", PhK
  PRINT '(A40,F14.5)', "Aggregate labor                  ", NN
  PRINT '(A40,F14.5)', "Aggregate output                 ", PRODOUT
  PRINT '(A40,F14.5)', "Net output                       ", &
       PRODOUT - delta*phK - Gexp - aggprexp
  PRINT '(A40,F14.5)', "Normalization Factor - Constanta ", constanta
  do i1=1,NEDU-1
     PRINT '(A40,I4,F14.5)', "Tech. Share for edu ", i1,  sh(i1)
  enddo
 
  print*,'***********************************'
  PRINT '(A40,F14.5)', "Av. consumption of students 1    ", avgconspreret(1,1)/max(1.d0,SUM(NUMHEADS(1,1,:,1)))
  PRINT '(A40,F14.5)', "Av. consumption of students 2    ", avgconspreret(2,1)/max(1.d0,SUM(NUMHEADS(2,1,:,1)))
  print*,'***********************************'
  PRINT '(A60)', ' Av. wealth and ability for HS stud. and working HS dropouts : '

  do i2=1,ngpf-1
     PRINT '(A40,I4,F10.2,TR2,F10.3)', ' HS students, abil.bin      = ', i2, &
          AVEwealth(1,1,i2,1)/MAX(1.D0,NUMHEADS_AGE(1,1,1,i2,1)), AVEabi(1,1,i2,1)/MAX(1.D0,NUMHEADS_AGE(1,1,1,i2,1))
  enddo

  do i2=1,ngpf-1
     PRINT '(A40,I4,F10.2,TR2,F10.3)', ' HS dropouts, abil.bin = ', i2, &
          
          sum(AVEwealth(1,1,i2,2:ngpst))/MAX(1.D0,SUM(NUMHEADS_AGE(1,1,2:ngpst,i2,1)+&
          NUMHEADS_AGE(1,1,2:ngpst,i2,2))),&
          
          sum(AVEabi(1,1,i2,2:ngpst))/MAX(1.D0,SUM(NUMHEADS_AGE(1,1,2:ngpst,i2,1)+&
          NUMHEADS_AGE(1,1,2:ngpst,i2,2)))

  enddo
  print*,'**********'
  PRINT '(A60)', ' Av. wealth and ability for Coll. stud. and working HS grads. : '

  do i2=1,ngpf-1
     PRINT '(A40,I4,F10.2,TR2,F10.3)', ' College students, abil.bin = ', i2, &
          AVEwealth(2,eduyr(2)+1,i2,1)/MAX(1.D0,NUMHEADS_AGE(2,eduyr(2)+1,1,i2,1)),&
          AVEabi(2,eduyr(2)+1,i2,1)/MAX(1.D0,NUMHEADS_AGE(2,eduyr(2)+1,1,i2,1))
  enddo

  do i2=1,ngpf-1
     PRINT '(A40,I4,F10.2,TR2,F10.3)', ' HS graduates , abil.bin = ', i2, &
          
          sum(AVEwealth(2,eduyr(2)+1,i2,2:ngpst))&
          /MAX(1.D0,SUM(NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,1)+&
          NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,2))),&
          
          sum(AVEabi(2,eduyr(2)+1,i2,2:ngpst))&
          /MAX(1.D0,SUM(NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,1)+&
          NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,2)))

  enddo
  print*,'***********************************'

  do i=1,nedu
     PRINT '(A40,I4)', ' EDU group = ', i
     do j=2,ngpst
		PRINT '(A30,I4,F14.5)', "Av. consumption st_ind =  ", j, avgconspreret(i,j)/max(1.d0,SUM(NUMHEADS(i,j,:,1)))
     enddo   ! do j=2,ngpst
     print*,'***********'

     do i2=1,ngpf-1
		PRINT '(A40,I4,F14.5)', ' Av. cons., st_ind=4,  ability group = ', i2, avgconspret_abedu(i,i2,3)/max(1.d0,numheads(i,4,i2,1))
     enddo
     print*,'***********'

     do j=1,ngpf-1
		PRINT '(A8,I3,A8,I3,A8,F10.5)',  'Edu=', i, 'Ab.Bin=', j, 'Av.Wage',avgwageABEDU(i,j)/max(1.d0,sum(numheads(i,2:ngpst,j,1)))
     enddo
     print*,'***********'

     do j=2,ngpst,2
		PRINT '(A30,I4,F14.5)', "Average earnings for st_ind = ", j, avgearn(i,j)/max(1.d0,sum(numheads(i,j,:,1)))
		PRINT '(A30,I4,F14.5)', "Average ability for st_ind =  ", j, avgabil(i,j)/max(1.d0,sum(numheads(i,j,:,1)))
		print*,'***********'
     enddo   !do j=2,ngpst,2

     ! Compute average WAGE for each education group    	
     tempvar1=0.d0; tempvar2=0.d0; tempvar3=0.d0; tempvar4=0.d0; checkvar=0.d0
     
     do j=2,ngpst
        tempvar1 = tempvar1 + avgwage(I,j)    !*sum(numheads(I,j,:,1))
    	tempvar2 = tempvar2 + sdwage(I,j)
    	tempvar3 = tempvar3 + lavgwage(I,j)
    	tempvar4 = tempvar4 + lsdwage(I,j)
        checkvar= checkvar + sum(numheads(I,j,:,1)) 
     enddo
     tempvar1 = tempvar1 / checkvar    
     tempvar2 = tempvar2 / checkvar
     tempvar3 = tempvar3 / checkvar
     tempvar4 = tempvar4 / checkvar

     print '(A40,I2,F11.5)', "Av.  post-tax wage rate  - edu ", I, tempvar1
     print '(A40,I2,F11.5)', "S.D. post-tax wage rate  - edu ", I, (tempvar2-tempvar1**2.d0)**0.5d0
     print '(A40,I2,F11.5)', "Av.  post-tax log wage rate  - edu ", I, tempvar3
     print '(A40,I2,F11.5)', "S.D. post-tax log wage rate  - edu ", I, (tempvar4-tempvar3**2.d0)**0.5d0
     print '(A40,I2,F11.5)', "Av. post-tax tot.income - edu ", I, grpincw(I)
     print*,'***********************************'
  enddo
  !	END SELECT

  numabil=0.d0; condedu=0.d0
  do i2=1,ngpf-1
     do I=1,nedu
        do J=2,ngpst
           do ii=1,2
              numabil(i2)=numabil(i2)+SUM(NUMHEADS_AGE(I,sum(eduyr)+1:retage-1,J,i2,ii))    
              condedu(I,i2)=condedu(I,i2)+SUM(NUMHEADS_AGE(I,sum(eduyr)+1:retage-1,J,i2,ii))
           enddo
        enddo
     enddo
  enddo


  do i=1,ngpf-1

     if (numabil(i)>0) then
		PRINT '(A14,I4,A20,F14.5)', "Ability", i , "in edu 1 ", condedu(1,i)/numabil(i)
		PRINT '(A14,I4,A20,F14.5)', "Ability", i , "in edu 2 ", condedu(2,i)/numabil(i)
		PRINT '(A14,I4,A20,F14.5)', "Ability", i , "in edu 3 ", condedu(3,i)/numabil(i)
     endif
     print*,'          *************************'

  enddo

  UNCONDEDU=0.D0

  UNCONDedu(1)= sum(condedu(1,:))/sum(numabil)	
  UNCONDedu(2)= sum(condedu(2,:))/sum(numabil)
  UNCONDedu(3)= sum(condedu(3,:))/sum(numabil)

  PRINT '(A40,F14.5)', "Total share in edu 1             ", UNCONDedu(1)
  PRINT '(A40,F14.5)', "Total share in edu 2             ", UNCONDedu(2)
  PRINT '(A40,F14.5)', "Total share in edu 3             ", UNCONDedu(3)
  print*,'***********************************'
  print '(A40,F14.5)', "Average post-tax income - workers", avinc
  print '(A40,F14.5)', "SD   of post-tax income - workers", varinc**.5d0
  print '(A40,F14.5)', "Median post-tax income  - workers", medinc
  print '(A40,F14.5)', "Average labour income   - workers", UNCavgearn
  print*,'***********************************'
  PRINT '(A40,F14.5)', "HC(1)                            ", HC(1)
  PRINT '(A40,F14.5)', "HC(2)                            ", HC(2)
  PRINT '(A40,F14.5)', "HC(3)                            ", HC(3)
  print*,'***********************************'
  incrementvar1=0.d0;incrementvar2=0.d0;incrementvar3=0.d0;
  do lstind=2,ngpst
     avabibyedu(1) = avabibyedu(1)+sum(AVEabi(1,1,:,lstind))
     avabibyedu(2) = avabibyedu(2)+sum(AVEabi(2,eduyr(2)+1,:,lstind))
     avabibyedu(3) = avabibyedu(3)+sum(AVEabi(3,sum(eduyr)+1,:,lstind))
     incrementvar1=incrementvar1+sum(NUMHEADS_AGE(1,1,lstind,:,1)+NUMHEADS_AGE(1,1,lstind,:,2))
     incrementvar2=incrementvar2+sum(NUMHEADS_AGE(2,eduyr(2)+1,lstind,:,1)+NUMHEADS_AGE(2,eduyr(2)+1,lstind,:,2))
     incrementvar3=incrementvar3+sum(NUMHEADS_AGE(3,sum(eduyr)+1,lstind,:,1)+NUMHEADS_AGE(3,sum(eduyr)+1,lstind,:,2))
  enddo

  avabibyedu(1) = avabibyedu(1)/incrementvar1
  avabibyedu(2) = avabibyedu(2)/incrementvar2
  avabibyedu(3) = avabibyedu(3)/incrementvar3

  PRINT '(A40,F14.5)', "Av. Ability - Edu 1              ", avabibyedu(1)
  PRINT '(A40,F14.5)', "Av. Ability - Edu 2              ", avabibyedu(2)
  PRINT '(A40,F14.5)', "Av. Ability - Edu 3              ", avabibyedu(3)
  print*,'***********************************'
  PRINT '(A40,F14.5)', "Total Tax Revenues               ", taxrev
  PRINT '(A40,F14.5)', "Total Transfer Expenditures      ", transfexp
  PRINT '(A40,F14.5)', "Total G Expenditure              ", Gexp
  if (polcosting==1) then
     PRINT '(A40,2(tr2,F5.4))', "Base and augmented lab tax rates  ", taxnbase , taxn
  else if (polcosting==2) then
     PRINT '(A40,2(tr2,F5.4))', "Base and augmented K tax rates    ", taxkbase , taxk
  endif
  print*,'***********************************'
  PRINT '(A40,F14.5)', "High School Tuition cost         ", tuit(1)
  PRINT '(A40,F14.5)', "University Tuition cost          ", tuit(2)
  if (sum(edufrac(2,:,1))>0.d0) then
     print*,'****************'
     PRINT '(A40,F14.5)', "% of Col. stud.s w/ transf       ", sum(transfshare(2,:,1)*edufrac(2,:,1)*popsize(:))/sum(edufrac(2,:,1)*popsize(:))
     PRINT '(A40,F14.5)', "Av. Transf among all Coll. stud. ", gaggtransf(2,1)/sum(edufrac(2,:,1)*popsize(:))
     PRINT '(A40,F14.5)', "Av. Transf among recipients only ", avtransfrec(2,1)
  endif
  if (sum(edufrac(1,:,1))>0.d0) then
     print*,'****************'
     PRINT '(A40,F14.5)', "% of HS stud.s w/ transf         ", sum(transfshare(1,:,1)*edufrac(1,:,1)*popsize(:))/sum(edufrac(1,:,1)*popsize(:))
     PRINT '(A40,F14.5)', "Av. Transf among all HS stud.    ", gaggtransf(1,1)/sum(edufrac(1,:,1)*popsize(:))
     PRINT '(A40,F14.5)', "Av. Transf among recipients only ", avtransfrec(1,1)
  endif
  print*,'***********************************'
  if (subregime==1 .or. subregime==10) then  ! Means-tested, randomized programme
     PRINT '(A40,F14.5)', "% of eligible people             ", pcteligible
     PRINT '(A40,F14.5)', "% of treated (given eligibility) ", pcttreatedcond
     PRINT '(A40,F14.5)', "% of treated (unconditional)     ", pcttreated
     print*,'**********'
     PRINT '(A50)', " % of take-up among treated by ability bin:   "
     DO I=1,NGPF-1
        PRINT '(A40,I4,F14.5)', " ability Bin ", I , pcttakeup(I)
     ENDDO
     print*,'**********'
     PRINT '(A40,F14.5)', "Average wealth at time of programme : "
     PRINT '(A40,F14.5)', " - among treated, take up     ", stats_pr(1,1,1)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", stats_pr(1,2,1)
     PRINT '(A40,F14.5)', " - among untreated            ", stats_pr(1,3,1)
     PRINT '(A40,F14.5)', " - non eligible               ", stats_pr(1,4,1)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average wealth over life cycle :      "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,1)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,1)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,1)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,1)		
     print*,'**********'
     PRINT '(A40,F14.5)', "Average ability :                     "
     PRINT '(A40,F14.5)', " - among treated, take up     ", stats_pr(1,1,3)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", stats_pr(1,2,3)
     PRINT '(A40,F14.5)', " - among untreated            ", stats_pr(1,3,3)
     PRINT '(A40,F14.5)', " - non eligible               ", stats_pr(1,4,3)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average income over life cycle :      "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,4)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,4)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,4)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,4)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average crime rate over life cycle :  "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,5)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,5)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,5)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,5)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average intensity of crime over life cycle :  "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,6)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,6)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,6)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,6)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average arrest rate over life cycle :  "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,7)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,7)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,7)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,7)
     print*,'**********'
     PRINT '(A40,F14.5)', "Average consumption over life cycle :  "
     PRINT '(A40,F14.5)', " - among treated, take up     ", aggstats_pr(1,2)
     PRINT '(A40,F14.5)', " - among treated, no take up  ", aggstats_pr(2,2)
     PRINT '(A40,F14.5)', " - among untreated            ", aggstats_pr(3,2)
     PRINT '(A40,F14.5)', " - non eligible               ", aggstats_pr(4,2)
     print*,'***********************************'
     PRINT '(A60)', "Education distribution by ability - various groups "
     PRINT '(A40)', " - among treated, take up     "
     DO I=1,NGPF-1
        DO J=1,NEDU
           PRINT '(A20,I4,A20,I4,F14.5)', " ability Bin ", I ,"  edu group ", J, edushare_pr(1,I,J) 
        ENDDO
     ENDDO
     print*,'**********'	
     PRINT '(A40)', " - among treated, no take up  "
     DO I=1,NGPF-1
        DO J=1,NEDU
           PRINT '(A20,I4,A20,I4,F14.5)', " ability Bin ", I ,"  edu group ", J, edushare_pr(2,I,J) 
        ENDDO
     ENDDO
     print*,'**********'
     PRINT '(A40)', " - among untreated            "
     DO I=1,NGPF-1
        DO J=1,NEDU
           PRINT '(A20,I4,A20,I4,F14.5)', " ability Bin ", I ,"  edu group ", J, edushare_pr(3,I,J) 
        ENDDO
     ENDDO
     print*,'**********'
     PRINT '(A40)', " - among non-eligible            "
     DO I=1,NGPF-1
        DO J=1,NEDU
           PRINT '(A20,I4,A20,I4,F14.5)', " ability Bin ", I ,"  edu group ", J, edushare_pr(4,I,J) 
        ENDDO
     ENDDO
     print*,'***********************************'
     PRINT '(A25,F14.5)', "HS grads: eligibles  ", sum(arrest_pr(3:45,1,1)*popsize(3:45)) / sum(popsize(3:45)) 
     PRINT '(A25,F14.5)', "HS grads: ineligibles", sum(arrest_pr(3:45,2,1)*popsize(3:45)) / sum(popsize(3:45)) 
     print*,'******'
     PRINT '(A38,F14.5)', "Eligibles: arrest rate among HS grads    ", sum(arrest_pr(3:45,1,2)*popsize(3:45)) / sum(popsize(3:45))
     PRINT '(A38,F14.5)', "Ineligibles: arrest rate among HS grads  ", sum(arrest_pr(3:45,2,2)*popsize(3:45)) / sum(popsize(3:45))
     print*,'******'
     PRINT '(A38,F14.5)', "Eligibles: arrest rate among LTHS        ", sum(arrest_pr(3:45,1,3)*popsize(3:45)) / sum(popsize(3:45))
     PRINT '(A38,F14.5)', "Ineligibles: arrest rate among LTHS      ", sum(arrest_pr(3:45,2,3)*popsize(3:45)) / sum(popsize(3:45))     
  endif   ! subregime==1 .or. subregime==10

  print*,'***********************************'
  PRINT '(A40,F14.5)', "Aggregate Crime rate             ", ccrime_agg
  !		PRINT '(A40,F14.5)', "# of criminals                   ", sum(numcrimtot(:)) !SUM(ccrime_intagedu(:)*pieceB(:))
  !		PRINT '(A40,F14.5)', "# of potential criminals         ", sum(numworktot(:)) !tempvar1
  PRINT '(A40,F14.5)', "Aggregate Victimization rate     ", victim_agg
  PRINT '(A40,F14.5)', "Share of prisoners age< 19       ", teenjailed_agg
  PRINT '(A40,F14.5)', "Share of prisoners with LTHS     ", LTHSjailed_agg
  PRINT '(A40,F14.5)', "Recidivists out of  apprehended  ", recidiv_agg
  PRINT '(A40,F14.5)', "Recidivists w/in 1 yr of release ", recidiv_agg2
  PRINT '(A40,F14.5)', "Unemployed out of apprehended    ", unempappr_agg
  do i1=1,NEDU-1
     PRINT '(A40,I4,F14.5)', "Share of crims in edu ", i1,  numcrimtot(i1)/sum(numcrimtot)
     PRINT '(A40,I4,F14.5)', "Arrest rate in edu    ", i1,  numapprtot(i1)/numworktot(i1)
  enddo

  PRINT '(A40,F14.5)', "Aggregate Prison Costs           ", aggprexp
  print*,'***********************************'

  PRINT '(A60)', ' Av. wealth and ability for Criminals and non Criminals : '

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.2)', ' Crim. HS dropouts aged 1, abil.bin      = ', i2, &
          (sum(AVEwealth(1,1,i2,2:3))+sum(AVEwealth(1,1,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(1,1,2:3,i2,1)+NUMHEADS_AGE(1,1,2:3,i2,2))+&
          sum(NUMHEADS_AGE(1,1,6:ngpst,i2,1)+NUMHEADS_AGE(1,1,6:ngpst,i2,2)))),&
          (sum(AVEabi(1,1,i2,2:3))+sum(AVEabi(1,1,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(1,1,2:3,i2,1)+NUMHEADS_AGE(1,1,2:3,i2,2))+&
          sum(NUMHEADS_AGE(1,1,6:ngpst,i2,1)+NUMHEADS_AGE(1,1,6:ngpst,i2,2))))
  enddo

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.2)', ' Non Crim. HS dropouts aged 1, abil.bin  = ', i2, &
          sum(AVEwealth(1,1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(1,1,4:5,i2,1))),&
          sum(AVEabi(1,1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(1,1,4:5,i2,1)))
  enddo

  print*,'**********'

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.3)', ' Crim. HS grads aged 3, abil.bin         = ', i2, &
          (sum(AVEwealth(2,3,i2,2:3))+sum(AVEwealth(2,3,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(2,3,2:3,i2,1)+NUMHEADS_AGE(2,3,2:3,i2,2))+&
          sum(NUMHEADS_AGE(2,3,6:ngpst,i2,1)+NUMHEADS_AGE(2,3,6:ngpst,i2,2)))),&
          (sum(AVEabi(2,3,i2,2:3))+sum(AVEabi(2,3,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(2,3,2:3,i2,1)+NUMHEADS_AGE(2,3,2:3,i2,2))+&
          sum(NUMHEADS_AGE(2,3,6:ngpst,i2,1)+NUMHEADS_AGE(2,3,6:ngpst,i2,2))))
  enddo

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.3)', ' Non Crim. HS grads aged 3, abil.bin     = ', i2, &
          sum(AVEwealth(2,3,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(2,3,4:5,i2,1))),&
          sum(AVEabi(2,3,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(2,3,4:5,i2,1)))
  enddo

  print*,'**********'

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.3)', ' Crim. Coll. grads at age 7, abil.bin    = ', i2, &
          (sum(AVEwealth(3,sum(eduyr)+1,i2,2:3))+sum(AVEwealth(3,sum(eduyr)+1,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(3,sum(eduyr)+1,2:3,i2,1)+NUMHEADS_AGE(3,sum(eduyr)+1,2:3,i2,2))+&
          sum(NUMHEADS_AGE(3,sum(eduyr)+1,6:ngpst,i2,1)+NUMHEADS_AGE(3,sum(eduyr)+1,6:ngpst,i2,2)))),&
          (sum(AVEabi(3,sum(eduyr)+1,i2,2:3))+sum(AVEabi(3,sum(eduyr)+1,i2,6:ngpst)))&
          /max(1.d0,(sum(NUMHEADS_AGE(3,sum(eduyr)+1,2:3,i2,1)+NUMHEADS_AGE(3,sum(eduyr)+1,2:3,i2,2))+&
          sum(NUMHEADS_AGE(3,sum(eduyr)+1,6:ngpst,i2,1)+NUMHEADS_AGE(3,sum(eduyr)+1,6:ngpst,i2,2))))
  enddo

  do i2=1,ngpf-1
     PRINT '(A40,I4,TR2,F6.3,TR2,F6.3)', ' Non Crim. Coll. grads aged 7, abil.bin  = ', i2, &
          sum(AVEwealth(3,sum(eduyr)+1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(3,sum(eduyr)+1,4:5,i2,1))),&
          sum(AVEabi(3,sum(eduyr)+1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(3,sum(eduyr)+1,4:5,i2,1)))
  enddo

  print *, '	'

  CONTINUE

  ! **** UPDATE AGGREGATE VICTIMIZATION RATE ****
  PI_V(1) = VICTIM_AGG
  PI_V(2) = 1.d0-VICTIM_AGG

  ! SAVE: Crime rates
  series1='CRIMEbyAGE.txt'
  CALL save_vector(series1,40,size(ccrime_intabedu),ccrime_intabedu) ! Crime rates by age
  series1='CRIMEbyEDU.txt'
  CALL save_vector(series1,40,size(ccrime_intabage),ccrime_intabage) ! Crime rates by edu
  series1='CRIMEbyABI.txt'
  CALL save_vector(series1,40,size(ccrime_intagedu),ccrime_intagedu) ! Crime rates by ability
  series1='criminal_byabi.txt'
  CALL save_vector(series1,40,size(criminal_byabi),criminal_byabi) ! Crime rates by ability


  series1='CRIMEbyAGE_variousedu.txt'
  CALL save_matrix(series1,40,size(ccrime_intab(:,1)),size(ccrime_intab(1,:)),ccrime_intab) ! Crime rates by age

  series1='CRIMEbyABI_variousedu.txt'
  CALL save_matrix(series1,40,size(ccrime_intage(:,1)),size(ccrime_intage(1,:)),ccrime_intage) ! Crime rates by edu and ability bins

  series1='CRIMEbyCGRID_ABI_edu.txt'
  CALL save_matrix_2(series1,40,size(ccrime_cgrid(:,1,1)),size(ccrime_cgrid(1,:,1)),&
       size(ccrime_cgrid(1,1,:)),ccrime_cgrid) ! Crime rates by edu and ability bins

  series1='workersbyCGRID_ABI_edu.txt'
  CALL save_matrix_2(series1,40,size(selec_cgrid(:,1,1)),size(selec_cgrid(1,:,1)),&
       size(selec_cgrid(1,1,:)),selec_cgrid) ! Crime rates by edu and ability bins


  series1='CRIMINALSbyABI_variousedu.txt'
  CALL save_matrix(series1,40,size(numcriminal_abiedu(:,1)),size(numcriminal_abiedu(1,:)),numcriminal_abiedu) ! Crime rates by edu and ability bins

  series1='arrested-by-age.txt'
  CALL save_vector(series1,40,retage-1,numapprage)
  series1='pop-by-age.txt'
  CALL save_vector(series1,40,retage-1,popsize(1:retage-1))

  CONTINUE


  continue

  SELECT CASE (techoice)
  CASE (0) ! SIMPLE CES

     MRHC(1)	= (1.d0-taxn)*prodout*(1.d0-theta)*sh(1)*(HC(1)**(jamma-1.d0))*(NN**(-jamma))
     MRHC(2)	= (1.d0-taxn)*prodout*(1.d0-theta)*sh(2)*(HC(2)**(jamma-1.d0))*(NN**(-jamma))
     MRHC(NEDU)= (1.d0-taxn)*prodout*(1.d0-theta)*sh(NEDU)*(HC(NEDU)**(jamma-1.d0))*(NN**(-jamma))
  CASE (1) ! COBB DOUGLAS
     MRHC(1)	= (1.d0-taxn)* prodout*(1.d0-theta)*sh(1)/HC(1)
     MRHC(2)	= (1.d0-taxn)* prodout*(1.d0-theta)*sh(2)/HC(2)
     MRHC(NEDU)= (1.d0-taxn)*prodout*(1.d0-theta)*sh(NEDU)/HC(NEDU)
  END SELECT


  ! INTEREST RATE - updated with marginal return on K or left fixed
  MRK = (1.d0-taxk) * ((theta * prodout /  PhK) - delta)	


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  aggwealth1 = aggwealth
  agginc1  =  agginc


  CALL save_output(wealthzero,wealthzeroW,aggwealth,avwealth35,avgconspreret,agginc,bcpeople,avgwage,avgearn,&
       avgabil,transfshare,gaggtransf,avtransfrec,transfexp,taxrev,Gexp,aggprexp,avgconspret_abedu,GRPincw,numwork,condedu,numabil,&
       UNcondedu,edudist,ccrime_agg,victim_agg,teenjailed_agg,LTHSjailed_agg,recidiv_agg,&
       unempappr_agg,numcrimtot,numapprtot,numworktot,edufrac,avgwageABEDU,NUMHEADS,NUMHEADS_AGE,avabibyedu,recidiv_agg2,int_cstat,meanwage,medwage,varwage,wage10,wage90,&
       topcode_loc)

  print*, 'forcepol', forcepol
  time2=rtc( )
  delta_t=time2+time0-2.0d0*time1
  print *, 'Execution of Simul terminated' ;	print *, ""
  print *, 'Elapsed time:' ,	delta_t

  !***********************************************************************
  IF (calibselect==1) then 
  ! Assign variables to array of calibration moments

  moms(1) = wealthzero ! wealthzero - share of agents with wealth <=0
  moms(2) = aggwealth/agginc ! wealth/income ratio
  moms(5) = victim_agg ! victim_agg - aggregate victimization rate
  moms(6) = LTHSjailed_agg ! LTHSjailed_agg - share of prisoners with LTHS
  moms(7) = recidiv_agg ! recidiv_agg - share of recidivists out of apprehended 
  moms(8) = coefreg(2)*100  ! education coefficient X 100
  moms(9) = coefreg(3)*100  ! ability coefficient X 100
  moms(10) = coefreg(4)*100 ! linear age coefficient X 100
    
  ! The above is the ratio of the average bequest in the fourth parental income quartile
  ! to the average bequest
  moms(12) = coefreg(1)*100 ! regression constant X 100
  
  call cal_moments
  call save_calib_params
  endif

  

1001 continue


End Subroutine Simul
