MODULE READ_parameters
! Reads and writes parameters and guesses 
	USE global1
	USE global2
	USE procs
	
	IMPLICIT NONE
	SAVE

!**********************************************************************
								CONTAINS
!**********************************************************************

	SUBROUTINE READPARAMETERS
	! Local
	INTEGER :: local1, i1,j
	CHARACTER ( len = 100 ) :: command
	INTEGER :: status
	INTEGER :: system
    REAL(long), DIMENSION(nedu) :: shlocal  
    
    
	!*** Read Parameters ***
    	OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"fixed_params.txt",action='read',position='REWIND')	

		READ(38,*) endog_lab ! set=1 if running model with endogenous labor choice
		READ(38,*) geneq	 ! set=1 if solving for the general equilibrium, =0 if running simulations with given parameters
        READ(38,*) pensind	 ! set=1 if exogenous pension replacement rate, =0 if exogenous pension and endogenous replacement rate
		READ(38,*) natbc	 ! set=1 if solving with natural borrowing constraint, =0 if exogenous borrowing constraint
		READ(38,*) ann	 ! share of wealth that is annuitized		
		READ(38,*) errcheck	 ! set=1 if you want to assess the size of errors in the decision rule
		READ(38,*) zeronw	 ! set=1 for zero net wealth (interest rates and wages exogenous)
		READ(38,*) polcosting !set=0 if edu subs. with no additional taxes, =1 if paid thru addit.labor taxes,=2 if paid thru add. capital taxes
		READ(38,*) y_wealth	 ! set=1 for exogenous initial wealth distribution when young, 0 for endogenous
		READ(38,*) xinterp	 ! set=1 for interpolation on consumption, 0 for interpolation on savings
		READ(38,*) techoice	 ! set=1 for C.D., =0 for non-nested CES
		READ(38,*) fixind	 ! set=1 if discrete ability bins, set=2 for continuous ability distribution
		READ(38,*) tfpnorm	 ! set=1 if Constanta (normalization factor) is set to normalize wage2
		READ(38,*) ucost	 ! set=1 if utility cost is being update, 0 if constant to given level
		READ(38,*) subregime ! set=0 no subs, 1 random subs
		READ(38,*) forcepol  ! set=0 voluntary takeup, =1 compulsory takeup for ALL treated
		READ(38,*) forceHS1  ! set=0 if compulsory HS is up to age 16 (standard case), =1 if compulsory HS is extended by ONE year only (to age 17)
		READ(38,*) unc_qop   ! set=1 if unconditional avg wealth transfer of same size as QOP, =0 if not
		READ(38,*) propsub ! set=0 no subs, 1 random subs to 35%
		READ(38,*) tuitquant ! set=0 tuition determined as %, 1 fixed value
		READ(38,*) cbarflag  ! set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
		READ(38,*) emmeflag  ! flag for emme. set=0 in experiments (cbar constant), set =1 in benchmark 
		READ(38,*) constinwealth    ! set=0 if refreshing the initial wealth distribution, set =1 when wealthi_age1 constant
		READ(38,*) scalefgrid	! factor rescaling size of fixed effects
		READ(38,*) scalewage	! factor scaling the wage we want to normalize to a given value equal to scalewage
		READ(38,*) chi_l        ! Lower support of crime fixed effect   
		READ(38,*) chi_h        ! Upper support of crime fixed effect 
		READ(38,*) s_alpha      ! Alpha parameter for beta distribution for crime fixed effect
		READ(38,*) s_beta       ! Beta parameter for beta distribution for crime fixed effect
		READ(38,*) prisT		! Prison term lenght (integer periods)
		READ(38,*) prisTres		! Additional fraction of prison term shorter than a year (measured in periods)
		READ(38,*) clow     ! Lower bound for consumption
		READ(38,*) inada    ! Lower bound for period utility
		READ(38,*) inaderiv ! Lower bound for marginal utility
		READ(38,*) cbarshare	! Consumption by inmates while in prison as a share of uncavgearn
		READ(38,*) ni		! = 0.35d0
		READ(38,*) sigma	! = 1.5d0
		READ(38,*) psi		! NOT USED
		READ(38,*) gamma_1	! weight of warm glow bequest motive
		READ(38,*) gamma_2	! intercept of warm glow bequest motive
		READ(38,*) eta		! curvature of warm glow bequest motive
		READ(38,*) taxn		! = 0.27d0
		READ(38,*) taxk		! = 0.40d0
		READ(38,*) pensreprate  ! = 0.2
		READ(38,*) eduyr(1) ! = 0
		READ(38,*) eduyr(2) ! = 2
		READ(38,*) eduyr(3) ! = 3
		READ(38,*) schshare(1) ! = 1
		READ(38,*) schshare(2) ! = 1
		READ(38,*) schshare(3) ! = 0.3
		READ(38,*) abweight(1)	! = .32d0
		READ(38,*) abweight(2)	! = .35d0
		READ(38,*) abweight(3)	! = .0d0
		READ(38,*) sh(1)	! share in CES for LTHS
		READ(38,*) sh(2)	! share in CES for HSG
		READ(38,*) sh(3)	! share in CES for CG
        READ(38,*) corrind	! = linear correlation in bivariate normal copula
		READ(38,*) ntar		! number of calibration targets
		READ(38,*) wpercent	! = wealth percentile used as threshold in means tested experiments
        READ(38,*) pctprice	! = % update in price during search for market clearing
        READ(38,*) pctint	! = % update in interest rate during search for market clearing
        READ(38,*) pctval	! = % update in quasilinear utility (aka LEISURE) for students
        READ(38,*) TOLPR	! = TOLERANCE (%) FOR HC PRICE CONVERGENCE
        READ(38,*) TOLIR	! = TOLERANCE (%) FOR CAPITAL PRICE (INTEREST RATE) CONVERGENCE
        READ(38,*) TOLHC	! = TOLERANCE (%) FOR HC AGGREGATES' CONVERGENCE

	CLOSE(unit=38)

    chi_h = chi_l + 1.d0
    
    CONTINUE

 	END SUBROUTINE READPARAMETERS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE READHETEROGENEITY
	
    ! Local
	INTEGER :: local1,LOCAL2, i, j, l
	CHARACTER ( len = 100 ) :: command
	INTEGER :: status
	INTEGER :: system

	! Variables used to bump-up the shares of enrolment (to match aggregate data)
	REAL(long) :: oldenrol(ngpf-1,nedu), oldtotenrol(nedu),bup(nedu),wab(ngpf-1),targenrol(nedu)
	REAL(long) :: temp1(ngpf-1),temp2(nedu),temp3, check_sum, prova(5)
	
    ! Earning-shock parameters (pers=rho,vareps=perturbation variance,pprob/qprob=Roewenhorst params)
    ! ZWIDTH=1/2 width of Z-grid, varz=variance of z shock,STEPZ=dist.between grid points
    REAL(long) :: STEPZ(nedu), ZWIDTH(nedu), varz(nedu,1:retage-1),&
    pers(nedu),vareps(nedu),pprob(nedu,1:retage-1),qprob(nedu,1:retage-1)
    real(long) :: ezgrid(ngpz,retage-1,nedu), e2zgrid(ngpz,retage-1,nedu), varzgrid(ngpz,retage-1,nedu)

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Read fixed effects values, age-efficiency processes, surv. probs !
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! fixed effect - the nodes are based on the quintiles of the distribution of log(AFQT89) 
    ! for all sample members in the NLSY79 (as computed in AFQTscores.do).
    ! In between any two nodes there is exactly 20% of the population.
    open (unit=9, file=trim(drive)//trim(path1)//TRIM(path2)//"\ability.txt",action='read',position='REWIND')
    read (9, *) fgrid ; 
    close(unit=9)

    ! Read grid (cgrid) for permanent quasi-linear utility from crime
    cgrid = 0.0d0    

	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"\enrol.txt",action='read',position='REWIND')
	DO i=1,ngpf-1
		DO j=1,nedu
	READ (9, *) enrol(i,j)
		ENDDO
	ENDDO
		DO j=1,nedu
	READ (9, *) totenrol(j)
		ENDDO
	CLOSE(unit=9)

	! Set target aggregate enrolment rates - numbers from CPS data
	! (average split in the workers' population between 1977 and 1983).
    OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"\targenrol.txt",action='read',position='REWIND')
		DO j=1,nedu
    READ (9, *) targenrol(j)
        ENDDO
    CLOSE(unit=9)

	! Gross-up total and ability-specific enrolment to match aggregates that are published by NCEDS or from CPS
	! Save enrolment rates 
	oldenrol=enrol
	oldtotenrol=totenrol

	! Create the ability bins weight
    do i=1,ngpf-1
	wab(i)=1.d0/real(ngpf-1,long)
	enddo

	! Find the bump-up factors (bup)
	bup(1)=targenrol(1)/SUM(oldenrol(:,1)*wab(:))
	bup(3)=targenrol(3)/SUM(oldenrol(:,3)*wab(:))
	! Re-define the ability-specific enrolment rates
	enrol(:,1)=min(1.d0,oldenrol(:,1)*bup(1))
	enrol(:,3)=min(1.d0,oldenrol(:,3)*bup(3))
	enrol(:,2)=1.0d0-enrol(:,1)-enrol(:,3)
	! Check that enrolment rates sum up to one and give the desired aggregate
	PRINT *, ' '
	DO i=1,ngpf-1
	temp1(i)=SUM(enrol(i,:))
	PRINT '(A40,I2,A20,F12.4)', "Share of Edu within Ability Bin ", i," sum up to ", temp1(i)
	ENDDO

	DO J=1,nedu
	totenrol(J)=SUM(wab(:)*enrol(:,J))
	PRINT '(A40,I2,A20,F12.4)', "Aggregate Enrolment in Edu Group       ", J," is equal to ", totenrol(J)
	ENDDO

	temp3=SUM(totenrol(:))
	PRINT '(A40,F12.4)', "Aggregate Enrolment Rates Sum Up to    ", temp3


	! Report enrolment rates adjusted to match aggregate enrolment data

	PRINT *, ' '; PRINT *, ' '
	PRINT '(A60)', "  *******************************************************   "
	PRINT '(A60)', "  Report enrol. rates adjusted to match aggr. enrol. data   "	
	PRINT '(A60)', "  *******************************************************   "
	PRINT *, ' '
	PRINT'(A20,I4)', 'Ability bin ', 1
	DO j=1,3
	PRINT'(A25,I4,F14.7)', ' Share in Edu ', j, enrol(1,j)
	ENDDO
	check_sum = 0.d0
	check_sum = SUM(enrol(1,:))
	PRINT'(A25,F14.7)', ' Shares sum up to ', check_sum

	DO i=1,ngpf-2
	PRINT'(A20,I4)', 'Ability bin ', i+1
	DO j=1,3
	PRINT'(A25,I4,F14.7)', ' Share in Edu ', j, enrol(i+1,j)
	ENDDO
	check_sum = 0.d0
	check_sum = SUM(enrol(i+1,:))
	PRINT'(A25,F14.7)', ' Shares sum up to ', check_sum
	ENDDO
	PRINT *, ' '

	DO j=1,3
	PRINT'(A35,I4,F14.7)', ' Total share of people in Edu ', j, totenrol(j)
	ENDDO
	check_sum = 0.d0
	check_sum = SUM(totenrol(:))
	PRINT'(A25,F14.7)', ' Shares sum up to ', check_sum

	CONTINUE

	!!!!!!!!!!!! Crime Parameters  !!!!!!!!!!!!

	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"crimepar.txt",action='read',position='REWIND')
	DO i=1,5
	READ (9, *) rescale(i)
	ENDDO
    DO j=1,5
	READ (9, *) shrob(j)
	ENDDO
	CLOSE(unit=9)
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Rescale to states stind>5 (i.e to account for different crime intensities). 
! Stind=2,6,8,...(worker,not robbed-committing 0,1,2...crimes) 
! stind=3,7,9,...(worker,robbed-committing 0,1,2...crimes))
IF (ngpst<6) THEN 
CONTINUE
ELSE
DO i=1,ngpst-5
    j=5+i
    ! assign rescale 
    IF (MOD(j,2)==0) THEN   
    rescale(j)=rescale(j-3-i)
        ELSE
    rescale(j)=rescale(j-2-i)
    ENDIF
!   Loot increases linearly in the number of crimes
    l=INT(j/2)
    shrob(j)=(l-1)*shrob(2)
ENDDO     
ENDIF

!!!!!!!!!!!!!!!!!!!!  Apprehension prob !!!!!!!!!!!!!!!!!!!!!!!!!!!!

    PI_A = 0.d0 ! initialize
	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"allprobs.txt",action='read',position='REWIND')

	    READ (9, *) PI_A(1,1)  !read probability of going to jail if one crime is committed
        PI_A(2,1) = 1.d0 - PI_A(1,1)  ! Assign prob. of not being apprehended after 1 crime 	

		IF (maxcrim>1) THEN
            ! Apprehension probability linear in number of crimes
 		    DO i=2,maxcrim
		    PI_A(1,i) = i*PI_A(1,1)  ! prob. of going to jail if more than one crime is committed
            PI_A(2,i) = 1.d0 - PI_A(1,i)    ! Complement: prob. of not going to jail (conditional on i) 
		    ENDDO
		ENDIF

    CLOSE (unit=9)



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !      READ PARAMETERS OF AR(1) PROCESS
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(experiment)//"AR1.txt",action='read',position='REWIND')
	    DO local1=1,NEDU
		    READ (9, *) pers(local1)   ! AR(1) CORRELATION COEFFICIENT
		    READ (9, *) vareps(local1) ! VARIANCE OF PERTURBATION SHOCKS IN AR(1)
		ENDDO
    CLOSE(unit=9)

    ! CONSTRUCT GRID OF Z-SHOCKS - USING ROUWENHORST(95)
    ! Grid with 'age-dependent' conditional variance
    
    ! Compute age-specific cross-sectional variances
    do local1=1,NEDU
    ! initial condition for cross-sectional variance
    varz(local1,1) = vareps(local1)   ! Start from shock distribution 
    ZWIDTH(local1)=((NGPZ-1)*varz(local1,1))**0.5D0
    STEPZ(local1)=2*ZWIDTH(local1)/(NGPZ-1)
    ! STEP 2: COMPUTE GRID FOR Z SHOCKS AND DENOTE IT AS ZGRID(NEDU,NGPZ,LIFET)
    ZGRID(1,1,local1)=-ZWIDTH(local1)
    ZGRID(NGPZ,1,local1)=ZWIDTH(local1)
    DO local2=2,NGPZ-1
        ZGRID(local2,1,local1)=ZGRID(local2-1,1,local1)+STEPZ(local1)
    ENDDO
    ! Transition matrix at age 1.
    ! If the initial income draw is equal to the shock then activate the following line
    pprob(local1,1) = 0.5
    qprob(local1,1)=pprob(local1,1)
    ! COMPUTE TRANSITION MATRIX
    CALL ztransit(ngpz,pprob(local1,1),qprob(local1,1),ztrans(:,:,1,local1))
        do i=2,retage-1  ! Working ages
        varz(local1,i)=pers(local1)**2*varz(local1,i-1)+vareps(local1)
        ! Compute width and gridpoints of z-grid
        ! STEP 1 
            ZWIDTH(local1)=((NGPZ-1)*varz(local1,i))**0.5D0
            STEPZ(local1)=2*ZWIDTH(local1)/(NGPZ-1)
            ! STEP 2: COMPUTE GRID FOR Z SHOCKS AND DENOTE IT AS ZGRID(NEDU,NGPZ,LIFET)
            ZGRID(1,i,local1)=-ZWIDTH(local1)
            ZGRID(NGPZ,i,local1)=ZWIDTH(local1)
            DO local2=2,NGPZ-1
                ZGRID(local2,i,local1)=ZGRID(local2-1,i,local1)+STEPZ(local1)
            ENDDO
            ! COMPUTE   PPROB AND QPROB FOR EACH AGE: chosen to match one-step-ahead 
            ! conditional expectation (pers*z=conditional expectation)
            pprob(local1,i)=0.5d0*(1.d0+pers(local1)*(varz(local1,i-1)/varz(local1,i))**0.5d0)
            qprob(local1,i)=pprob(local1,i)
            ! COMPUTE TRANSITION MATRIX
            CALL ztransit(ngpz,pprob(local1,i),qprob(local1,i),ztrans(:,:,i,local1))
            ezgrid(:,i,local1) = pers(local1)*zgrid(:,i-1,local1)
            do local2 = 1,ngpz
            e2zgrid(local2,i,local1) = dot_product(zgrid(:,i,local1),ztrans(local2,:,i,local1) )
            varzgrid(local2,i,local1) = dot_product((zgrid(:,i,local1)-ezgrid(local2,i,local1))**2,ztrans(local2,:,i,local1) )
            enddo 
        ENDDO
        ! Compute the stationary z distribution (only in the stationary case)
         !call D_EVCRG(ztrans(:,:,1,local1),zeval,zevec)
         zstat(:,local1) = 0.d0
         zstat(3,local1) = 1.d0
         do i = 1,1000
            zstat(:,local1) = matmul(transpose(ztrans(:,:,1,local1)),zstat(:,local1))
         enddo
         
    enddo   ! local1



	!!!!!!!!!!!!  Assign HOURS !!!!!!!!!!!!!!!!!

    ! Set hours for person who suffers i_zet shock and has jail==1 (out of jail)
    DO local1=1,NEDU
        do local2=1,retage-1
		hour(local2,:,1,local1)=exp(zgrid(:,local2,local1))
        enddo
    ENDDO

! Assign values for "hour" to people with jail==2 (in jail)
hour(:,:,2,:) = 0.d0

! Assign hours to states jail>2 (jail indexes different taste shocks)
! Hours are the same for any jail=odd (out of jail) 
IF (maxjail>2) THEN
   DO j=3,maxjail
      IF  (MOD(j,2)==1) THEN
          hour(:,:,j,:)=hour(:,:,1,:)
      ELSE
          hour(:,:,j,:)=hour(:,:,2,:)      
      ENDIF
   ENDDO
ENDIF

   
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Rescale FGRID - to generate more heterogeneity  ! 
!	fgrid  = fgrid * scalefgrid 
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! survival probabilities between age 1 and 90
	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"surv1to110.txt",action='read',position='REWIND')
	READ (9, *) morprobr ; CLOSE(unit=9)

	! age-efficiency factors estimated from PSID by education for age 25 and 60
	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"eff_lths.txt",action='read',position='REWIND')
	READ (9, *) eff(1,1:oldest-youngest+1) ; CLOSE(unit=9)	

	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"eff_hsg.txt",action='read',position='REWIND')
	READ (9, *) eff(2,1:oldest-youngest+1) ; CLOSE(unit=9)	

	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"eff_cg.txt",action='read',position='REWIND')
	READ (9, *) eff(3,1:oldest-youngest+1) ; CLOSE(unit=9)


	END SUBROUTINE READHETEROGENEITY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	SUBROUTINE save_leisure
	! SAVE LEISURE_SAVE(i)
	INTEGER :: i

	OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"leisure.txt",action='write',position='REWIND')	
    
    DO i=1,2*ngpf-2
		WRITE(38,*) leisure_save(i)	, "   ! leisure_save(", i,")"
    ENDDO
		WRITE(38,*), ' '
	CLOSE(unit=38)

    END	SUBROUTINE save_leisure

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE save_params
	! SAVE EQUILIUBRIUM PARAMETERS
	INTEGER :: i


	OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"in_conditions.txt",action='write',position='REWIND')	

		WRITE(38,*) HC(1)	, "   ! HC(1) "	
		WRITE(38,*) HC(2)	, "   ! HC(2) "
		WRITE(38,*) HC(3)	, "   ! HC(3) "
		WRITE(38,*) rrate	, "   ! rrate "
		WRITE(38,*) wmin	, "   ! wmin  "
		WRITE(38,*) bumpup	, "   ! bumpup"
		WRITE(38,*) wmax0	, "   ! wmax  "
		WRITE(38,*) bita	, "   ! bita  "
		WRITE(38,*) prodout	, "   ! prodout"
        WRITE(38,*) Gexp    , "   ! Government resid. expenditure"
        WRITE(38,*) Transfexp   , "   ! Government Edu. subsidy expenditure"
        WRITE(38,*) Aggprexp    , "   ! Total prison expenditure"
		WRITE(38,*) wagetge(1)	, "   ! wagetge(1)"
		WRITE(38,*) wagetge(2)	, "   ! wagetge(2)"
		WRITE(38,*) wagetge(3)	, "   ! wagetge(3)"
		WRITE(38,*) constanta	, "   ! constanta "
		WRITE(38,*) avwealth	, "   ! avwealth"
		WRITE(38,*) medwealth	, "   ! medwealth"
		WRITE(38,*) medinc		, "   ! medinc"
		WRITE(38,*) UNCavgearn	, "   ! UNCavgearn"
		WRITE(38,*) emme		, "   ! emme-offsets cbar to match tot.cost per prisoner"
		WRITE(38,*) penv(retage), "   ! penv(retage)"
		WRITE(38,*) avinc		, "   ! avinc"
		WRITE(38,*) meanstestw1		, "   ! meanstestw1"
		WRITE(38,*) vardrawHS       , "   ! NOT USED - Set to zero"
		WRITE(38,*) vardrawC        , "   ! NOT USED - Set to zero"
		WRITE(38,*) PI_V(1)	        , "   ! Victim_agg - Victimization rate "
		WRITE(38,*), ' '
	CLOSE(unit=38)


	END SUBROUTINE save_params

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	SUBROUTINE initial_conditions

	INTEGER :: i

	OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"in_conditions.txt",action='read',position='REWIND')	
		READ(38,*) HC(1)			! = 1.2
		READ(38,*) HC(2)			! = 3
		READ(38,*) HC(3)			! = 1.2
		READ(38,*) irate			! = 0.04
		READ(38,*) wmin				! = - .15
		READ(38,*) bumpup			! rescaling factor for students borrowing constraint
		READ(38,*) wmax				! = 30
		READ(38,*) bita				! = 0.9995d0
		READ(38,*) prodout			! = 11.d0
        READ(38,*) Gexp             ! Government resid. expenditure
        READ(38,*) Transfexp        ! Government Edu. subsidy expenditure
        READ(38,*) Aggprexp			! Total prison expenditure
		READ(38,*) wagetge(1)		! = 0.7d0
		READ(38,*) wagetge(2)		! = 1.0d0
		READ(38,*) wagetge(3)		! = 1.4d0
		READ(38,*) constanta		! TFP factor used to normalize wage2
		READ(38,*) avwealth			! = .5d0
		READ(38,*) medwealth		! = .5d0
		READ(38,*) medinc			! = .5d0
		READ(38,*) UNCavgearn		! UNCavgearn
		READ(38,*) emme             ! = .53*UNCavgearn - cbar - term offsetting cbar to match tot.cost per prisoner	
		READ(38,*) penv(retage)		! penv(retage)
		READ(38,*) avinc			! avinc
		READ(38,*) meanstestw1			! means test wealth threshold
		READ(38,*) vardrawHS            ! NOT USED - Set to zero"
		READ(38,*) vardrawC             ! NOT USED - Set to zero"
		READ(38,*) PI_V(1)	            ! Victim_agg - Victimization rate "

	CLOSE(unit=38)

    ! ASSIGN PI_V(2)
    PI_V(2) = 1.d0 - PI_V(1)

	! Tuition File
	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"tuitions.txt",action='read',position='REWIND')
	READ (9, *) tuit(1)		! tuit(1)
	READ (9, *) tuit(2)		! tuit(2)
	CLOSE(unit=9) 


	! Read file of initial leisure_save	
	OPEN (unit=9, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"leisure.txt",action='read',position='REWIND')
    DO i=1,2*ngpf-2
		READ(9,*) leisure_save(i)     ! leisure_save(i)
    ENDDO
	CLOSE(unit=9) 

	END SUBROUTINE initial_conditions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE save_output(wealthzero,wealthzeroW,aggwealth,avwealth35,avgconspreret,agginc,bcpeople,avgwage,avgearn,&
    avgabil,transfshare,gaggtransf,avtransfrec,transfexp,taxrev,Gexp,aggprexp,avgconspret_abedu,GRPincw,numwork,condedu,numabil,&
    UNcondedu,edudist,ccrime_agg,victim_agg,teenjailed_agg,LTHSjailed_agg,recidiv_agg,&
    unempappr_agg,numcrimtot,numapprtot,numworktot,edufrac,avgwageABEDU,numheads,numheads_age,avabibyedu,recidiv_agg2,int_cstat,&
    meanwage,medwage,varwage,wage10,wage90,topcode_loc)

    ! SAVE RESULTS SCREEN

	REAL(long), INTENT(in) :: wealthzero,wealthzeroW,aggwealth,avwealth35,agginc,ccrime_agg,victim_agg,&
	recidiv_agg2
	REAL(long), INTENT(in) :: bcpeople(ngpst,lifet,nedu),avgwage(nedu,ngpst),avgearn(nedu,ngpst),avgabil(nedu,ngpst),&
	transfshare(nedu,lifet,ngpst),gaggtransf(nedu,ngpst),avtransfrec(nedu,ngpst),transfexp,taxrev,Gexp,aggprexp,&
	grpincW(nedu),condedu(nedu,ngpf-1),numabil(ngpf-1),UNcondedu(nedu),avgconspret_abedu(nedu,ngpf-1,4),numwork(nedu,lifet,ngpf-1),&
	edudist(nedu,lifet,ngpf-1),avgconspreret(nedu,ngpst),numcrimtot(nedu),numapprtot(nedu),numworktot(nedu),teenjailed_agg,LTHSjailed_agg,recidiv_agg,&
    unempappr_agg, edufrac(nedu,lifet,ngpst),avgwageABEDU(nedu,ngpf-1),	&
    numheads_age(nedu,lifet,ngpst,ngpf-1,2),numheads(nedu,ngpst,ngpf-1,2),avabibyedu(nedu),int_cstat(nedu,maxcrim),&
    meanwage,medwage,varwage,wage10,wage90
    integer, intent(in)   :: topcode_loc




    ! Locals
    INTEGER :: i,j,i1,i2
    real(long) :: tempvar, checkvar

    OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND')  

		WRITE(38,*),' '
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), " Interest rate after tax and depr          ", rrate
		WRITE (38,'(A40,F14.5)'), " MRK after tax and depr                    ", MRK
		WRITE (38,'(A40,F14.5)'), " Discount factor                           ", bita
		WRITE (38,'(A40,F14.5)'), " Borrowing constraint                      ", wmin
		WRITE (38,'(A40,F14.5)'), " % of total popn with wealth <= 0          ", wealthzero
		WRITE (38,'(A40,F14.5)'), " % of NON studentl popn with wealth <= 0   ", wealthzeroW
		IF (agginc>0.d0) THEN
		WRITE (38,'(A40,F14.5)'), "Wealth/Income                    ", aggwealth/agginc
		ENDIF
		WRITE (38,'(A40,F14.5)'), "Average  wealth                  ", avwealth
		WRITE (38,'(A40,F14.5)'), "Av. wealth under 35 / Av. wealth ", avwealth35/avwealth	
		WRITE (38,'(A40,F14.5)'), "SD wealth                        ", varwealth**.5d0
		WRITE (38,'(A40,F14.5)'), "Median   wealth                  ", medwealth
		WRITE (38,'(A40,F14.5)'), "Lowest   wealth                  ", MINVAL(wdist_2)
		WRITE (38,'(A40,F14.5)'), "Highest  wealth                  ", MAXVAL(wdist_2)
		WRITE (38,'(A40,F14.5)'), "Share of students on B.C., edu 1 ", bcpeople(1,1,1) 
		WRITE (38,'(A40,F14.5)'), "Share of students on B.C., edu 2 ", bcpeople(1,3,2) 
		WRITE (38,'(A40,i14)'),   "Index of first top-coded guy     ", topcode_loc 
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Average  wage                    ", meanwage
		WRITE (38,'(A40,F14.5)'), "SD wage                          ", varwage**.5d0
		WRITE (38,'(A40,F14.5)'), "Median   wage                    ", medwage
		WRITE (38,'(A40,F14.5)'), "10 Percentile wage               ", wage10
		WRITE (38,'(A40,F14.5)'), "90 Percentile wage               ", wage90
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Aggregate wealth                 ", aggwealth
		WRITE (38,'(A40,F14.5)'), "Aggregate capital stock          ", NN*((rrate/(1-taxk)+delta)/(theta*constanta))**(1.d0/(theta-1.d0))
		WRITE (38,'(A40,F14.5)'), "Aggregate labor                  ", NN
		WRITE (38,'(A40,F14.5)'), "Aggregate output                 ", PRODOUT
		WRITE (38,'(A40,F14.5)'), "Net output                       ", &
			PRODOUT - delta*phK - Gexp - aggprexp
		WRITE (38,'(A40,F14.5)'), "Normalization Factor - Constanta ", constanta

		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Av. consumption of students 1    ", avgconspreret(1,1)/max(1.d0,SUM(NUMHEADS(1,1,:,1)))
		WRITE (38,'(A40,F14.5)'), "Av. consumption of students 2    ", avgconspreret(2,1)/max(1.d0,SUM(NUMHEADS(2,1,:,1)))
		WRITE(38,*),'***********************************'
		WRITE (38,'(A60)'), ' Av. wealth and ability for HS stud. and working HS dropouts : '
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' HS students, abil.bin      = ', i2, &
		AVEwealth(1,1,i2,1)/MAX(1.D0,NUMHEADS_AGE(1,1,1,i2,1)), AVEabi(1,1,i2,1)/MAX(1.D0,NUMHEADS_AGE(1,1,1,i2,1))
		ENDDO
		DO i2=1,ngpf-1

		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' HS dropouts, abil.bin = ', i2, &
		SUM(AVEwealth(1,1,i2,2:ngpst))/MAX(1.D0,SUM(NUMHEADS_AGE(1,1,2:ngpst,i2,1)+&
		NUMHEADS_AGE(1,1,2:ngpst,i2,2))),&

		SUM(AVEabi(1,1,i2,2:ngpst))/MAX(1.D0,SUM(NUMHEADS_AGE(1,1,2:ngpst,i2,1)+&
		NUMHEADS_AGE(1,1,2:ngpst,i2,2)))

		ENDDO
		WRITE(38,*),'**********'
		WRITE (38,'(A60)'), ' Av. wealth and ability for Coll. stud. and working HS grads. : '
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' College students, abil.bin = ', i2, &
		AVEwealth(2,eduyr(2)+1,i2,1)/MAX(1.D0,NUMHEADS_AGE(2,eduyr(2)+1,1,i2,1)),&
		AVEabi(2,eduyr(2)+1,i2,1)/MAX(1.D0,NUMHEADS_AGE(2,eduyr(2)+1,1,i2,1))
		ENDDO
		DO i2=1,ngpf-1

		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' HS graduates , abil.bin = ', i2, &
		sum(AVEwealth(2,eduyr(2)+1,i2,2:ngpst))&
		/MAX(1.D0,SUM(NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,1)+&
		NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,2))),&

		sum(AVEabi(2,eduyr(2)+1,i2,2:ngpst))&
		/MAX(1.D0,SUM(NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,1)+&
		NUMHEADS_AGE(2,eduyr(2)+1,2:ngpst,i2,2)))

		ENDDO
		WRITE(38,*),'***********************************'
		DO i=1,nedu
		WRITE(38, '(A40,I4)'), ' EDU group = ', i
		    do j=2,ngpst
		WRITE (38,'(A30,I4,F14.5)'), "Av. consumption st_ind =  ", j, avgconspreret(i,j)/max(1.d0,SUM(NUMHEADS(i,j,:,1)))
		    enddo   ! do j=2,ngpst
		WRITE(38,*),'***********'
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F14.5)'), ' Av. cons., st_ind=4,  ability group = ', i2, avgconspret_abedu(i,i2,3)/max(1.d0,numheads(i,4,i2,1))
		ENDDO
		WRITE(38,*),'***********'
        do j=1,ngpf-1
		WRITE (38,'(A8,I3,A8,I3,A8,F10.5)'), 'Edu=', i, 'Ab.Bin=', j, 'Av.Wage',avgwageABEDU(i,j)/max(1.d0,sum(numheads(i,2:ngpst,j,1)))
		enddo
		WRITE(38,*),'***********'
    	    do j=2,ngpst,2
		WRITE (38,'(A30,I4,F14.5)'), "Average earnings for st_ind = ", j, avgearn(i,j)/max(1.d0,sum(numheads(i,j,:,1)))
		WRITE (38,'(A30,I4,F14.5)'), "Average ability for st_ind =  ", j, avgabil(i,j)/max(1.d0,sum(numheads(i,j,:,1)))
		WRITE(38,*),'***********'
    	    enddo   !do j=2,ngpst,2		

        ! Compute average WAGE for each education group    	
    	tempvar=0.d0; checkvar=0.d0
    	do j=2,ngpst
    	tempvar = tempvar + avgwage(I,j)   
        checkvar= checkvar + sum(numheads(I,j,:,1))
    	enddo
    	tempvar = tempvar / checkvar    

		WRITE(38,'(A40,I2,F11.5)'), "Av. post-tax wage rate  - edu ", I, tempvar
		WRITE(38,'(A40,I2,F11.5)'), "Av. post-tax tot.income - edu ", I, grpincw(I)
		WRITE(38,*),'***********************************'
		ENDDO

        do i=1,ngpf-1
		if (numabil(i)>0) then
		WRITE (38,'(A14,I4,A20,F14.5)'), "Ability", i , "in edu 1 ", condedu(1,i)/numabil(i)
		WRITE (38,'(A14,I4,A20,F14.5)'), "Ability", i , "in edu 2 ", condedu(2,i)/numabil(i)
		WRITE (38,'(A14,I4,A20,F14.5)'), "Ability", i , "in edu 3 ", condedu(3,i)/numabil(i)
		endif
		WRITE(38,*), '     *************************'
		enddo


		WRITE (38,'(A40,F14.5)'), "Total share in edu 1             ", UNCONDedu(1)
		WRITE (38,'(A40,F14.5)'), "Total share in edu 2             ", UNCONDedu(2)
		WRITE (38,'(A40,F14.5)'), "Total share in edu 3             ", UNCONDedu(3)
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Average post-tax income - workers", avinc
		WRITE (38,'(A40,F14.5)'), "SD   of post-tax income - workers", varinc**.5d0
		WRITE (38,'(A40,F14.5)'), "Median post-tax income  - workers", medinc
		WRITE (38,'(A40,F14.5)'), "Average labour income   - workers", UNCavgearn
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "HC(1)                            ", HC(1)
		WRITE (38,'(A40,F14.5)'), "HC(2)                            ", HC(2)
		WRITE (38,'(A40,F14.5)'), "HC(3)                            ", HC(3)
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Av. Ability - Edu 1              ", avabibyedu(1)
		WRITE (38,'(A40,F14.5)'), "Av. Ability - Edu 2              ", avabibyedu(2)
		WRITE (38,'(A40,F14.5)'), "Av. Ability - Edu 3              ", avabibyedu(3)
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Total Tax Revenues               ", taxrev
		WRITE (38,'(A40,F14.5)'), "Total Transfer Expenditures      ", transfexp
		WRITE (38,'(A40,F14.5)'), "Total G Expenditure              ", Gexp
		IF (polcosting==1) THEN
		WRITE(38, '(A40,2(tr2,F5.4))'), "Base and augmented lab tax rates  ", taxnbase , taxn
		ELSE IF (polcosting==2) THEN
		WRITE(38, '(A40,2(tr2,F5.4))'), "Base and augmented K tax rates    ", taxkbase , taxk
		ENDIF
		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "High School Tuition cost         ", tuit(1)
		WRITE (38,'(A40,F14.5)'), "University Tuition cost          ", tuit(2)
		IF (SUM(edufrac(2,:,1))>0.d0) THEN
		WRITE (38,'(A40,F14.5)'), "% of Col. stud.s w/ transf       ", SUM(transfshare(2,:,1)*edufrac(2,:,1)*popsize(:))/SUM(edufrac(2,:,1)*popsize(:))
		WRITE (38,'(A40,F14.5)'), "Av. Transf among all Coll. stud. ", gaggtransf(2,1)/SUM(edufrac(2,:,1)*popsize(:))
		WRITE (38,'(A40,F14.5)'), "Av. Transf among recipients only ", avtransfrec(2,1)
		ENDIF

		WRITE(38,*),'***********************************'
		WRITE (38,'(A40,F14.5)'), "Aggregate Crime rate             ", ccrime_agg
		WRITE (38,'(A40,F14.5)'), "Aggregate Victimization rate     ", victim_agg
		WRITE (38,'(A40,F14.5)'), "Share of prisoners age< 19       ", teenjailed_agg
		WRITE (38,'(A40,F14.5)'), "Share of prisoners with LTHS     ", LTHSjailed_agg
		WRITE (38,'(A40,F14.5)'), "Recidivists out of  apprehended  ", recidiv_agg
		WRITE (38,'(A40,F14.5)'), "Recidivists w/in 1 yr of release ", recidiv_agg2
		WRITE (38,'(A40,F14.5)'), "Unemployed out of apprehended    ", unempappr_agg

		do i1=1,NEDU-1
		WRITE (38,'(A40,I4,F14.5)'), "Share of crims in edu ", i1, numcrimtot(i1)/sum(numcrimtot(:))
		WRITE (38,'(A40,I4,F14.5)'), "Arrest rate in edu    ", i1,  numapprtot(i1)/numworktot(i1)
        enddo
		WRITE (38,'(A40,F14.5)'), "Aggregate Prison Costs           ", aggprexp
		WRITE(38,*),'***********************************'

		WRITE (38,'(A60)'), ' Av. wealth and ability for Criminals and non Criminals : '
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Crim. HS dropouts aged 1, abil.bin      = ', i2, &
		(sum(AVEwealth(1,1,i2,2:3))+sum(AVEwealth(1,1,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(1,1,2:3,i2,1)+NUMHEADS_AGE(1,1,2:3,i2,2))+&
		sum(NUMHEADS_AGE(1,1,6:ngpst,i2,1)+NUMHEADS_AGE(1,1,6:ngpst,i2,2)))),&
		(sum(AVEabi(1,1,i2,2:3))+sum(AVEabi(1,1,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(1,1,2:3,i2,1)+NUMHEADS_AGE(1,1,2:3,i2,2))+&
		sum(NUMHEADS_AGE(1,1,6:ngpst,i2,1)+NUMHEADS_AGE(1,1,6:ngpst,i2,2))))
		ENDDO
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Non Crim. HS dropouts aged 1, abil.bin  = ', i2, &
		sum(AVEwealth(1,1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(1,1,4:5,i2,1))),&
		sum(AVEabi(1,1,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(1,1,4:5,i2,1)))
		ENDDO
		WRITE(38,*),'**********'
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Crim. HS grads aged 3, abil.bin         = ', i2, &
		(sum(AVEwealth(2,3,i2,2:3))+sum(AVEwealth(2,3,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(2,3,2:3,i2,1)+NUMHEADS_AGE(2,3,2:3,i2,2))+&
		sum(NUMHEADS_AGE(2,3,6:ngpst,i2,1)+NUMHEADS_AGE(2,3,6:ngpst,i2,2)))),&
		(sum(AVEabi(2,3,i2,2:3))+sum(AVEabi(2,3,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(2,3,2:3,i2,1)+NUMHEADS_AGE(2,3,2:3,i2,2))+&
		sum(NUMHEADS_AGE(2,3,6:ngpst,i2,1)+NUMHEADS_AGE(2,3,6:ngpst,i2,2))))
		ENDDO
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Non Crim. HS grads aged 3, abil.bin     = ', i2, &
		sum(AVEwealth(2,3,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(2,3,4:5,i2,1))),&
		sum(AVEabi(2,3,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(2,3,4:5,i2,1)))
		ENDDO
		WRITE(38,*),'**********'
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Crim. Coll. grads at age 6, abil.bin    = ', i2, &
		(sum(AVEwealth(3,6,i2,2:3))+sum(AVEwealth(3,6,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(3,6,2:3,i2,1)+NUMHEADS_AGE(3,6,2:3,i2,2))+&
		sum(NUMHEADS_AGE(3,6,6:ngpst,i2,1)+NUMHEADS_AGE(3,6,6:ngpst,i2,2)))),&
		(sum(AVEabi(3,6,i2,2:3))+sum(AVEabi(3,6,i2,6:ngpst)))&
		/max(1.d0,(sum(NUMHEADS_AGE(3,6,2:3,i2,1)+NUMHEADS_AGE(3,6,2:3,i2,2))+&
		sum(NUMHEADS_AGE(3,6,6:ngpst,i2,1)+NUMHEADS_AGE(3,6,6:ngpst,i2,2))))
		ENDDO
		DO i2=1,ngpf-1
		WRITE (38,'(A40,I4,F10.4,TR2,F10.4)'), ' Non Crim. Coll. grads aged 6, abil.bin  = ', i2, &
		sum(AVEwealth(3,6,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(3,6,4:5,i2,1))),&
		sum(AVEabi(3,6,i2,4:5))/max(1.d0,sum(NUMHEADS_AGE(3,6,4:5,i2,1)))
		ENDDO
		WRITE(38, *), '	'
		do i2=1,nedu
		WRITE (38,'(A20,I4)'), ' Edu = ', i2
		do i=1,maxcrim
            if (sum(int_cstat(i2,:))>0.d0) then
			WRITE (38,'(A40,I4,A8,TR2,F10.4)'), ' Proportion of criminals committing ', i, 'crimes', &
			int_cstat(i2,i)/sum(int_cstat(i2,:))
			else
            WRITE (38,'(A40,I4,A8,TR2,F10.4)'), ' Proportion of criminals committing ', i, 'crimes', &
            0.d0
            endif
		enddo
		enddo
	CONTINUE

      CLOSE(unit=38)                                                                    


    OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"select_output.txt",action='write',position='REWIND')  
  	WRITE (38,'(A40,F14.5)'), "Gross output                     ", PRODOUT 
   	WRITE (38,'(A40,F14.5)'), "Net output                       ", &
			PRODOUT - delta*phK - Gexp - aggprexp
    WRITE (38,'(A40,F14.5)'), "Average  wage                    ", meanwage
	WRITE (38,'(A40,F14.5)'), "10 Percentile wage               ", wage10
	WRITE (38,'(A40,F14.5)'), "90 Percentile wage               ", wage90
    WRITE (38,'(A40,F14.5)'), "Aggregate Victimization rate     ", victim_agg
	WRITE (38,'(A40,F14.5)'), "Aggregate Crime rate             ", ccrime_agg
    WRITE (38,'(A40,F14.5)'), "Aggregate Prison Costs           ", aggprexp
    
    CLOSE(unit=38)                                                                    

    END SUBROUTINE save_output

!****************************************************************
    
    SUBROUTINE cal_moments
 
    open (unit=38, file=trim(drive)//TRIM(path1)//TRIM(path2)//"modelmoments.txt",action='write',position='REWIND')    

    write(38,*) moms(1)
    write(38,*) moms(2)
    write(38,*) moms(3)
    write(38,*) moms(4)
    write(38,*) moms(5)
    write(38,*) moms(6)
    write(38,*) moms(7)
    write(38,*) moms(8)
    write(38,*) moms(9)
    write(38,*) moms(10)
    write(38,*) moms(11)
    write(38,*) moms(12)
    close(38)

    

    end subroutine cal_moments

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 SUBROUTINE read_calib_params
    
    open (unit=38,  file=trim(drive)//TRIM(path1)//TRIM(path2)//"calib_params.txt",action='read',position='REWIND')
    
    !open (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"calib_params.txt",action='read',position='REWIND')
    read(38,*) wmin
    read(38,*) bita
    read(38,*) gamma_1
    read(38,*) gamma_2
    read(38,*) eta
    read(38,*) cbarshare
    read(38,*) chi_l
    read(38,*) chi_h
    read(38,*) s_alpha 
    read(38,*) s_beta
    read(38,*) corrind
    ! read(38,*) ! The last line in calib_params.txt is not read
    close(38)
    
    chi_h = chi_l + 1.d0
   
    
    end subroutine read_calib_params  

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 SUBROUTINE save_calib_params
    
    open (unit=38,  file=trim(drive)//TRIM(path1)//TRIM(path2)//"calib_params_bench.txt",action='write',position='REWIND')
    
    write(38,*) wmin
    write(38,*) bita
    write(38,*) gamma_1
    write(38,*) gamma_2
    write(38,*) eta
    write(38,*) cbarshare
    write(38,*) chi_l
    write(38,*) chi_h
    write(38,*) s_alpha 
    write(38,*) s_beta
    write(38,*) corrind
    WRITE(38,*) 0.d0  ! NOT USED
    close(38)
    
    end subroutine save_calib_params  


END MODULE READ_parameters
