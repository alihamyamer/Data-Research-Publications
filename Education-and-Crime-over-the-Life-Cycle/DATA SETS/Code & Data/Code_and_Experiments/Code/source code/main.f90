subroutine  SchoolingOLG
! Main subroutine, reading parameters and calling subroutine that  computes steady state equilibrium
	use global1
	use global2
	use global3
	use read_parameters
	use fixed
	USE BETIN_INT
	USE ZBREN_INT
	USE rnper_int

	IMPLICIT NONE

external steadystate

! Local Variables and variables for the minimization routines
real(long), dimension(nedu,lifet,ngpa,ngpz,ngpf)	:: residv	! vector to evaluate Euler Errors
real(long),dimension(gensize) :: ftracking,vtracking
REAL(long), EXTERNAL :: betinv_fn

INTEGER :: i, j, i_it, it_r, id, second_round

real(long) :: temp,x_guess(2),r0(gensize),r2(gensize)
real(long) , allocatable, dimension(:) :: samplefix	
character(100) :: name, series1, plot

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Locals for DCORVC subroutine
    INTEGER :: LDCOV, LDINCD, LDX, NVAR
    PARAMETER  (LDCOV=2, LDINCD=2, LDX=gensize, NVAR=2)
    INTEGER ::  ICOPT, IDO, IFRQ, INCD(NVAR,NVAR), IWT, MOPT, NMISS, NOBS, NOUT, NROW, NV
    REAL(long)  ::  COV(LDCOV,NVAR), SUMWT, XX(LDX,NVAR), XMEAN(NVAR)
    IDO   = 0;  NROW  = gensize;    IFRQ  = 0;  IWT   = 0;  MOPT  = 0
    ICOPT = 2   ! get correlations rather than covs
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialize flag for number of rounds after convergence
    second_round = 0

	write(6,*)""												!	
	write(6,'(A35)') "Program:      Crime                    "	!
	write(6,'(A35)') "by:           G Gallipoli & G Fella    "	!
	write(6,'(A35)') "This version: 3 Aprile 2012            "	!
	write(6,*)""												!



	!Read name of current Drive and Directory 
    open(unit=34,file="paths\drive.drv",action='read',position='rewind')
	read(34,*)  drive     !name of Drive in computer
    continue
	close(unit=34)
    drive = trim(drive)
    
    open(unit=34,file="paths\directory.drv",action='read',position='rewind')
	read(34,*)  path1     !name of path to code and working files
    continue
	close(unit=34)
    path1 = trim(path1)

    open(unit=34,file="paths\common-files.drv",action='read',position='rewind')
	read(34,*)  path2     !name of path to code and working files
    continue
	close(unit=34)
    path1 = trim(path1)

	!Read name of current experiment - Just prepare a subdir with appropriate name
    open(unit=34,file=trim(drive)//TRIM(path1)//TRIM(path2)//"expname.txt",action='read',position='rewind')
	read(34,*)  experiment     !name of experiment
    continue
	close(unit=34)
	parg  = trim(experiment)		! name of folder in which results are stored

	
! Allocate global vectors
allocate(con(nedu,lifet,ngpa,ngpz,ngpf,ngpst,maxjail)) 
allocate(sav(nedu,lifet,ngpa,ngpz,ngpf,ngpst,maxjail))
allocate(val(nedu,lifet,ngpa,ngpz,ngpf,ngpst,maxjail))
allocate(dval(nedu,lifet,ngpa,ngpz,ngpf,ngpst,maxjail))
allocate(hour(lifet,ngpz,maxjail,nedu))
allocate(ztrans(ngpz,ngpz,retage-1,nedu))   
ALLOCATE(shrob(ngpst),rescale(ngpst))
ALLOCATE(MRHC(nedu))



CALL RNSET(717)	! set random number seed

	! ****** SET parameter values and INITIAL conditions ****** 
CALL readparameters
 
CALL READHETEROGENEITY

    taxnbase = taxn	
    taxkbase = taxk   ! BASE RATES USED WHEN COMPUTING CHANGES DUE TO POLICIES  

    if (calibselect==1) call read_calib_params
        
    ! Generate grid for crime fixed effect 
    ! Since the crime fixed effect is assumed to be drawn from a Beta 
    ! distrution we use jacobi nodes for its grid
    call jacobi_rule(maxjail/2,s_alpha,s_beta,chi_l,chi_h,cgrid) 

 	

	! ****** Construct average wages and processes for efficiency units ****** 
	print *, "";	print *, 'Call demo subroutine' ;  print *, ""
	CALL Demographics
	print *, ""
	print *, "Retage        ", retage+15
	print *, "Oldest worker ", oldest-youngest+1+15 ! 


	! ****** FIXED EFFECTS: Select realization of fixed effect for individual id (only do once at age 1) ****** 
	! ** FIXED EFFECTS: Select realization of fixed effect for individual id (only do once at age 1) ** 
	CALL DRNUN (gensize,unidrawX)
		
    CALL FIXEFFECT(unidrawX,i_fixed)
	do id=1,gensize
	CALL sifl(i_fixed(id),lowabnode(id)) ! SELECT LOWER ABILITY NODE FOR EACH INDIVIDUAL
    enddo
	where (i_fixed>fgrid(ngpf))
	i_fixed = fgrid(ngpf)
	endwhere
	
	CALL initial_conditions
    CALL DRNNOR (gensize,ldrawHS)	! NORMAL DRAW
    CALL DRNNOR (gensize,ldrawC)	! NORMAL DRAW
	ldrawHS(:) = 0.d0
    ldrawC(:) = 0.d0
    crim_fixed(:) = 0.d0
	! Introduce correlation between crime fixed effect and ability fixed effect using normal copula
    	CALL COPULAMAKER(gensize,unidrawX,crim_fixed)
    	do i = 1,gensize
    	   bet_prob = crim_fixed(i)
    	   x_guess(1) = 0.d0
    	   x_guess(2) = 1.d0
    	   call d_zbren(betinv_fn,x_guess(1),x_guess(2),errabs=1.d-3,errrel=1.d-3)
    	   crim_fixed(i) = x_guess(2)
    	enddo
    	crim_fixed = chi_l + (chi_h - chi_l)*crim_fixed

        ! Check correlation between crime and ability fixed effects a posteriori - stored in COV(.,.)
        XX(:,1)=crim_fixed(:) ; XX(:,2)=i_fixed(:)
        CALL DCORVC (IDO, NROW, NVAR, XX, LDX, IFRQ, IWT, MOPT, &
        ICOPT, XMEAN, COV, LDCOV, INCD, LDINCD, NOBS,NMISS, SUMWT)
        continue
    
    allocate(samplefix(10000))
    ! Plotting the fixed effects' distribution
	samplefix(:)=i_fixed(1:10000)
	series1 = 'fixedeffs.txt' ; plot = 'plotability.txt'
	CALL save_vector(series1,40,10000,samplefix)
	deallocate(samplefix)
	
	print *, "cgrid",cgrid
	! COMPUTE SHARE OF GENSIZE IN EACH ABILITY BIN (FROM 1 TO NGPF-1)
	abgroup=0.d0
	do id=1,gensize
	do i = 1,ngpf-1
	if (i_fixed(id) .gt. fgrid(i) .and. i_fixed(id) .le. fgrid(i+1)) then
	abgroup(i) = abgroup(i) + 1.d0
	endif
	enddo
	enddo
	abgroup = abgroup / real(gensize,long)


	! ****** DRAW USED FOR RANDOM TUITIONS (CONSTANT OVER ITERATIONS) ****** 
	CALL DRNUN (gensize,rndtuit)
	
	! ****** Permutation of id index used to ensure that bequests are uncorrelated with ind. character. ****** 
	CALL rnper(perm)
	
		! ****** DRAW USED FOR RANDOM ELIGIBILITY OF TUITIONS (CONSTANT OVER ITERATIONS) ****** 
	CALL DRNUN (gensize,rndtuit2)
	
	! *** WEALTH DISTRIBUTION *** !
	
	! Initialize wealth distribution of youngest age group ****** 
    if (y_wealth == 1) then	! y_wealth==1 implies exogenous wealth dist. for young
    CALL DRNUN (gensize,r0(:))	! UNIFORM 
 
    wealthi_age1 = r0*5.d0
	
	else ! (y_wealth < 1) 
	name = 'wealthy.txt'
	CALL load_matrix(name,30,1,size(wealthi_age1),wealthi_age1)
	endif

	! COMPUTING EQUILIBRIUM
120 CALL initial_conditions    
    ! Initialize quasilinear terms
 	leisure = leisure_save
	
	wmax0 = wmax
	MRHC = wagetge
    failed_conv = 0
    pctval = 1.d0
    
     
    ! ******** NTAR is the number of calibration targets (it is set in fixed_params) ******** 
	! ******** Allocating *********
	allocate(moms(ntar))     
	CALL steadystate(ftracking,vtracking)
	

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ! DISPLAY correlation of fixed effects !
        print *,' '
        print *,'*******************************************************'
        PRINT '(A40,F14.5)', "Corr.btw.init.wealth and ability ", cov(1,2)

OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND')
WRITE(38,*),' '
WRITE(38,*),'**********************************************'
WRITE (38,'(A40,F14.5)'), "Corr.btw.crime fixed effect and ability ", cov(1,2)
WRITE(38,*),'**********************************************'
CLOSE (unit=38)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    print *, "pctval      ", pctval
    call cal_moments	
    
    IF (failed_conv==0) then 
        !if (second_round > 2 .and. iterazioni==1) then 
            CALL WELFARE(ftracking,vtracking)
            deallocate(moms)
    endif

    deallocate(con,sav,val,dval,hour,ztrans)
    DEALLOCATE(shrob,rescale,MRHC)


END subroutine  SchoolingOLG
