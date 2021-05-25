MODULE GLOBAL1
!use AvFRT
!    use ifport
	IMPLICIT NONE
	SAVE

				!	PRECISION PARAMETER FOR REAL KIND
INTEGER, PARAMETER :: LONG=SELECTED_REAL_KIND(15,100)
INTEGER, PARAMETER :: PI=3.14159265358979323846d0

! NOTES: the aggregate production technology is A*K^theta*N^(1-theta)
! where N comes from (some) aggregation of various labor inputs.
! A is a constant (constanta) which is time invariant and ensures that the Marginal
! productivity of aggregate labor factors (N) is equal to 1
! in a steady state. N = (HC1^share1*HC2^share2*HC3^share3)

Real :: T1

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
			! Choice variables for alternative experiments								  !
			!NOTE: use same names as in HSV to avoid confusion even when procedures differ!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INTEGER :: endog_lab ! set =1 if running model with endogenous labor choice, =0 if labor supply inealstic
	INTEGER :: geneq	 ! set =1 if solving for the general equilibrium, =0 if running simulations with given parameters
	INTEGER :: natbc	 ! set =1 if solving with natural borrowing constraint, =0 if exogenous borrowing constraint
	INTEGER :: errcheck  ! set =1 if you want to assess the size of errors in the decision rule
	INTEGER :: zeronw	 ! set =1 for zero net wealth (interest rates and wages exogenous)
	INTEGER :: polcosting	! =0 if edu subs. with no additional taxes, =1 if paid thru addit.labor taxes,=2 if paid thru add. capital taxes
	INTEGER :: y_wealth	 ! set =1 for exogenous initial wealth distribution when young, 0 for endogenous
	INTEGER :: xinterp	 ! set =1 for interpolation using consumption, 0 for savings interpolation
	INTEGER :: techoice	 ! set =1 for C.D., =0 for non-nested CES
	INTEGER :: cbarflag	 ! set=0 if not calibrating cbar, set =1 when calibrating cbar	
	INTEGER :: emmeflag	 ! set=0 if not calibrating emme, set =1 when calibrating emme (emme determines the cost per prisoner)
	INTEGER :: constinwealth ! set=0 if refreshing the initial wealth distribution, set =1 when wealthi_age1 constant

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Set directory paths and other practicalities		!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INTEGER :: iseed = 90110	! set seed for simulation
	CHARACTER(100):: path1		! path to main directory of program
	CHARACTER(100):: path2		! path to utility folder, containing general utility files 
	CHARACTER(100):: pathAD		! path to files to run Ability Discretization
	CHARACTER(60) :: parg		! name of (empty) folder in which results are saved
	CHARACTER(20) :: drive		! name of drive where we work
    CHARACTER(60) :: experiment	! name of subdirectory where we work

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Demographics and Time parameters				!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INTEGER, PARAMETER :: youngest = 16		! age of youngest youngest in PSID sample
	INTEGER, PARAMETER :: oldest   = 65		! age of oldest worker in PSID sample
	INTEGER, PARAMETER :: maxage   = 95		! maximum age in real world data
    INTEGER, PARAMETER :: beqage   = 30		! bequest age in model age
	
	INTEGER, PARAMETER :: lifet = maxage-youngest + 1		! lenght of lifetime
	INTEGER, PARAMETER :: retage = oldest-youngest+ 2		! RETIREMENT AGE
	INTEGER	 :: prisT  	    ! Prison Term (how many periods in jail)
    REAL(long)	 :: prisTres    ! non-integer part of jail sentence (sentence=prisT+prisTres) 
                                ! measured in (fraction of) periods
    

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Target moments parameters		 				!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INTEGER :: ntar ! Ntar=Number of target moments
	
	REAL(long), ALLOCATABLE, DIMENSION(:) :: moms ! moms = vec of target moments
	REAL(long), ALLOCATABLE, DIMENSION(:) :: MRHC	! Marginal returns on Human Capital Types
	REAL(long)	:: MRK	! Marginal return on Physical Capital Types


			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Technology parameters			    !
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	INTEGER,PARAMETER :: nedu = 3		! # of education groups included in model
                                                
	REAL(long), PARAMETER :: delta = 0.065		! depreciation rate
	REAL(long), PARAMETER :: theta = 0.35		! share of physical capital in (C-D) output
	REAL(long), PARAMETER :: jamma = 0.677		! Old FG = 0.45 - curvature of CES technology for non CobbDouglas cases
    !REAL(long), PARAMETER :: jamma = 0.8d0		! Robustness: Using the max elasticity (5) as Goldin & Katz (NBER 12984, 2007)
    !REAL(long), PARAMETER :: jamma = 0.5d0		! Robustness: Using the min elasticity (2) as Goldin & Katz (NBER 12984, 2007)
    !REAL(long), PARAMETER :: jamma = 0.29d0	! Robustness: Using the same elasticity (1.4) as Katz & Murphy
        
	! Long run averages of the cobb douglas shares of HC inputs
	REAL(long), DIMENSION(nedu) :: sh

	! Human Capital aggregates
	REAL(long), DIMENSION(nedu) :: HC , HClag

	! Physical Capital Aggregate, prod. output and aggregate labor input
	REAL(long) :: PhK , prodout, prodout_lag, NN

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Exogenous/Endogenous labor supply, utility & consumption !
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	REAL(long), PARAMETER, DIMENSION(lifet) :: hbar = 0.99		! Hours worked when employed - fixed labor case
	REAL(long) :: CLOW  ! LOWER BOUND FOR CONSUMPTION
	REAL(long) :: cbar	! consumption in jail
	REAL(long) :: cbarshare	! consumption in jail as a fraction of avgwage(1,4)
    REAL(long) :: INADA, INADERIV ! LOWER BOUND OF: (1) PERIOD UTILITY; (2) MARGINAL UTILITY 
	REAL(long) :: emme	! Administrative costs of jailing per prisoner in each period
    REAL(long) :: const_beq, v_glob, bet_prob ! Temporary variables used by the fns con_beq & min_z_star
    integer    :: i_we_glob ! Temporary variable used by the fun min_z_star
    !$OMP THREADPRIVATE(const_beq, i_we_glob, v_glob)
    
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Government and Edu Cost parameters				!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	REAL(long) :: taxn, taxnbase	! tax rate on labor income
	REAL(long) :: taxk, taxkbase	! tax rate on capital income
	REAL(long) :: Gexp, Transfexp	! non-valued governm. expenditure and edu transfer expenditure
	REAL(long) :: aggprexp			! aggregate prison expenditure (total)
	REAL(long) :: TAXrev			! total tax revenue
	REAL(long), DIMENSION(nedu) :: tuit	, tuit_lag	! tuition cost
	REAL(long), DIMENSION(nedu) :: schshare         ! share of school goers receiving subsidy
	INTEGER	:: subregime,tuitquant,forcepol,forceHS1
	REAL(long) :: meanstestw1		! wealth threshold for means-testing
    REAL(long) :: wpercent          ! Wealth percentile
    REAL(long) :: propsub           ! Edu subsidy as a share of tuition cost
    INTEGER :: pensind				! Pension system indicator (1 if exogenous replacement rate)
	INTEGER, DIMENSION(nedu) :: eduyr		! number of years to pass from an education group to the next
        

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!   	Social security parameters
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        REAL(long), DIMENSION(lifet) :: penv	! Pension value: 0 before retirement, +ve afterwards
        REAL(long), DIMENSION(lifet) :: penvlag	! lagged value of penv
        REAL(long) :: pensreprate		! pension replacement rate
        REAL(long) :: AGGPENS			! aggregate pension expenditure

					! AVERAGE and MEDIAN of WEALTH and INCOME

	REAL(long)	::	avinc, medinc, varinc, avwealth,avwealth35, medwealth, varwealth,UNCavgearn, ann


			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Grids parameters				 				!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	REAL(long), PARAMETER :: pw1 = 0.015d0		! space between first two points in asset grid - 0.03 is standard
											! - with natural borrowing constraint can be set to 0.05
	REAL(long) :: wmax, wmax0	! Benchmark value - 30 also for NBC - can be set to 45 in case of log-log preferences (additive)
	
	INTEGER, PARAMETER :: ngpa = 100 	! # of points in asset grid
	INTEGER, PARAMETER :: ngpz = 5		! # of points in izet grid 
	! (1 employed, 2 unemployed)
    INTEGER, PARAMETER :: ngpst = 11    ! # of points in stind grid: change MAXCRIM whenever this is changed 
	INTEGER, PARAMETER :: ngpf = 6		! # of points in fixed effect grid	
   	INTEGER, PARAMETER :: maxcrim = 4   ! Attention!!! maxcrim = (ngpst-3)/2 

! maxjail is # of states for state variable jail. It equals 2 (in jail/out of jail at
! beginning of period) times the # number of possible states for the crime taste shock crimut  
! Change read command for crimut whenever maxjail is changed 
    INTEGER, PARAMETER :: maxjail = 12      
    
    ! borncrim is the proportion of agents with each crimut
    REAL(long), DIMENSION(maxjail/2-1)	 :: borncrim  
    
!	integer, parameter :: ntp  = 20		! # of test points in grid to evaluate euler errors 

	! Summary measures
	REAL(long), DIMENSION(nedu,lifet,ngpst) :: edufrac		! fraction of people in each education group by age/empl.status excluding inmates in numerator
	REAL(long), DIMENSION(nedu,lifet,ngpst) :: edufractot	! fraction of people in each education group by age/empl.status including inmates in numerator
	REAL(long), DIMENSION(nedu,lifet,ngpf-1,ngpst) :: abifrac	! fraction of people in each education group by age/ability bin/STIND
	REAL(long), DIMENSION(nedu,lifet,ngpf-1,ngpst-1) :: abifracW	! fraction of people in each education group by age/ability bin/STIND EXCLUDING STUDENTS
	REAL(long), DIMENSION(nedu,lifet,ngpf-1,ngpst-1) :: abifracWF	! fraction of people in each education group by age/ability bin/STIND EXCLUDING STUDENTS and EXCLUDING INMATES	
	REAL(long), DIMENSION(nedu,lifet,ngpf-1,ngpst) :: avewealth	! average wealth in each education group by age/ability bin/STIND
	REAL(long), DIMENSION(nedu,lifet,ngpf-1,ngpst) :: aveabi	! average ability in each education group by age/ability bin/STIND

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! Z SHOCK PARAMETERS 
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL(long), allocatable, DIMENSION(:,:,:,:) :: ztrans ! Transition matrix
    real(long), dimension(ngpz,nedu)            :: zstat ! Stationary z distribution


			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Parameters	for the simulation of the economy	!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
	INTEGER, PARAMETER :: gensize = 10000	! size of each cohort  
	INTEGER, PARAMETER :: ss = gensize/5     ! Sample size

	REAL(long), PARAMETER :: crith = 0.02d0		! criterion to define hours-constrained individuals
	REAL(long), PARAMETER :: critw = 0.0d0		! criterion to define wealth-constrained individuals

    REAL(long) :: TOLPR   ! CRITERION FOR HC PRICE CONVERGENCE
    REAL(long) :: TOLIR   ! CRITERION FOR CAPITAL PRICE (INTEREST RATE) CONVERGENCE
    REAL(long) :: TOLHC   ! CRITERION FOR HC AGGREGATES' CONVERGENCE

	! parameters for DZREAL (IMSL routine solving for real zeroes of an equation)
	REAL(long), PARAMETER :: eps = 0.00001d0
	REAL(long), PARAMETER :: errabs = 0.000001d0		! 0.0000001
	REAL(long), PARAMETER :: errrel = 1.d-9	!0.0000001d0		! 0.00000001
	REAL(long), PARAMETER :: ayta   = 0.0001
	INTEGER :: iterazioni
	INTEGER, PARAMETER :: nroot = 1


	! parameters for DBCPOL

	! For optimal policy
	INTEGER, PARAMETER :: PP = 1
    INTEGER ::    IBTYPE
    INTEGER ::    MAXFCN
	REAL(long)	::	FTOL, XLB(PP), XUB(PP)
	
	! For calibration
	REAL(long) , ALLOCATABLE :: LBounds(:) , UBounds(:)


	! parameters fo DNEQBF
	INTEGER :: IPARAM(6)
	REAL(long) :: RPARAM(5)
	REAL(long) , ALLOCATABLE :: FSCALE(:), XSCALE(:)

	! Utility var: to be cancelled
	REAL(long) :: tempobj, tempobj2, tempbox


END MODULE GLOBAL1
