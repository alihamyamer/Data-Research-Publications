MODULE GLOBAL2

	USE GLOBAL1

	IMPLICIT NONE
    SAVE

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Key parameters to (possibly) solve for endogenously		!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	integer :: tfpnorm
	REAL(long) :: bita,sigma,ni,irate,wmin,bumpup,constanta
    REAL(long) :: psi, gamma_1, gamma_2,eta   ! Preferences parameters for bequest motive
	real(long), dimension(25)	::  leisure_save , leisure
	real(long), dimension(gensize) :: ldrawHS, ldrawC
	real(long) :: vardrawHS,vardrawC  ! Standard Deviations of the exogenous distr. for psychic costs of schooling  
	real(long) :: chi_l, chi_h, s_alpha, s_beta    ! Parameters for Beta distribution of crime fixed effect
	real(long), allocatable     :: rescale(:),shrob(:) ! rescale=residual proportion of disposable labour income, shrob=share of income robbed
	! bita is time-preference factor, sigma is coefficient of relative risk aversion, 
	! ni is share of leisure in non separable cobb douglas utility, 
	! irate is interest rate after taxes and depreciation, 
	! wmin is the borrowing constraint, 
	! constanta is the scaling factor used to normalize the after tax wage for HS graduates to 1
	! the leisure values are the coefficients of a polynomial to be used to fit enrolment rates
 	! gamma_1, gamma_2, eta: warm glow bequest motive function - gamma_1*(gamma_2+b_^(1-eta)/(1-eta)
    !             with b = bequest
    ! chi_l, chi_h: lower and upper support of the Beta distribution
    ! s_alpha, s_beta: alpha and beta parameters of the Beta distribution   

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Demographic globals (some read read from external files)!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long), dimension(nedu,lifet)	:: eff	! vector of efficiency units (age-earnings) (read from ext. file)

	real(long), dimension(lifet)	:: surprob	! conditional survival probabilities
	real(long), dimension(maxage)	:: morprobr	! conditional morbidity probabilities (90 entries read from ext. file)
	real(long), dimension(lifet)	:: popsize	! population size (=1 at birth for each cohort)

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Globals for grids										!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long), dimension(ngpa,lifet)		:: agrid		! asset grid, age dependent
	real(long), dimension(ngpz,retage-1,nedu)   :: zgrid		! pers. shocks grid, age/edu dependent
	real(long), dimension(ngpf)				:: fgrid		! permanent. shocks grid (fixed effects)
	real(long), dimension(maxjail/2)		:: cgrid        ! permanent crime shock (nodes)
	real(long), dimension(ngpf-1,nedu)		:: enrol		! enrolment rates by fixed effect groups
	real(long), dimension(nedu)				:: totenrol		! enrolment rates, total population

	real(long) :: scalefgrid		! This factor multiplies the elements of fgrid, changing relative scale of f.e.

	real(long), dimension(nedu) :: abweight			! Weight of ability in wage equation by EDU group



			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Globals to store decision rules							!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long), allocatable, dimension(:,:,:,:,:,:,:) :: con			! optimal consumption policy
	real(long), allocatable, dimension(:,:,:,:,:,:,:) :: sav			! optimal saving policy
	real(long), allocatable, dimension(:,:,:,:,:,:,:) :: val			! value functions
	real(long), allocatable, dimension(:,:,:,:,:,:,:) :: dval		    ! wealth-derivatives of value functions
	real(long), allocatable, dimension(:,:,:,:,:,:)   :: eval,edval     ! expected continuation value and wealth-derivative
	real(long), allocatable, dimension(:,:,:,:,:) :: seval,sedval  ! expected continuation value and wealth-derivative (students)
	real(long), allocatable, dimension(:,:,:,:) :: hour					    	! time endowment
		
!	Marginal utilities for different consumption choices, given states
	real(long), dimension(2,5,ngpa,ngpz,ngpf,25,3)	::	IOM

	! the state variables are: nedu=education , lifet=age , ngpa = asset , ngpz=persistent shock ,
	! ngpf = fixed effect , 2 = whether employed or student

!	real(long), dimension(nedu,lifet,ngpa,ngpz,ngpf,2) :: mmeasure		! state space measure
	integer :: st_ind	! study indicator - =1 if studying, =2 if working
	integer :: jail		! last period jail indicator =odd if not in jail , =even if in jail
	integer :: robbed(gensize)		! robbed indicator =1 if robbed, =2 if not robbed
	integer :: inmate(gensize,maxcrim)	! current period jail indicator =1 if in jail , =2 if not in jail
    integer, dimension(gensize) :: perm ! permuted id indeces
    !$OMP THREADPRIVATE (jail)
    
	! PI_V=Probability of being robbed, PI_A=Probability of being imprisoned
	real(long), dimension(2) :: PI_V
	real(long), dimension(2,maxcrim) :: PI_A 
    
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Price rates and related variables						!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long), dimension(nedu) :: waget,wagetge,wagetgelag
	real(long)	:: pctprice			! In the search for an equilibrium, labor prices are updated by pctprice %
	real(long)	:: pctint			! In the search for an equilibrium, interest rate is updated by pctint %
	real(long)	:: pctval			! In the search for an equilibrium, LEISURE is update by pctval
	real(long)	:: rrate, rratelag	! rate of return
	real(long)	:: scalewage

	real(long) :: zeta			! persistent shocks
	
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Globals passed as inputs in external procedures			!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long) :: wealth,wg, consumption,bq, dist_r, r_0 , r_1, rndtuit(gensize),rndtuit2(gensize)
	integer	:: ialp, age, edu, nfx1
	
	
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Globals for steady states								!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	real(long)	:: aggwealth1 , agginc1 , mhour1 , aggeffs1 , avgwage1 , avgincw1, NetWEdu
	real(long)	:: reswealth
	real(long)	:: wealthi_age1(gensize), wealthi_agemax(gensize), i_fixed(gensize),crim_fixed(gensize)
	integer :: lowabnode(gensize)
	real(long), dimension(ss,lifet) :: wealthdist,hoursdist

	real(long), dimension(ss*lifet) :: wdist,hdist,wdist_2,hdist_2,&
	wdist_beq ! distr. for entire population
	real(long), allocatable :: wvecglobal(:)

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!	Globals for calibration								!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	real(long), dimension(ngpf-1) :: abgroup
	real(long), dimension(ngpf-1,2,3,4)   ::  marginfo      ! info about utility values

		! Global Flags
	INTEGER :: ucost, failed_conv, unc_qop

		! Globals for integration
	integer :: nnodi
	real(long), allocatable :: xloc(:) ,scxloc(:) ,wxloc(:)
	real(long) ::  fixedeffect, FIXIND

	real(long), dimension(gensize)	:: unidrawX	! Uniform draws associated to ability
	real(long), dimension(gensize)	:: unidrawY	! random uniform draw obtained using the copula on unidrawX
	real(long) :: corrind		! Correlation between the variables used to construct Normal copula


END MODULE GLOBAL2
