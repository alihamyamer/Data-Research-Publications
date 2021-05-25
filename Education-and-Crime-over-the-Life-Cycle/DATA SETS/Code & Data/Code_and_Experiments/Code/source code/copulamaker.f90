Subroutine copulamaker(szsample,abdraw,crimedraw)
! This subroutine takes the random draw of the ability for each individual and constructs a random draw of crime fixed effect
! which has a correlation of our choice with ability.

	use global1
	use global2
	!use imsl
	use numerical_libraries
	implicit none

	integer, intent(in)	:: szsample
	real(long), intent(in)	:: abdraw(szsample)
	real(long), intent(out) :: crimedraw(szsample)
	integer :: i,j,id
	real(long) :: standnormX(szsample), condnorm(szsample), sdcond, meancond, standnormY(szsample), normdraw(szsample) 

	! LOCAL VARIABLES USED IN DUVSTA SUBROUTINE TO PRODUCE BASIC STATISTICS
	INTEGER :: IDO, IFRQ, IWT, MOPT, IPRINT, NRMISS, NROW, LDX	! LDX SHOULD BE A PARAMETER BUT I SET IT TO A SIMPLE INTEGER
	INTEGER, PARAMETER :: NVAR=1, LDSTAT=15
	REAL(LONG) :: CONPRM, CONPRV	! Confidence level for two-sided interval estimate of the MEAN AND VARIANCE
	! SET TO ZERO FOR NO INTERVAL
	REAL(LONG) :: STAT(LDSTAT,NVAR)
	
	
	! NORMAL DENSITY COPULA
	! Step 1: generate a vector of size szsample of standard normal draws
	CALL DRNNOR (szsample,standnormX)
	! Step 2: transform standnorm vector approrpriately so that each realization is a draw
	! from the conditional N(Y;mean=corr(X,Y)*X,VAR=1-corr(X,Y)^2) - see Gary Venter, Tails of Copulas, pp.75
	
	! Notice: we retrieve the standard normal draw for ability - normdraw(id) - based on unidraw(id)
	! since the distribution of fixed effects is based on unidraw(id).

	! This generates the conditional draws
	do id = 1,szsample
	normdraw(id) = DNORIN(abdraw(id))	! Take the normal dist. ordinate corresponding to each probability
	condnorm(id) = standnormX(id)*((1.d0-corrind**2.d0)**0.5d0) + corrind*normdraw(id)
	enddo 

	NROW=szsample; IDO=0; IFRQ=0; IWT=0; MOPT=0; CONPRM=0; CONPRv=0; IPRINT=0;LDX=szsample;
	! THIS SUBROUTINE RETURNS A VECTOR OF STATISTICS (STAT)
	! Legend:
	!I=15	STAT(I, *)
	!1	contains means
	!2	contains variances
	!3	contains standard deviations
	!4	contains coefficients of skewness
	!5	contains coefficients of excess (kurtosis)
	!6	contains minima
	!7	contains maxima
	!8	contains ranges
	!9	contains coefficients of variation, when they are defined. If the coefficient of variation is not defined for a given variable, STAT(9, *) contains a zero in the corresponding position.
	!10	contains numbers (counts) of nonmissing observations
	!11	is used only when CONPRM is positive, and, in this case, contains the lower confidence limit for the mean (assuming normality)
	!12	is used only when CONPRM is positive, and, in this case, contains the upper confidence limit for the mean (assuming normality)
	!13	is used only when CONPRV is positive, and, in this case, contains the lower confidence limit for the variance (assuming normality).
	!14	is used only when CONPRV is positive, and, in this case, contains the upper confidence limit for the variance (assuming normality).
	!15	is used only when weighting is used (IWT is nonnegative), and, in this case, contains the sums of the weights.
	CALL DUVSTA (IDO, NROW, NVAR, CONDNORM, LDX, IFRQ, IWT, MOPT, CONPRM, CONPRV, IPRINT, STAT, LDSTAT, NRMISS)
	! Save the mean and standard deviation of the condnorm distribution
	meancond=stat(1,1)
	sdcond=stat(3,1)
	! Transform the condnorm distribution into a standard normal
	standnormY(:) = (CONDNORM(:) - meancond)/sdcond

	! Step 3: evaluate Gaussian on values from standnormY(:) to obtain the uniform draws for the second variable Y, that is p(Y)
	do id = 1,szsample
	crimedraw(id) = DNORDF(standnormY(id))
	enddo

	! NOTICE: both unidrawX and unidrawY are gloabl. UnidrawX is used to set the fixed effects.
	! We can now use unidrawY to assign assets that are correlated with the fixed effects.

End Subroutine copulamaker
