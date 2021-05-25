Codes for G. Fella and G. Gallipoli, "Education and Crime over the Life
Cycle," The Review of Economic Studies

Any questions or problems with running these codes should be directed to
Giulio Fella (g.fella@qmul.ac.uk).

--------------------------------------------------------------------------------------------

The Fortran 90 files used to calibrate and solve the model in the paper
are provided in the zip file *****.zip. The code relies on functions
from the IMSL Fortran Numerical Library.  

The file contains the following files:

8 utility files:
- betinv_fn.f90   : Computes the inverse image of the Beta distribution
  		    at a point 
- copulamaker.f90 : Computes normal copula
- gf_qsort.f90    : Quicksort algorithm
- gridsa.f90      : Constructs asset grid
- int_node.f90    : Locates interpolating nodes
- jacobi_rule.f90 : Computes Gauss-Jacobi nodes (used for crime fixed effect)
- numderiv.f90    : Computes numerical derivatives by finite difference
- ztrans.f90      : Generates Rouwenhorst (nxn) transition matrix starting
		    from (2x2) matrix 

23 program files:
- global1.f90
  global2.f90
  global3.f90		  : Declare global variables and assign parameter values
- read.90 	    	  : Various subroutines that read exogenous
  		    	    variables and guesses and save equilibrium vector
- startup.f90	    	  : Outside programme, it calls either the calibration
                    	    subroutine (Complex_calibration.f90) or the
                    	    one that computes the equilibrium (main.f90)
- nllsfun.f90       	  : Loss function used for calibration
- Complex_calibration.f90 : Calibrates the model, by minimising the
  			    loss function nllsfun.f90 
- decision.f90            : Solves decision problem
- simul.f90               : Simulates the model
- steadystate.f90         : Computes steady state equilibrium (calls
  			    decisions.f90 and simul.f90) 
- main.f90		  : Contains subroutine SchoolingOLG that calls
  			    the subroutines in read.f90 and the function
  			    steadystate in steadystate.f90
- welfare.f90 		  : Computes welfare (constant lifetime consumption 
  			    equivalent)
- fun_putility_arr.f90    : Computes felicity from consumption
- con_bequest.f90 	  : Computes bequest using intra-temporal condition
- min_z_star.f90 	  : Function whose zero is the value of cash at
	         	    hand below which the borrowing constraint is binding
- dvalue_int.f90   	  : Contains functions that return the expected 
		   	    (i.e. before uncertainty over victimisation
		   	    and apprehension (if criminal) is resolved)
		   	    marginal utility of wealth conditional on
		   	    the current income realisation
- value_int.f90    	  : Contains functions that return the expected 
  			    (i.e. before uncertainty over victimisation and
		   	    apprehension (if criminal) is resolved) continuation
		   	    value conditional on the current income realisation
- enrolment.f90    	  : Computes enrolment rates and iterates on
			    disutility of schooling to match targets.
- govbud.f90 		  : Computes and balances the government budget 
- tranf.f90 		  : Computes education transfers
- ir.f90     		  : Computes interest rate (adjusted for mortality) 
- discount.f90     	  : Computes various compounded-interest factors
- fixed.f90 		  : Contains subroutines fixeffect and fixcrim that 
  			    generate respectively the distribution of innate
	    		    ability and crime taste
- demographics.f90 	  : Computes survival probabilities
- processes.f90 	  : Module containing various utilities, functions and
  			    subroutines 



The following input data files have to be present for the code to run 

1. The folder "paths" has to be created in the default folder. It has to contain:
   - drive.drv        : Contains drive name (e.g. <drive>="c:\")
   - directory.drv    : Contains main folder name relative to directory root
     			(e.g. <directory>="Folder\")
   - common-files.drv : Contains name of "common-files" folder
     			(i.e."common-files\")

2. The folder "common-files" (located in <drive><directory>) has to contain:
- ability.txt     : Boundaries of the five ability bins
- allprobs.txt    : Probability of going to jail for each crime committed 
- calib_start.txt : Initial guess for calibrated parameters
- eff_lths.txt 	  : Age profile of efficiency for high school dropouts
- eff_hsg.txt  	  : Age profile of efficiency for high school graduates
- eff_cg.txt   	  : Age profile of efficiency for college graduates
- enrol.txt    	  : Enrolment targets (by ability)
- targenrol.txt	  : Enrolment targets (aggregate)
- targets.txt  	  : Moments targeted for calibration
- expname.txt 	  : Contains folder name for the experiment (e.g. "Experiment\")
  		    being run
- lower_b.txt  	  : Lower bounds for calibration search region
- upper_b.txt  	  : Upper bounds for calibration search region
- weights.txt  	  : Moment weights in loss function (TO FIX!)

3. The experiment folder (located in <drive><directory>) has to contain: 
- AR1.txt	       : Parameters for stochastic earnings component
- crimepar.txt         : Shares of income depending on victimisation
- fixed_parameters.txt : Various parameter values
- inconditions.txt     : Initial guesses for various endogenous variables
- leisure.txt          : Initial guess for disutility from studying
- surv1to110.txt       : Age profile of mortality rates
- tuitions.txt         : Initial guess for tuition costs (absolute value rather than
  			 share of earnings)
- wealthy.txt          : Initial guess on wealth distribution

The code saves the output in the experiment folder. The main output files are:

- save_output.txt   : Complete output file
- select_output.txt : Subset of output   

