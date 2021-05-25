Name of Experiment: Overall Effect, all changes together


Explanation: 
Taking the benchmark calibration for 1980, we input simultaneously data for the human capital shares, the apprehension probability and the length of the prison term, the variance of income shocks, college tuitions and the age composition of the labor force for the year 2000.

What are changed: 
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

0.16d0		! sh(1) - year 2000.
0.39d0		! sh(2) - year 2000.
0.45d0		! sh(3) - year 2000.

0.99d0		! prisTres - Additional fractional prison term lenght (measured in years)

2. In the common-files\allprobs.txt, the following lines are changed to:

.077d0 ! prob. of going to jail if only one crime is committed, it is 0.17 in Merlo et al (binary crime choice) - .057d0 if each crime state equals only one crime in the data

3. In the AR1.txt, the following lines are changed to:

0.0197D0	! vareps 1

0.0123D0	! vareps 2

0.0172D0	! vareps 3

4. In the codes simul.f90, the following lines are changed
from
"TUIT (2) = MAX(0.097d0  * UNCavgearn , 0.0D0)"
to
"TUIT (2) = MAX(0.23d0  * UNCavgearn , 0.0D0)"

5. In the codes demographics.f90, uncomment the following lines:

!    popsize(1:10) =    popsize(1:10)*0.74d0
!    popsize(11:30)=    popsize(11:30)*1.09d0
!    popsize(31:50)=    popsize(31:50)*1.12d0
!    popsize(51:80)=    popsize(51:80)*1.10d0