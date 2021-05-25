Name of Experiment: college response -- decrease tuition

Explanation: 
Decreasing average tuitions by $430 in 1980$ ($1000 in 2001$)
This is a one-iteration experiment.

What are changed:
1) In the fixed_params.txt, the following lines are changed to:

2		! geneq, set =1 if solving for the general equilibrium, =0 if running simulations with given prices

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar

2) In the codes simul.f90, change the following lines

from
TUIT (2) = MAX(0.097d0  * UNCavgearn , 0.0D0)	! Main benchmark tuitions 

to
TUIT (2) = MAX(0.051d0  * UNCavgearn , 0.0D0)	! Decreasing average tuitions by $430 in 1980$ ($1000 in 2001$)