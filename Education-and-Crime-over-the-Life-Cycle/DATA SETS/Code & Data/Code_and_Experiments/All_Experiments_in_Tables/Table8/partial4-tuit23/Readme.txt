Name of Experiment: changing college tuitions

Explanation: 
college tuitions increased in real terms from 9.7% of average labor earnings in 1980 to 23% of average earnings in 2000

What are changed: 
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

2. In the codes simul.f90, the following lines are changed
from
"TUIT (2) = MAX(0.097d0  * UNCavgearn , 0.0D0)"
to
"TUIT (2) = MAX(0.23d0  * UNCavgearn , 0.0D0)"