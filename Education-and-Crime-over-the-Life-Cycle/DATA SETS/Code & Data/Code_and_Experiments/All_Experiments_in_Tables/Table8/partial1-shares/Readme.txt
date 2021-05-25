Name of Experiment: changing human capital shares

Explanation: 
the high school and college shares changed respectively from 0.41 to 0.39 and from 0.37 to 0.45.

What are changed:
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

0.16d0		! sh(1) - year 2000.
0.39d0		! sh(2) - year 2000.
0.45d0		! sh(3) - year 2000.