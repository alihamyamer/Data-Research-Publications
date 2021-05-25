Name of Experiment: changing income variance

Explanation: 
the variance of income shocks increased by 23%

What are changed: 
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

2. In the AR1.txt, the following lines are changed to:

0.0197D0	! vareps 1

0.0123D0	! vareps 2

0.0172D0	! vareps 3