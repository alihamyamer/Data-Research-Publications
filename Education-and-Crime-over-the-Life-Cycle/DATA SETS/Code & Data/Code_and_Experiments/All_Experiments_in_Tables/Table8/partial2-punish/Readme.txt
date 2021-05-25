Name of Experiment: changing expected punishment

Explanation: 
the probability of conviction for a property crime increased from 0.057 to 0.077,
while the prison term for property crimes increased from 19 to 25 months.

What are changed: 
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

0.99d0		! prisTres - Additional fractional prison term lenght (measured in years)

2. In the common-files\allprobs.txt, the following lines are changed to:

.077d0 ! prob. of going to jail if only one crime is committed, it is 0.17 in Merlo et al (binary crime choice) - .057d0 if each crime state equals only one crime in the data