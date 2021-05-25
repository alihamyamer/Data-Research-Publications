Name of Experiment: changing demographics

Explanation: 
the population age composition saw an increase in average age; using Census data,
we approximate the demographic change by a 26 percent reduction in the 16-25 age bracket
and respectively a 9, 12 and 10 percent increases in the 26-45, 46-65 and 66-95 age brackets

What are changed: 
1. In the fixed_params.txt, the following lines are changed to:

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

2. In the codes demographics.f90, uncomment the following lines:

!    popsize(1:10) =    popsize(1:10)*0.74d0
!    popsize(11:30)=    popsize(11:30)*1.09d0
!    popsize(31:50)=    popsize(31:50)*1.12d0
!    popsize(51:80)=    popsize(51:80)*1.10d0