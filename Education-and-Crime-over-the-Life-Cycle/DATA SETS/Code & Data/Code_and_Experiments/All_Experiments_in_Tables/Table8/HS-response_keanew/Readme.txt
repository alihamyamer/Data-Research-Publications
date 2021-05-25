Name of Experiment: HS response large (Keane and Wolpin)

Explanation: 
a high school subsidy equal to 25000 in 1994
This is a one-iteration experiment.

What are changed:
1) In the fixed_params.txt, the following lines are changed to:

2		! geneq, set =1 if solving for the general equilibrium, =0 if running simulations with given prices

1		! zeronw, set =1 for zero net wealth (interest rates exogenous)
1		! polcosting, =0 edu subs. with no added taxes,=1 paid thru addedlab.tax,=2 paid thru added cap. tax
0		! y_wealth, set =1 for exogenous initial wealth distribution when young, 0 for endogenous

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant
2		! subregime-set=0 no subs, 1 random subs to 44% of college stud.

30.d0		! propsub - tuition subsidy as a share of tuition cost
1		! tuitquant-set=0 tuition determined as %, 1 fixed value
0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar

