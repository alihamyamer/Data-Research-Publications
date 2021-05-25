!!! Description !!!
This experiment corresponds to Column (5) in Table 11.
We consider a tougher prison sentence that achieves the same victimization rate as the HS subsidy equal to 8.8% of average labor income.
The prison term is 20.0 months.
This is a general equilibrium experiment.


!!! Implementation !!!
The increase in prison term is implemented mainly by changing the value of prisTres.
Therefore, there is no need to do any modifications on the codes.
Everything is done through fixed_params.txt.


!!! What're Changed? (compared to benchmark) !!!

fixed_params.txt: The following lines are changed to

1		! polcosting, =0 edu subs. with no added taxes,=1 paid thru addedlab.tax,=2 paid thru added cap. tax
0		! y_wealth, set =1 for exogenous initial wealth distribution when young, 0 for endogenous

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant

1		! tuitquant-set=0 tuition determined as %, 1 fixed value
0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar

0.663d0		! prisTres - Additional fractional prison term lenght (measured in years)

0.4d0		! wpercent - wealth percentile used in means tested experiments
0.005d0		! pctprice  - = % update in price during search for market clearing
0.005d0		! pctint  - = % update in interest rate during search for market clearing
0.80d0		! pctval  - = update in quasilinear utility (aka LEISURE) for students
1.D-3		! = TOLERANCE (%) FOR WAGE PRICE CONVERGENCE
1.D-3		! = TOLERANCE (%) FOR CAPITAL PRICE (INTEREST RATE) CONVERGENCE