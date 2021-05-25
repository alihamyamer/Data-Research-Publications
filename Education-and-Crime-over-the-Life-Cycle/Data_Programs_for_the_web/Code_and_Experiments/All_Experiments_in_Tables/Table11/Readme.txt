!!! Description !!!
This experiment corresponds to Column (4) in Table 11.
We consider a high school subsidy amount to 17.6% of average labor income.
(The subsidy is paid upfront in one shot.)
This is a general equilibrium experiment.


!!! Implementation !!!
The HS subsidy is implemented mainly by choosing subregime==2 and propsub==17.66.
Therefore, there is no need to do any modifications on the codes.
Everything is done through fixed_params.txt.


!!! What're Changed? (compared to benchmark) !!!

fixed_params.txt: The following lines are changed to

1		! polcosting, =0 edu subs. with no added taxes,=1 paid thru addedlab.tax,=2 paid thru added cap. tax
0		! y_wealth, set =1 for exogenous initial wealth distribution when young, 0 for endogenous

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant
2		! subregime-set=0 no subs, 1 random subs to 44% of college stud.

17.66d0		! propsub - tuition subsidy as a share of tuition cost
1		! tuitquant-set=0 tuition determined as %, 1 fixed value
0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar

0.4d0		! wpercent - wealth percentile used in means tested experiments
0.02d0		! pctprice  - = % update in price during search for market clearing
0.02d0		! pctint  - = % update in interest rate during search for market clearing