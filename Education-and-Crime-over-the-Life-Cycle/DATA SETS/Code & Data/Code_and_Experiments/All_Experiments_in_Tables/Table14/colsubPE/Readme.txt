!!! Description !!!
This experiment corresponds to Column (2) in Table 14.
We consider a college subsidy equal to 15.3% of average labor income.
(The subsidy is paid upfront in one shot.)
This is a partial equilibrium experiment.


!!! Implementation !!!
The college subsidy is implemented mainly by choosing subregime==5 and adjusting pctr in transf.f90.
Therefore, we need to do modifications on both fixed_params.txt and the codes.


!!! What're Changed? (compared to benchmark) !!!

1) fixed_params.txt: The following lines are changed to

0		! geneq, set =1 if solving for the general equilibrium, =0 if running simulations with given prices

1		! zeronw, set =1 for zero net wealth (interest rates exogenous)
1		! polcosting, =0 edu subs. with no added taxes,=1 paid thru addedlab.tax,=2 paid thru added cap. tax
0		! y_wealth, set =1 for exogenous initial wealth distribution when young, 0 for endogenous

0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant
5		! subregime-set=0 no subs, 1 random subs to 44% of college stud.

1		! tuitquant-set=0 tuition determined as %, 1 fixed value
0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar

0.4d0		! wpercent - wealth percentile used in means tested experiments
0.05d0		! pctprice  - = % update in price during search for market clearing
0.05d0		! pctint  - = % update in interest rate during search for market clearing

2) in transf.f90, the following lines (around Line 95) are changed to

    CASE (5)
    	if (ledu==2) then
		pctr	= 0.153d0/0.097d0
	else
		pctr	= 0.d0
	endif