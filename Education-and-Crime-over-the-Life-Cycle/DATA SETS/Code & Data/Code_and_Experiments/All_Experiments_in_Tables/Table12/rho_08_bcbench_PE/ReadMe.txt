Elasticity value = 5 (PE)

Goal: Partial equilibrium for high elasticity of substitution between high school and college

Folder: goldinkatz-subPE_08

File change:  fixed_params.txt

0		! geneq, set =1 if solving for the general equilibrium, =0 if running simulations with given prices
1		! zeronw, set =1 for zero net wealth (interest rates exogenous)
1		! polcosting, =0 edu subs. with no added taxes,=1 paid thru addedlab.tax,=2 paid thru added cap. tax
0		! y_wealth, set =1 for exogenous initial wealth distribution when young, 0 for endogenous
0		! tfpnorm- set=1 for updating Constanta and AVERAGE EFFICIENCY to normalize WAGE2
0		! ucost- match enrolment rates . =1 if updated, 0 if constant
2		! subregime-set=0 no subs, 1 random subs to 44% of college stud.
1		! tuitquant-set=0 tuition determined as %, 1 fixed value
0		! cbarflag - set=0 if cbar is kept constant (experiment), set =1 when cbar=cbarshare*uncavgearn (benchmark)
0		! emmeflag -set=0 if not calibrating cbar, set =1 when calibrating cbar
0.3058888       !cbarshare - Consumption by inmates while in prison as share of avgwage(1,4) - 0.314d0
3.28000011444092       !gamma_2 
0.12388         !corrind   - correlation between fixed effects of crime and ability, used in copula
0.4d0		! wpercent - wealth percentile used in means tested experiments
0.05d0	! pctprice  - = % update in price during search for market clearing
0.05d0	! pctint  - = % update in interest rate during search for market clearing
0.20d0		! pctval  - = update in quasilinear utility (aka LEISURE) for students


Code change : See “global1.f90” REAL(long), PARAMETER :: jamma = 0.8d0	
