***********************************************************************************
* NLSY data on property crime participation in 1980 by black, hispanics and whites. 
* The dataset (nlsy_crime.dta) was kindly provided by Lance Lochner.
***********************************************************************************
Variables used to estimate the effect of education on crime participation (Table
8, "Data" column)

propertyc : dummy equal 1 if the respondent commits property crime, zero otherwise
hsgrad    : dummy variable equal 1 if hgc greater than or equal to 12
hgc       : highest grade completed (as of 1980 survey)
enrly     : dummy variable equal 1 if enrolled in school last year
afqt      : AFQT percentile
age       : age
fam       : living with parents at age 14
hgm       : highest grade completed by respondent's mother
hgf       : highest grade completed by respondent's father
smsa      : smsa
regnc     : dummy equal 1 if in north central region 
regne     : dummy equal 1 if in north east region
regns     : dummy equal 1 if in south region
teenmom   : dummy variable equal 1 if mother was a teenager at respondent's birth


*********************************************************************************
* Simulated data on property crime participation: 
* crime-participation-simulated.dta (steady state calibrated to the year 1980) 
*********************************************************************************
Variables used to estimate the effect of education on crime participation (Table
8, "Model" column)

ctracking : dummy equal 1 if the individual has committed property crime
edum      : high-school graduation dummy
newage    : age 
pctint    : percentile of individual ability (fixed effect)
cpctint   : percentile of utility cost of crime (crime fixed effect)


*Note*: The participation rate in the NLSY data is for males only,
 while the model applies to everybody. To account for this discrepancy
 one has to rescale the average participation rate in the NLSY
 regression to make it comparable with that in the regression using
 simulated data. We compute the rescaling factor in the following
 way. If one denotes by FM the ratio between the female and male
 participation rates, the aggregate participation rate equals the male
 rate times the correction factor CF=0.5*(1+FM). We approximate FM by
 the ratio between female and male jail inmates in 1982, the closest
 survey year. The ratio is 7 per cent according to Table 6.18 in
 Pastore and Maguire (1983).  Since the male participation rate in the
 NLSY data is 0.15, applying the correction factor gives a target
 participation for the whole economy of CF*0.15=0.5*(1+.07)=0.08. Therefore, the
 target for the intercept in the calibration regression equals the
 intercept in the NLSY regression minus the adjustment 0.07=0.15-0.08.


