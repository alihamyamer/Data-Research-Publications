** THE RAW CPS DATA CAN BE DOWNLOADED AT http://www.nber.org/morg/annual/
* Start by preparing a merge of STAN INDUSTRIES to Census02 industries in the CPS data
* This is a necessary step before constructing cross-matching measures for each INDUSTRIES
clear 
set memory 120m
set more off

cd "${USdir}\Data_files-Abbott-Gallipoli-RED\CPS"
use census02-to-ISIC3-match-r.dta, replace

tostring isic3_2digit, replace
tostring census02, replace
for any 170 180 190 270 280 290 370 380 390 470 480 490 770 570 580 590 670 680 690: replace census02 = "0X" if census02=="X"

* Drop unnecessary variables and redundant obs
keep ISIClab census02
sort ISIClab census02
gen x=1 if ISIClab==ISIClab[_n-1] & census02==census02[_n-1]
drop if x==1
drop x
sort census02
* Save in a temp file
save "temp.dta", replace


* 1) 2000 CPS data
clear
set more off
set memory 200m
set matsize 2500
use "morg00.dta", clear
* First, generate variable Census02
* Add an initial zero to some of the sectors in ind02 in order to stick to the 2002 Census Code 
* and avoid confusions if I decide to aggregate the data to higher levels. i.e don't confuse sectors 2770 with 0270
gen census02 = ind02
tostring census02, replace
for any 170 180 190 270 280 290 370 380 390 470 480 490 770 570 580 590 670 680 690: replace census02 = "0X" if ind02==X
drop ind02
* GENERATE WAGE VARIABLES, CLEAN DATA
* Construct wages for each individual in dataset (follow Lemieux 2006)
* (1) For workers paid by the hour, wage is just hourly rate of pay
gen wage = earnhre/100 if paidhre == 1
* Note earnhre is measured in pennies
* (2) For other workers, compute the hourly wage rate dividing usual weekly earnings by usual weekly hours of work
replace wage =  earnwke / uhourse if paidhre == 2
bysort paidhre: sum wage
* Adjust topcoded wages and earnings by a factor of 1.4
* Topcoded wages (4 obs)
replace wage = 1.4*wage if earnhre == 9999
* Topcoded earnings (1221 obs)
replace wage = 1.4*wage if earnwke == 2884
* Clean dataset (following Lemieux 2006).
* Keep workers of age 16-65 with positive potential experience
keep if age>=16 & age<=65
gen potexp = age - ihigrdc - 6
keep if potexp>0 & potexp!=.
* Drop workers with missing wages or missing education
drop if wage==0 | wage==.
* ihigrdc: is the NBER-imputed highest grade of school completed
gen hcap = ln(ihigrdc)
drop if hcap==.
sort census02
joinby census02 using "temp.dta", unmatched(both)
* _==1 cases are workers employed in the 4 Census industries that group "not specified" manufacturing activities
* These are marginal activities for which there's no good match 
tab _
tab census02 if _==1
keep if _==3
drop _

* Next, construct industry-specific measures of wage dispersion for ISIClab
order weight ISIClab wage

* a) WAGE INEQUALITY
* NOTE: All inequality measures will be calculated for the distribution of LOG WAGES
replace wage=ln(wage) 
* Second, construct industry-specific weighted mean
* The weights are given by the product of the number of workers represented by the observation and the hours worked
gen weighthours = weight*uhourse if (wage!=.)
* Define industry-specific Final weights
egen indweight = sum(weighthours), by (ISIClab)
* Mean weighted-wage in each industry (wbar)
gen wbartemp = (wage*weighthours)/indweight
egen wbar = sum(wbartemp), by(ISIClab)
* Generate industry-specific wage variance, stardard deviation and coefficient of variation
gen dev2 = (wage-wbar)^2*(weighthours/indweight)
egen varwage = sum(dev2), by(ISIClab)
gen sdwage = sqrt(varwage)
gen cvwage = sdwage/wbar
drop dev2 wbartemp weighthours indweight
* Generate industry-specific 95th percentile - 5th percentile
egen p95 = pctile(wage), p(95) by(ISIClab)
egen p5 = pctile(wage), p(5) by(ISIClab)
gen wagepctile = p95-p5
gen wagepctilen = wagepctile / wbar

* b) RESIDUAL WAGE 
* Generate alternative measure of wage dispersion.
gen age2 = age * age
gen age3 = age2 * age
gen weightr=round(weight)
xi, noomit: reg wage hcap i.ISIClab i.state i.smsastat age age2 age3 i.sex i.veteran i.race i.ethnic i.prcitshp i.class94 [fw=weightr], r 
predict rage1, res

* Next, generate a measure using the same procedure as for raw wages
* The weights are given by the product of the number of workers represented by the observation and the hours worked
* over the sample of workers reporting postive (and non-missing) wages
gen weighthours = weight*uhourse if rage1!=.
* Define industry-specific Final weights
egen indweight = sum(weighthours), by (ISIClab)
* Mean weighted-rage1 in each industry (r1bar)
gen r1bartemp = (rage1*weighthours)/indweight
egen r1bar = sum(r1bartemp), by(ISIClab)
* Now, generate the industry-specific rage1 variance, stardard deviation and coefficient of variation
gen dev2 = (rage1 - r1bar)^2*(weighthours / indweight)
egen varrage1 = sum(dev2), by(ISIClab)
gen sdrage1 = sqrt(varrage1)
gen cvrage1 = sdrage1/wbar
drop dev2 r1bartemp weighthours indweight
* Generate the industry-specific 95th percentile - 5th percentile
egen pr195 = pctile(rage1), p(95) by(ISIClab)
egen pr15 = pctile(rage1), p(5) by(ISIClab)
gen rage1pctile = pr195-pr15
gen rage1pctilen = rage1pctile / wbar

* c) YEARS OF EDUCATION
* Generate an alternative measure of cross matching: variance in (log) HUMAN CAPITAL of the individuals employed in a given industry
* The weights are given by the product of the number of workers represented by the observation and the hours worked
gen weighthours = weight*uhourse if hcap!=.
* Define industry-specific Final weights
egen indweight = sum(weighthours), by (ISIClab)
* Second, construct industry-specific weighted mean
* Mean weighted-hcap in each industry (hbar)
gen hbartemp = (hcap*weighthours)/indweight
egen hbar = sum(hbartemp), by(ISIClab)
drop hbartemp 
* Now, generate the industry-specific hcap variance, stardard deviation and coefficient of variation
gen dev2 = (hcap-hbar)^2*(weighthours/indweight)
egen varhcap = sum(dev2), by(ISIClab)
gen sdhcap = sqrt(varhcap)
gen cvhcap = sdhcap/hbar
drop dev2 weighthours indweight
* Generate the industry-specific 95th percentile - 5th percentile
egen ph95 = pctile(hcap), p(95) by(ISIClab)
egen ph5 = pctile(hcap), p(5) by(ISIClab)
gen hcappctile = ph95-ph5
gen hcappctilen = hcappctile / hbar

* Keep wage, rage, hcap and abil cross-marching measures
keep ISIClab wagepctile wagepctilen varwage sdwage cvwage wbar hcappctile hcappctilen varhcap sdhcap cvhcap hbar rage1pctile rage1pctilen varrage1 sdrage1 cvrage1
sort ISIClab
keep if ISIClab!=ISIClab[_n-1] & ISIClab==ISIClab[_n+1]
* Prepare for merge with the rest of the CPS data
for var wbar- hcappctilen: rename X X00
sort ISIClab
**save "E:\Documents\on top of my desk\various docs\Brant Abbott\Dimeng Chen\Data Collection\Data analysis\Comparing cross-matching measures over time CPS.dta", replace

cap drop Rcv* 
cap drop Rsd* 
cap drop Rrage* 
cap drop Rwage*

egen Rcvrage1=rank(cvrage1)
egen Rsdrage1=rank(sdrage1)
egen Rrage1pctile=rank(rage1pctile00)
egen Rcvwage=rank(cvwage)
egen Rsdwage=rank(sdwage)
egen Rwagepctile=rank(wagepctile00)

save "Comparing cross-matching measures over time CPS.dta", replace


**************************************************
** Merge with the STAN industry weights 2001-05 -- for different countries
**************************************************
use "Comparing cross-matching measures over time CPS.dta", replace
keep ISIClab cvrage1 sdrage1 rage1pctile00 cvwage sdwage wagepctile00 Rcvrage1 Rsdrage1 Rrage1pctile Rcvwage Rsdwage Rwagepctile
sort ISIClab
keep if ISIClab[_n] ~= ISIClab[_n-1]
sort ISIClab
merge ISIClab using "${USdir}\Data_files-Abbott-Gallipoli-RED\STAN Data\Stata\Stan-Industry-Weights-2001-05-val.dta"


** Construct country-level proxies for substitutability
rename australia country_1 /**australia*/ 
rename canada    country_2 /**canada*/
rename denmark   country_3 /**denmark*/
rename finland   country_4 /**finland*/
rename france    country_5 /**france*/
rename germany   country_6 /**germany*/
rename japan     country_7 /**japan*/
rename korea     country_8 /**korea*/
rename netherlands country_9 /**netherlands*/
rename norway    country_10 /**norway*/
rename sweden    country_11 /**sweden*/
rename switzerland country_12 /**switzerland*/ 
rename uk country_13 /**uk*/
rename us country_14 /**us*/

** VARIABLES TO BE USED FOR COMPLEMENTARITY MEASURES:  sdwage00 cvwage00 sdrage100 cvrage100 Rsdwage Rcvwage Rsdrage1 Rcvrage1

** GROSS WAGES VARIABLES
** SD OF WAGE -- construct average SDWAGE index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = sdwage00*country_`i' 
egen SDW`i' = sum(x`i')
}
** CV OF WAGE -- construct average CVWAGE index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = cvwage00*country_`i' 
egen CVW`i' = sum(x`i')
}
** RANK FOR SD OF WAGE -- construct average Rsdwage index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = Rsdwage*country_`i' 
egen RSDW`i' = sum(x`i')
}
** RANK FOR CV OF WAGE -- construct average Rcvwage index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = Rcvwage*country_`i' 
egen RCVW`i' = sum(x`i')
}

** RESIDUAL WAGES VARIABLES
** SD OF RESIDUAL WAGE -- construct average SDRAGE100 index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = sdrage100*country_`i' 
egen SDR`i' = sum(x`i')
}
** CV OF RESIDUAL WAGE -- construct average cvrage100 index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = cvrage100*country_`i' 
egen CVR`i' = sum(x`i')
}
** RANK FOR SD OF RESIDUAL WAGE -- construct average Rsdrage1 index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = Rsdrage1*country_`i' 
egen RSDR`i' = sum(x`i')
}
** RANK FOR CV OF RESIDUAL WAGE -- construct average Rcvrage1 index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = Rcvrage1*country_`i' 
egen RCVR`i' = sum(x`i')
}

drop  ISIClab country* sdwage00 cvwage00 sdrage100 cvrage100 Rsdwage Rcvwage Rsdrage1 Rcvrage1  wagepctile00 rage1pctile00 Rrage1pctile Rwagepctile _merge
drop if SDW1==SDW1[_n-1]
save beforexpose_wages.dta, replace

***************************************************
** Transpose each index and organize them in a file
** SD OF WAGE 
keep SDW*
xpose , clear
rename v1 sdw
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
save wagesubs_by_country.dta, replace

** CV OF WAGE 
use beforexpose_wages.dta, replace
keep CVW*
xpose , clear
rename v1 cvw
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** RANK FOR SD OF WAGE 
use beforexpose_wages.dta, replace
keep RSDW*
xpose , clear
rename v1 rsdw
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** RANK FOR CV OF WAGE 
use beforexpose_wages.dta, replace
keep RCVW*
xpose , clear
rename v1 rcvw
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** SD OF RESIDUAL WAGE
use beforexpose_wages.dta, replace
keep SDR*
xpose , clear
rename v1 sdr
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** CV OF RESIDUAL WAGE
use beforexpose_wages.dta, replace
keep CVR*
xpose , clear
rename v1 cvr
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** RANK FOR SD OF RESIDUAL WAGE 
use beforexpose_wages.dta, replace
keep RSDR*
xpose , clear
rename v1 rsdr
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace

** RANK FOR CV OF RESIDUAL WAGE 
use beforexpose_wages.dta, replace
keep RCVR*
xpose , clear
rename v1 rcvr
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"

replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_2 canada*/
replace country="denmark"		if number==3 /**country_3 denmark*/
replace country="finland"		if number==4 /**country_4 finland*/
replace country="france"		if number==5 /**country_5 france*/
replace country="germany"		if number==6 /**country_6 germany*/
replace country="japan"		if number==7 /**country_7 japan*/
replace country="korea"		if number==8 /**country_8 korea*/
replace country="netherlands"	if number==9 /**country_9 netherlands*/
replace country="norway"		if number==10 /**country_10 norway*/
replace country="sweden"		if number==11 /**country_11 sweden*/
replace country="switzerland"	if number==12 /**country_12 switzerland*/ 
replace country="uk"			if number==13 /**country_13 uk*/
replace country="us"			if number==14 /**country_14 us*/

sort number
merge number using wagesubs_by_country.dta
drop _merge
sort number
save wagesubs_by_country.dta, replace


** REPLACE ZERO ENTRIES WITH MISSING VALUES
replace sdw=. if sdw==0
replace cvw=. if cvw==0 
replace rsdw=. if rsdw==0 
replace rcvw=. if rcvw==0  
replace sdr=. if sdr==0  
replace cvr=. if cvr==0  
replace rsdr=. if rsdr==0  
replace rcvr=. if rcvr==0  
sort number

save wagesubs_by_country.dta, replace


**************************************************
** Merge with the samples of IGE and ONET proxies
**************************************************
use "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta", replace
sort country
save "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta", replace

use wagesubs_by_country.dta, replace
sort country
merge country using "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta"
cap drop _merge

/*SELECT DEPENDENT VARIABLE: which measure of IGE to use in the analysis!*/
/*** Options are:
igeearn_corak
igeearn_corak_low
igeearn_corak_high
igeearn_corak_plus5
***/
cap drop igeearn
*gen igeearn=igeearn_corak
gen igeearn=igeearn_corak_plus5
*gen igeearn=igeearn_corak_high
*gen igeearn=igeearn_corak_low

** AVAILABLE MEASUREMENTS FOR ANALYSIS OF COMPLEMENTARITY: 
*** (1) sdw=sd of wages; 
*** (2) cvw=CV of wages;
*** (3) rsdw=rank(sdw);
*** (4) rcvw=rank(cvw);
*** (5) sdr=SD of res wage;
*** (6) cvr=CV of res wage;
*** (7) rsdr=rank(sdr);
*** (8) rcvr=rank(cvr)

** STANDARDIZE
cap drop std1 std2 std3 std4 
cap drop std5 std6 std7 std8

egen std1=std(sdw) if igeearn~=.
egen std2=std(cvw) if igeearn~=.
egen std3=std(rsdw) if igeearn~=.
egen std4=std(rcvw) if igeearn~=.
egen std5=std(sdr) if igeearn~=.
egen std6=std(cvr) if igeearn~=.
egen std7=std(rsdr) if igeearn~=.
egen std8=std(rcvr) if igeearn~=.

****************************************************************************************************************************
** CHECK DIRECTION OF EFFECTS, R_SQUARED, SIGNIFICANCE (STANDARDIZED VARIABLES -- EFFECT OF A ONE-STANDARD-DEVIATION CHANGE)
reg igeearn std1, robust /*SD of raw wages*/
reg igeearn std2, robust /*CV of raw wages*/
*reg igeearn std3, robust /*rank of SD of raw wages*/
*reg igeearn std4, robust /*rank of CV of raw wages*/
reg igeearn std5, robust /*SD of res wages*/
reg igeearn std6, robust /*CV of res wages*/
*reg igeearn std7, robust /*rank of SD of res wages*/
*reg igeearn std8, robust /*rank of CV of res wages*/

** GROUPING #1: no ranks, only standardized levels
cap drop f1_1 
factor std1 std2 std5 std6, pcf 
rotate
predict f1_1
reg igeearn f1_1, robust

****************
***  GRAPHS
****************

cap drop pos
gen pos=3
replace pos = 1 if country == "canada"
replace pos = 2 if country == "norway"
replace pos = 6 if country == "finland"
replace pos = 10 if country == "korea"
replace pos = 9 if country == "uk"
replace pos = 1 if country == "sweden"
replace pos = 7 if country == "italy"



graph twoway (lfit igeearn std1) (scatter igeearn std1, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-2.3 0 2.3)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: S.D. of raw wages ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export wIGE-vs-SDW.eps, replace 


graph twoway (lfit igeearn std2) (scatter igeearn std2, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-2.3 0 2.3)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy:  C.V. of raw wages  ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export wIGE-vs-CVW.eps, replace 


graph twoway (lfit igeearn std5) (scatter igeearn std5, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-2.3 0 2.3)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy:  S.D. of residual wages  ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export wIGE-vs-SDR.eps, replace 

graph twoway (lfit igeearn std6) (scatter igeearn std6, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-2.3 0 2.3)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy:  C.V. of residual wages  ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export wIGE-vs-CVR.eps, replace 

graph twoway (lfit igeearn f1_1) (scatter igeearn f1_1, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-2.3 0 2.3)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: estimated common component ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export wIGE-vs-ecc.eps, replace 



