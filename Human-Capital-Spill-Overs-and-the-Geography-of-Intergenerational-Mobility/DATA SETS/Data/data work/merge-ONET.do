** CPS 2000 data

clear
set more off
set memory 120m
cd "${USdir}\Data_files-Abbott-Gallipoli-RED\CPS"
use "morg00.dta", clear
sort occ00
drop if occ00==.
save "census00.dta", replace
clear
use "occ00-soc-concordance.dta", clear
keep occ00 soc_code
sort occ00 soc_code
drop if occ00==occ00[_n-1] & soc_code==soc_code[_n-1]
replace soc_code=substr(soc_code,1,length(soc_code)-1) if occ00==occ00[_n+1]
replace soc_code = plural(2, soc_code, "+0") if length(soc_code)==6
drop if occ00==occ00[_n-1]
drop if occ00==.
save "temp.dta", replace
use "census00.dta", clear
merge occ00 using "temp.dta"
tab _merge
drop _merge
rename soc_code occsoc5 /* use same name as in ONET-occsoc5.dta */
drop if occsoc5==""
save "morg00+soc.dta", replace

*********************************
use "${USdir}\Data_files-Abbott-Gallipoli-RED\STAN Data\Stata\Stan-Industry-Weights-2001-05-val.dta", replace
sort ISIClab
save "${USdir}\Data_files-Abbott-Gallipoli-RED\STAN Data\Stata\Stan-Industry-Weights-2001-05-val.dta", replace

clear
use "${USdir}\Data_files-Abbott-Gallipoli-RED\ONET\ONET-occsoc5-a.dta"
sort occsoc5
save "${USdir}\Data_files-Abbott-Gallipoli-RED\ONET\ONET-occsoc5-a.dta", replace
use  "morg00+soc.dta", clear
sort occsoc5 occ00 hhid
save "morg00+soc.dta", replace
merge occsoc5 using "${USdir}\Data_files-Abbott-Gallipoli-RED\ONET\ONET-occsoc5-a.dta"
*merge occsoc5 using "E:\Dropbox\BA_GG_IGE\CPS and Census\cross-country\ONET-occsoc5-a.dta"
tab _merge
drop _merge

sort occsoc5 occ00 hhid
rename ind02 census02
sort census02
** CENSUS TO ISIC CROSSWALK
joinby census02 using "${USdir}\Data_files-Abbott-Gallipoli-RED\CPS\census02-to-ISIC3-match-r.dta", unmatched(both)
tab _merge
drop _merge

sort census02 occsoc5
egen noindep = mean(noindependence), by(ISIClab)
egen resp = mean(responsibility), by(ISIClab)
egen teamem = mean(teammember), by(ISIClab)


keep ISIClab noindep resp teamem 
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


** NOIND -- construct average NOIND index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = noindep*country_`i' 
egen NOIND`i' = sum(x`i')
}
** RESP -- construct average RESP index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = resp*country_`i' 
egen RESP`i' = sum(x`i')
}
** TEAMMEM -- construct average TEAMMEM index for each country 
cap drop x*
forval  i = 1(1)14{
generate x`i' = teamem*country_`i' 
egen TEAMMEM`i' = sum(x`i')
}


cap drop  ISIClab teamwork impact_on cntact commune noindep resp country* coord freq impact_of teamem _merge
save beforexpose.dta, replace

***************************************************
** Transpose each index and organize them in a file

** No Independence
use beforexpose.dta, replace
keep NOIND*
xpose , clear
rename v1 noindependence
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"
replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_4 canada*/
replace country="denmark"		if number==3 /**country_6 denmark*/
replace country="finland"		if number==4 /**country_8 finland*/
replace country="france"		if number==5 /**country_9 france*/
replace country="germany"		if number==6 /**country_10 germany*/
replace country="japan"		if number==7 /**country_17 japan*/
replace country="korea"		if number==8 /**country_18 korea*/
replace country="netherlands"	if number==9 /**country_21 netherlands*/
replace country="norway"		if number==10 /**country_22 norway*/
replace country="sweden"		if number==11 /**country_28 sweden*/
replace country="switzerland"	if number==12 /**country_29 switzerland*/ 
replace country="uk"			if number==13 /**country_30 uk*/
replace country="us"			if number==14 /**country_31 us*/
sort number
save subs_by_country.dta, replace

** Responsibility
use beforexpose.dta, replace
keep RESP*
xpose , clear
rename v1 responsibility
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"
replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_4 canada*/
replace country="denmark"		if number==3 /**country_6 denmark*/
replace country="finland"		if number==4 /**country_8 finland*/
replace country="france"		if number==5 /**country_9 france*/
replace country="germany"		if number==6 /**country_10 germany*/
replace country="japan"		if number==7 /**country_17 japan*/
replace country="korea"		if number==8 /**country_18 korea*/
replace country="netherlands"	if number==9 /**country_21 netherlands*/
replace country="norway"		if number==10 /**country_22 norway*/
replace country="sweden"		if number==11 /**country_28 sweden*/
replace country="switzerland"	if number==12 /**country_29 switzerland*/ 
replace country="uk"			if number==13 /**country_30 uk*/
replace country="us"			if number==14 /**country_31 us*/
sort number
merge number using subs_by_country.dta
drop _merge
sort number
save subs_by_country.dta, replace

** Team Member
use beforexpose.dta, replace
keep TEAMMEM*
xpose , clear
rename v1 teammember
cap drop number country
gen number=1
replace number = number[_n-1] + 1 if number[_n-1] ~=.
gen country = "country"
replace country="australia"		if number==1 /**country_1 australia*/ 
replace country="canada"		if number==2 /**country_4 canada*/
replace country="denmark"		if number==3 /**country_6 denmark*/
replace country="finland"		if number==4 /**country_8 finland*/
replace country="france"		if number==5 /**country_9 france*/
replace country="germany"		if number==6 /**country_10 germany*/
replace country="japan"		if number==7 /**country_17 japan*/
replace country="korea"		if number==8 /**country_18 korea*/
replace country="netherlands"	if number==9 /**country_21 netherlands*/
replace country="norway"		if number==10 /**country_22 norway*/
replace country="sweden"		if number==11 /**country_28 sweden*/
replace country="switzerland"	if number==12 /**country_29 switzerland*/ 
replace country="uk"			if number==13 /**country_30 uk*/
replace country="us"			if number==14 /**country_31 us*/
sort number
merge number using subs_by_country.dta
drop _merge
sort number
save subs_by_country.dta, replace


** REPLACE ZERO ENTRIES WITH MISSING VALUES
replace teammember=. if teammember==0  
replace responsibility=. if responsibility==0  
replace noindependence=. if noindependence==0  

sort number
save subs_by_country.dta, replace

use "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta", replace
sort country
save "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta", replace

use subs_by_country.dta, replace
sort country
merge country using "${USdir}\Data_files-Abbott-Gallipoli-RED\CORAK IGE\ige-country.dta"
drop _merge
*cap drop if country=="new zealand"


/* SUMMARY OF VARIABLES
responsibility -- (How responsible are you for work outcomes and results of other workers on your current job?) 1low-5high
noindependence -- (How important is INDEPENDENCE to the performance of your current job?) - reverse scale: higher value mean less independence
teammember -- ("Work as a team member") - zero/one binary variable
*/

/*SELECT DEPENDENT VARIABLE: which measure of IGE to use in the analysis!*/
/*** Options are:
igeearn_corak
igeearn_corak_low
igeearn_corak_high
igeearn_corak_plus5
***/
cap drop igeearn
** Select any of the following possibilities to reproduce results in paper!
*gen igeearn=igeearn_corak_low
gen igeearn=igeearn_corak_plus5
*gen igeearn=igeearn_corak_high
*gen igeearn=igeearn_corak

** STANDARDIZE
cap drop stdteamm stdresp stdnoind

egen stdteamm=std(teammember) if igeearn~=.
egen stdresp=std(responsibility) if igeearn~=.
egen stdnoind=std(noindependence) if igeearn~=.

** CHECK DIRECTION OF EFFECTS, R_SQUARED, SIGNIFICANCE (STANDARDIZED VARIABLES -- EFFECT OF A ONE-STANDARD-DEVIATION CHANGE)
reg igeearn stdteamm, robust 
reg igeearn stdresp, robust 
reg igeearn stdnoind, robust 

** COMMON FACTOR
cap drop f1_1 
factor stdteamm stdresp stdnoind, pcf 
rotate
predict f1_1
reg igeearn f1_1, robust

cap drop pos
gen pos=3
replace pos = 1 if country == "canada"
replace pos = 9 if country == "norway"
replace pos = 6 if country == "finland"
replace pos = 10 if country == "korea"
replace pos = 2 if country == "uk"

****************
***  GRAPHS
****************


* Responsibility graph
graph twoway (lfit igeearn stdresp) (scatter igeearn stdresp, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-1.8 0 1.8)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: responsibility ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export IGE-vs-resp.eps, replace 
* Independence graph
graph twoway (lfit igeearn stdnoind) (scatter igeearn stdnoind, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-1.8 0 1.8)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: independence ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export IGE-vs-ind.eps, replace 
* Team graph
graph twoway (lfit igeearn stdteamm) (scatter igeearn stdteamm, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-1.8 0 1.8)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: team member ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export IGE-vs-teamm.eps, replace 
* Common factor graph
graph twoway (lfit igeearn f1_1) (scatter igeearn f1_1, mcolor(gs1) yscale(r(.0 .55)) mlabel(country) mlabv(pos) xlabel(-1.8 0 1.8)), ///
title("IGE decline by increasing skill complementarity",size(4.8))  ///
subtitle(" complementarity proxy: estimated common component ",size(3.0)) ///
ytitle("IGE by country") xtitle("Standardized measure of skill complementarity",size(3)) legend(ring(0) pos(2) order(1 "linear fit") size(3)) 
graph export IGE-vs-ecc.eps, replace 





