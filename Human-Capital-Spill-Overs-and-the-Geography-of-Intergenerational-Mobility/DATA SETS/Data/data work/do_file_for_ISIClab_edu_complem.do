** THIS DO FILE PERFORMS THE ANALYSIS LINKING AVERAGE YEARS OF SCHOOLING TO MEASURES OF COMPLEMENTARITY BY INDUSTRY.

clear
clear matrix 
set memory 800m
set more off
cd "${USdir}\Data_files-Abbott-Gallipoli-RED\CPS"
use "CPS2000.dta", clear

* Put restrictions on earnings per week
* missing values or too small (earnwke is earnings per week)
drop if earnwke == . | earnwke<300
* age restriction to approximate labor force
keep if age>=16 & age <= 65
* generating potential working experience
* ihigrdc is imputed highest grade completed and average entering school age is 6
gen work_experience = age - ihigrdc - 6
* dropping observations with no work experience and missing values
keep if work_experience >0 & work_experience ~= .
* Putting restrictions on class of workers (dropping some working classes.)
* 1=federal gov; 2=state gov; 3=Local gov; 5=private nonprofit; 8=without pay; 6 or 7 = selfemployed
drop if class94p==1 | class94p==2 | class94p==3 | class94p==5 | class94p==8
drop if class94p==6 | class94p==7
* Sorting by occupation code (occ00 - occupation code for primary job)
sort occ00
drop if occ00==.
save "filteredCPS2000.dta", replace
* Preparing to connect industry code with ISIClab
rename ind02 census02
sort census02
joinby census02 using "mapping industry codes to ISIClab.dta", unmatched(both)
tab _merge
drop _merge

keep ISIClab ihigrdc weight
sort ISIClab
by ISIClab: egen weight_sum = total(weight)
by ISIClab: gen ISIC_weight = weight/weight_sum
gen w_ihigrdc = ISIC_weight*ihigrdc
sort ISIClab
by ISIClab: egen ave_ihigrdc = total(w_ihigrdc)
keep ISIClab ave_ihigrdc
sort ISIClab
keep if ISIClab[_n]~=ISIClab[_n-1]
save "average_edu_by_ISIClab.dta", replace
sort ISIClab
merge ISIClab using "ISIClab_complementarity.dta"
tab _merge
drop _merge

* Standardizing complementarity measures
egen std_teamem =std(teamem) if teamem ~=.
egen std_resp =std(resp) if resp ~=.
egen std_noindep =std(noindep) if noindep ~=.

* Computing factor component for complementarity measures
factor std_teamem std_resp std_noindep , pcf 
rotate
predict fac_comp

*twoway scatter fac_comp ave_ihigrdc || lfit fac_comp ave_ihigrdc
*twoway (scatter fac_comp ave_ihigrdc, sort mlabel(ISIClab) mlabsize(tiny) mlabcolor(midgreen)) (lfit fac_comp ave_ihigrdc) 
cap drop pos
gen pos=3
replace pos = 3 if ISIClab == "Other community, social and personal services"
replace pos = 11 if ISIClab == "Construction"
replace pos = 10 if ISIClab == "Printing and publishing"
replace pos = 8 if ISIClab == "Real estate activities"
replace pos = 6 if ISIClab == "Pulp, paper and paper products"
replace pos = 4 if ISIClab == "Hotels and restaurants"
replace pos = 9 if ISIClab == "Other non-metallic mineral products"

* Generate Figure 4 in the paper
twoway (lfit ave_ihigrdc fac_comp, legend(label(1 Linear fit))) (scatter ave_ihigrdc fac_comp if ISIClab~="Residual", sort mlabv(pos) mlabel(ISIClab) mlabsize(tiny) mcolor(gs1) legend(label(2 industry)) ytitle(mean years of education, size(small)) xtitle(complementarity (standardized common factor), size(small)) )  
graph export industry_sorting.eps, replace
reg ave_ihigrdc fac_comp, robust

save "CPS_final.dta", replace
