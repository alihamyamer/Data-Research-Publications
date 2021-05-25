** THIS CODE MERGES ALL THE STAN (OECD DATA) SHARES INTO A UNIQUE FILE

******************************************************************************
clear
cd "${USdir}\Data_files-Abbott-Gallipoli-RED"
cd "STAN Data\Stata"
******************************************************************************

***************************
** VALUE ADDED SHARES
***************************

use Australia_Raw.dta, replace
keep ISIClab ValAdd*
gen australia=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace australia=australia/5
keep ISIClab australia
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Canada_Raw.dta, replace
keep ISIClab ValAdd*
gen canada=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace canada=canada/5
keep ISIClab canada
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Denmark_Raw.dta, replace
keep ISIClab ValAdd*
gen denmark=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace denmark=denmark/5
keep ISIClab denmark
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Finland_Raw.dta, replace
keep ISIClab ValAdd*
gen finland=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace finland=finland/5
keep ISIClab finland
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use France_Raw.dta, replace
keep ISIClab ValAdd*
gen france=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace france=france/5
keep ISIClab france
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Germany_Raw.dta, replace
keep ISIClab ValAdd*
gen germany=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace germany=germany/5
keep ISIClab germany
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Japan_Raw.dta, replace
keep ISIClab ValAdd*
gen japan=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace japan=japan/5
keep ISIClab japan
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Korea_Raw.dta, replace
keep ISIClab ValAdd*
gen korea=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace korea=korea/5
keep ISIClab korea
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Netherlands_Raw.dta, replace
keep ISIClab ValAdd*
gen netherlands=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace netherlands=netherlands/5
keep ISIClab netherlands
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Norway_Raw.dta, replace
keep ISIClab ValAdd*
gen norway=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace norway=norway/5
keep ISIClab norway
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Sweden_Raw.dta, replace
keep ISIClab ValAdd*
gen sweden=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace sweden=sweden/5
keep ISIClab sweden
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use Switzerland_Raw.dta, replace
keep ISIClab ValAdd*
gen switzerland=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace switzerland=switzerland/5
keep ISIClab switzerland
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use UK_Raw.dta, replace
keep ISIClab ValAdd*
gen uk=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace uk=uk/5
keep ISIClab uk
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

use US_Raw.dta, replace
keep ISIClab ValAdd*
gen us=ValAdd_01+ValAdd_02+ValAdd_03+ValAdd_04+ValAdd_05
replace us=us/5
keep ISIClab us
sort ISIClab
merge ISIClab using "Stan-Industry-Weights-2001-05-val.dta"
drop _merge
sort ISIClab
save "Stan-Industry-Weights-2001-05-val.dta", replace

replace ISIClab="Agriculture, Hunting, Forestry and Fishing" if ISIClab=="Agriculture, Hunting, Forestry and Fishing"
save "Stan-Industry-Weights-2001-05-val.dta", replace

