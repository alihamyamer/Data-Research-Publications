/* This file estimates the effect of High School graduation on property
   crime participation using simulated data, as reported in Table 8, column "Model"
Data from stationary equilibrium calibrated to the year 1980. */
clear
use "crime-participation-simulated.dta", replace
set more off
** generate dummy for high school or higher
gen edum = etracking>1
** Generate newage
gen newage = age+15
gen newagesq = newage^2
*! Generate percentiles (rank) of ability like in the work of Lochner and Moretti for 'afqt'
cap drop  pct
egen pct =  rank(i_fixed)
egen maxpct = max(pct)
** Rescale pct
replace  pct= pct/maxpct
replace  pct= pct*99
** integer percentiles
gen pctint=int(pct)
*! Generate percentiles (rank) of crimfix (like for 'afqt')
cap drop  cpct
egen cpct =  rank(crimfix)
egen maxcpct = max(cpct)
** Rescale cpct
replace  cpct= cpct/maxcpct
replace  cpct= cpct*99
** Generate integer percentiles
gen cpctint=int(cpct)
*****************************************************************************************
*! Linear Probability Model: effect of education on crime participation in simulated data
*****************************************************************************************
reg ctracking edum newage newagesq pctint cpctint  if stracking>1
