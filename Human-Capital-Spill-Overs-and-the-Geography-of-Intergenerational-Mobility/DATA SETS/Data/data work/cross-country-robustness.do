** THIS FILE PRODUCES PLOTS FOR THE CORRELATION BETWEEN EDUCATION ACHIEVEMENT AND MEASURES OF COMPLEMENTARITY
** EDUCATION DATA ARE OBTAINED FROM THE 1994-98 IALS STUDY, AS WELL AS FROM THE BARRO LEE (2013) DATA SET

clear
cd "${USdir}\Data_files-Abbott-Gallipoli-RED\robustness"
use "country_measures_all.dta"

*** Plots of mean years of schooling versus complementariity proxy (Figure 6, in paper's appendix)
** Barro Lee Edu levels (2000) vs proxy of complementarity 
*twoway (scatter mean_yrs_schl_2000 fac_comp if exporter~="ITA" & exporter~="NLD" & exporter~="CHE", sort mlabel(exporter) mlabsize(vsmall) mlabcolor(midgreen)) (lfit mean_yrs_schl_2000 fac_comp if exporter~="ITA" & exporter~="NLD" & exporter~="CHE")
*twoway (lfit mean_yrs_schl_2000 fac_comp if exporter~="ITA" & exporter~="NLD" & exporter~="CHE", legend(label(1 Linear fit))) (scatter mean_yrs_schl_2000 fac_comp if exporter~="ITA" & exporter~="NLD" & exporter~="CHE", sort mlabel(exporter) mlabsize(vsmall) mcolor(gs1) legend(label(2 country)) ytitle(mean years of education, size(small)) xtitle(complementarity (standardized common factor), size(small)))  
twoway (lfit mean_yrs_schl_2000 fac_comp if exporter~="AUS" & exporter~="JAP" & exporter~="KOR" & exporter~="ITA" & exporter~="NLD" & exporter~="CHE", legend(label(1 Linear fit))) (scatter mean_yrs_schl_2000 fac_comp if exporter~="AUS" & exporter~="JAP" & exporter~="KOR" & exporter~="ITA" & exporter~="NLD" & exporter~="CHE", sort mlabel(exporter) mlabsize(vsmall) mcolor(gs1) legend(label(2 country)) ytitle(mean years of education, size(small)) xtitle(complementarity (standardized common factor), size(small)))  
graph export EDU_BL2000.eps, replace
reg mean_yrs_schl_2000 fac_comp if exporter~="AUS" & exporter~="JAP" & exporter~="KOR" & exporter~="ITA" & exporter~="NLD" & exporter~="CHE", robust
** Barro Lee Edu levels (2005) vs proxy of complementarity (if exporter~="ITA")
*twoway (scatter mean_yrs_schl_2005 fac_comp, sort mlabel(exporter) mlabsize(tiny) mlabcolor(gs1) legend(label(1 country)) ytitle(mean years of education, size(small)) xtitle(complementarity (standardized common factor), size(small))) (lfit mean_yrs_schl_2005 fac_comp, ) 
*reg mean_yrs_schl_2005 fac_comp, robust
