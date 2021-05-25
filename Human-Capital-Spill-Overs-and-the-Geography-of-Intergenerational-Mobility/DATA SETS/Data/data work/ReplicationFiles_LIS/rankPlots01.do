clear
cd "C:\Users\Brad\Dropbox\RA Work\Giovanni\LIS output\"
*local datfiles sdincomeRanks_ISIClab.dta r_sdincomeRanks_ISIClab.dta ///
*				sdwageRanks_ISIClab_PrivateSector.dta r_sdwageRanks_ISIClab_PrivateSector.dta

				
local plots sdincome r_sdincome sdwage r_sdwage

local sdincomefile sdincomeRanks_ISIClab.dta
local r_sdincomefile r_sdincomeRanks_ISIClab.dta
local sdwagefile sdwageRanks_ISIClab_PrivateSector.dta
local r_sdwagefile r_sdwageRanks_ISIClab_PrivateSector.dta

local sdincometitle "Industry ranks by S.D. of log labour income"
local r_sdincometitle "Industry ranks by S.D. of log labour income residual"
local sdwagetitle "Industry ranks by S.D. of log wage"
local r_sdwagetitle "Industry ranks by S.D. of log wage residual"

foreach p in `plots' {
	
	use ``p'file', clear
	drop if isiclab == "Residual (all remaining industries)"

	gen shortlab = isiclab
	replace shortlab = substr(shortlab,1,75-2*usrank) + "..." if length(shortlab)>75-2*usrank

	
	twoway (scatter derank usrank, mlabel(shortlab)  mlabangle(0) mlabsize(vsmall) mlabgap(1) graphregion(margin(r+15 t-2 b-2)) ///
	 xlabel(0(10)30) ylabel(0(10)30) legend(off) subtitle(``p'title') xtitle("US") ytitle("Germany")) (lfit derank usrank), ///
	 name(`p'plot, replace)
	
	graph export `p'plot.eps, name(`p'plot) replace
}

*gr combine sdincomeplot r_sdincomeplot sdwageplot r_sdwageplot, graphregion(color(white)) iscale(0.75)
