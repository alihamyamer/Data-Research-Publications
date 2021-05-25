

cd "C:\Users\Brad\Dropbox\RA Work\Giovanni\LIS output\"
local datfiles sdincomeRanks_ISIClab.dta r_sdincomeRanks_ISIClab.dta ///
				sdwageRanks_ISIClab_PrivateSector.dta r_sdwageRanks_ISIClab_PrivateSector.dta

local datasets de ie uk us10 de10 ie10 uk10 fr05 at04 be00 cz04 es00 fi07 gr04 sk07

local dename "Germany 2004,2007,2010"
local iename "Ireland 2000,2004,2007,2010"
local ukname "UK 1999,2004,2007,2010"
local us10name "US 2010"
local uk10name "UK 2010"
local ie10name "Ireland 2010"
local de10name "Germany 2010"
local fr05name "France 2005"
local at04name "Austria 2004"
local be00name "Belgium 2000"
local cz04name "Czech Republic 2004"
local gr04name "Greec 2004"
local es00name "Spain 2000"
local fi07name "Finland 2007"
local sk07name "Slovakia 2007"


local numDatfiles : list sizeof local(datfiles)
local numDatasets : list sizeof local(datasets)

matrix resultsTable = J(2*`numDatasets',`numDatfiles',.)
matrix colnames resultsTable = sdincome r_sdincome sdwage r_sdwage

local j=1
foreach datfile in `datfiles' {
	
	use `datfile', clear
	drop if isiclab == "Residual (all remaining industries)"

	local i=1
	foreach dataset in `datasets' {
		
		capture {
			*quietly reg `dataset'rank usrank [w=`dataset'obs+usobs], robust
			quietly reg `dataset'rank usrank, robust
			quietly matrix B = e(b)
			quietly matrix V = e(V)
			
			quietly matrix resultsTable[2*`i'-1,`j'] = B[1,1]
			quietly matrix resultsTable[2*`i',`j'] = V[1,1]
		}
		local ++i
	}
	
	local ++j
}



local i=1
foreach dataset in `datasets' {
	
	*display "`dataset'" "	& " %-3.2f resultsTable[2*`i'-1,1] " & " %-3.2f resultsTable[2*`i'-1,2] " \\ [-1.5pt]"  
	*display "	& (" %-3.2f resultsTable[2*`i',1] ") & (" %-3.2f resultsTable[2*`i',2] ") \\ [-1.5pt]"
	
	display "``dataset'name'" "	 & " %-3.2f resultsTable[2*`i'-1,1] " & " %-3.2f resultsTable[2*`i'-1,2]  ///
						" & " %-3.2f resultsTable[2*`i'-1,3] " & " %-3.2f resultsTable[2*`i'-1,4] " \\ [-1.5pt]"  
	display "	 & (" %-3.2f resultsTable[2*`i',1] ") & (" %-3.2f resultsTable[2*`i',2] ")" ///
			" & (" %-3.2f resultsTable[2*`i',3] ") & (" %-3.2f resultsTable[2*`i',4] ") \\ [-1.5pt]"

	
	local ++i
}

