
**Control and Selection variables

*HID: Household ID (for linking household and personal variables)

**Household variables
*REGION_C: Country specific
*GROSSNET: Gross or net income measure

**Personal variables:
*PPOPWGT: population person weight
*SEX: 1=male, 2=female
*AGE: age in years
*EMP: 0=unemployed, 1=employed
*EDUC: 1=low, 2=medium, 3=high, 9=indistinguishable
*GROSS1/2: Gross wage in primary/secondary job
*WEEKS/FT/PT: Annual weeks worked, total/part-time/full-time
*IND1/2_C: Country-specific industry code for primary/secondary employment 
*HOURS: Weekly hours worked


*cd "C:\Users\Brad\Documents\LIS_Test"

clear all

**List of all datasets to search through. Only those with all necessary variables will be used.
	*This subset of datasets is selected because they can all be converted to ISIC3.
*local datasets at04 be00 co04 cz04 co07 co10 gr04 gt06 ie00 ie04 ie07 ie10 es00 fi07 sk04 sk07 de00 de04 de07 de10 us04 us07 us10
	*EU + US only:
*local datasets at04 be00 cz04 gr04 ie00 ie04 ie07 ie10 es00 fi07 sk04 sk07 de00 de04 de07 de10 us04 us07 us10

local pools ie de us uk eu

local iePool ie00 ie04 ie07 ie10
local dePool de04 de07 de10
local usPool us04 us07 us10
local ukPool uk99 uk04 uk07 uk10
local euPool at04 be00 cz04 gr04 es00 fi07 sk07



*Declare all the variables required for the code to run:
*	Note: hid required in  both lists for merging

**List of required household variables
local hVariables hid region_c cname year

**List of required personal variables
local pVariables hid ppopwgt sex age emp educ ind1_c pil


**Select output variable
local outputVar r_sdincome







*****Generate empty dataset with only industry codes (1-99)*****

set obs 99
egen ISIC = fill(1/99)

gen ISIClab = "Residual (all remaining industries)"
replace ISIClab = "Agriculture, hunting, forestry and fishing" if ISIC >= 1 & ISIC <= 5
replace ISIClab = "Mining" if ISIC >= 10 & ISIC <= 14
replace ISIClab = "Food products, beverages and tobacco" if ISIC >= 15 & ISIC <= 16
replace ISIClab = "Textiles" if ISIC == 17
replace ISIClab = "Wearing apparel, dressing and dyeing of fur" if ISIC == 18
replace ISIClab = "Leather, leather products and footwear" if ISIC == 19 
replace ISIClab = "Wood and products of wood and cork" if ISIC == 20 
replace ISIClab = "Pulp, paper and paper products" if ISIC == 21 
replace ISIClab = "Printing and publishing" if ISIC == 22 
replace ISIClab = "Chemical, rubber, plastics and fuel products" if ISIC >= 23 & ISIC <= 25
replace ISIClab = "Other non-metallic mineral products" if ISIC == 26
replace ISIClab = "Basic metals and fabricated metal products" if ISIC >= 27 & ISIC <= 28
replace ISIClab = "Machinery and equipment, n.e.c." if ISIC == 29
replace ISIClab = "Electrical and optical equipment" if ISIC >= 30 & ISIC <= 33
replace ISIClab = "Motor vehicles, trailers, and semi-trailers" if ISIC == 34
replace ISIClab = "Other transport equipment" if ISIC == 35 
replace ISIClab = "Manufacturing n.e.c. and recycling" if ISIC >= 36 & ISIC <= 37
replace ISIClab = "Electricity, gas and water supply" if ISIC >= 40 & ISIC <= 41
replace ISIClab = "Construction" if ISIC == 45
replace ISIClab = "Wholesale and retail trade - repairs" if ISIC >= 50 & ISIC <= 52
replace ISIClab = "Hotels and restaurants" if ISIC == 55
replace ISIClab = "Transport and storage" if ISIC >= 60 & ISIC <= 63
replace ISIClab = "Post and telecommunications" if ISIC == 64
replace ISIClab = "Financial intermediation" if ISIC >= 65 & ISIC <= 67
replace ISIClab = "Real estate activities" if ISIC == 70
replace ISIClab = "Renting of mach. and equip. - other business activities" if ISIC >= 71 & ISIC <= 74
replace ISIClab = "Public admin. and defence - compulsory social security" if ISIC == 75
replace ISIClab = "Education" if ISIC == 80
replace ISIClab = "Health and social work" if ISIC == 85
replace ISIClab = "Other community, social and personal services" if ISIC >= 90 & ISIC <= 93

save $mydata\bhacki_ranks, replace

foreach cc in `pools' {

	clear all
	save $mydata\bhacki_pooled, replace emptyok
	
	display _newline(3)
	display "Attempting to analyze `cc'"
	
	
	*Load each dataset in the pool and append them together
	
	foreach ccyy in ``cc'Pool' {
	
		capture noisily {

			display _newline(1)
			display "Attempting to load `ccyy'"
			

			**********Merge household and personal data**********
		
		
			use `hVariables' using $`ccyy'h, clear	
			sort hid

			save $mydata\bhacki_merge, replace
			
			use `pVariables' using $`ccyy'p, clear		
			sort hid
			
			merge m:1 hid using $mydata\bhacki_merge, keep(match)				
			

			
			**********Unify industry encoding**********
		capture {
		
			if "`ccyy'" == "us04" | "`ccyy'" == "us07" | "`ccyy'" == "us10"{
				*Most US data uses census 2002 codes. Apply a census 2002 to ISIC3 crosswalk:
				gen ISIC =.
				
				replace ISIC = 1 if ind1_c == 170
				replace ISIC = 1 if ind1_c == 180
				replace ISIC = 2 if ind1_c == 190
				replace ISIC = 2 if ind1_c == 270
				replace ISIC = 5 if ind1_c == 280
				replace ISIC = 1 if ind1_c == 290
				replace ISIC = 11 if ind1_c == 370
				replace ISIC = 10 if ind1_c == 380
				replace ISIC = 13 if ind1_c == 390
				replace ISIC = 14 if ind1_c == 470
				replace ISIC = 14 if ind1_c == 480
				replace ISIC = 11 if ind1_c == 490
				replace ISIC = 40 if ind1_c == 570
				replace ISIC = 40 if ind1_c == 580
				replace ISIC = 40 if ind1_c == 590
				replace ISIC = 41 if ind1_c == 670
				replace ISIC = 90 if ind1_c == 680
				replace ISIC = 45 if ind1_c == 770
				replace ISIC = 15 if ind1_c == 1070
				replace ISIC = 15 if ind1_c == 1080
				replace ISIC = 15 if ind1_c == 1090
				replace ISIC = 15 if ind1_c == 1170
				replace ISIC = 15 if ind1_c == 1180
				replace ISIC = 15 if ind1_c == 1190
				replace ISIC = 15 if ind1_c == 1270
				replace ISIC = 15 if ind1_c == 1280
				replace ISIC = 15 if ind1_c == 1290
				replace ISIC = 15 if ind1_c == 1370
				replace ISIC = 16 if ind1_c == 1390
				replace ISIC = 17 if ind1_c == 1470
				replace ISIC = 17 if ind1_c == 1480
				replace ISIC = 17 if ind1_c == 1490
				replace ISIC = 17 if ind1_c == 1570
				replace ISIC = 17 if ind1_c == 1590
				replace ISIC = 17 if ind1_c == 1670
				replace ISIC = 18 if ind1_c == 1680
				replace ISIC = 18 if ind1_c == 1690
				replace ISIC = 19 if ind1_c == 1770
				replace ISIC = 18 if ind1_c == 1790
				replace ISIC = 21 if ind1_c == 1870
				replace ISIC = 21 if ind1_c == 1880
				replace ISIC = 21 if ind1_c == 1890
				replace ISIC = 22 if ind1_c == 1990
				replace ISIC = 23 if ind1_c == 2070
				replace ISIC = 23 if ind1_c == 2090
				replace ISIC = 24 if ind1_c == 2170
				replace ISIC = 24 if ind1_c == 2180
				replace ISIC = 24 if ind1_c == 2190
				replace ISIC = 24 if ind1_c == 2270
				replace ISIC = 24 if ind1_c == 2280
				replace ISIC = 24 if ind1_c == 2290
				replace ISIC = 25 if ind1_c == 2370
				replace ISIC = 25 if ind1_c == 2380
				replace ISIC = 25 if ind1_c == 2390
				replace ISIC = 26 if ind1_c == 2470
				replace ISIC = 26 if ind1_c == 2480
				replace ISIC = 26 if ind1_c == 2490
				replace ISIC = 26 if ind1_c == 2570
				replace ISIC = 26 if ind1_c == 2590
				replace ISIC = 27 if ind1_c == 2670
				replace ISIC = 27 if ind1_c == 2680
				replace ISIC = 27 if ind1_c == 2690
				replace ISIC = 27 if ind1_c == 2770
				replace ISIC = 28 if ind1_c == 2780
				replace ISIC = 28 if ind1_c == 2790
				replace ISIC = 28 if ind1_c == 2870
				replace ISIC = 28 if ind1_c == 2880
				replace ISIC = 28 if ind1_c == 2890
				replace ISIC = 28 if ind1_c == 2970
				replace ISIC = 28 if ind1_c == 2980
				replace ISIC = 28 if ind1_c == 2990
				replace ISIC = 29 if ind1_c == 3070
				replace ISIC = 29 if ind1_c == 3080
				replace ISIC = 29 if ind1_c == 3090
				replace ISIC = 29 if ind1_c == 3170
				replace ISIC = 29 if ind1_c == 3180
				replace ISIC = 29 if ind1_c == 3190
				replace ISIC = 29 if ind1_c == 3290
				replace ISIC = 30 if ind1_c == 3360
				replace ISIC = 32 if ind1_c == 3370
				replace ISIC = 33 if ind1_c == 3380
				replace ISIC = 31 if ind1_c == 3390
				replace ISIC = 29 if ind1_c == 3470
				replace ISIC = 31 if ind1_c == 3490
				replace ISIC = 34 if ind1_c == 3570
				replace ISIC = 35 if ind1_c == 3580
				replace ISIC = 35 if ind1_c == 3590
				replace ISIC = 35 if ind1_c == 3670
				replace ISIC = 35 if ind1_c == 3680
				replace ISIC = 35 if ind1_c == 3690
				replace ISIC = 20 if ind1_c == 3770
				replace ISIC = 20 if ind1_c == 3780
				replace ISIC = 20 if ind1_c == 3790
				replace ISIC = 20 if ind1_c == 3870
				replace ISIC = 36 if ind1_c == 3890
				replace ISIC = 33 if ind1_c == 3960
				replace ISIC = 36 if ind1_c == 3970
				replace ISIC = 36 if ind1_c == 3980
				replace ISIC = 36 if ind1_c == 3990
				replace ISIC = 50 if ind1_c == 4070
				replace ISIC = 51 if ind1_c == 4080
				replace ISIC = 51 if ind1_c == 4090
				replace ISIC = 51 if ind1_c == 4170
				replace ISIC = 51 if ind1_c == 4180
				replace ISIC = 51 if ind1_c == 4190
				replace ISIC = 51 if ind1_c == 4260
				replace ISIC = 51 if ind1_c == 4270
				replace ISIC = 51 if ind1_c == 4280
				replace ISIC = 51 if ind1_c == 4290
				replace ISIC = 51 if ind1_c == 4370
				replace ISIC = 51 if ind1_c == 4380
				replace ISIC = 51 if ind1_c == 4390
				replace ISIC = 51 if ind1_c == 4470
				replace ISIC = 51 if ind1_c == 4480
				replace ISIC = 51 if ind1_c == 4490
				replace ISIC = 51 if ind1_c == 4560
				replace ISIC = 51 if ind1_c == 4570
				replace ISIC = 51 if ind1_c == 4580
				replace ISIC = 51 if ind1_c == 4585
				replace ISIC = 51 if ind1_c == 4590
				replace ISIC = 50 if ind1_c == 4670
				replace ISIC = 50 if ind1_c == 4680
				replace ISIC = 50 if ind1_c == 4690
				replace ISIC = 52 if ind1_c == 4770
				replace ISIC = 52 if ind1_c == 4780
				replace ISIC = 52 if ind1_c == 4790
				replace ISIC = 52 if ind1_c == 4870
				replace ISIC = 52 if ind1_c == 4880
				replace ISIC = 52 if ind1_c == 4890
				replace ISIC = 52 if ind1_c == 4970
				replace ISIC = 52 if ind1_c == 4980
				replace ISIC = 52 if ind1_c == 4990
				replace ISIC = 52 if ind1_c == 5070
				replace ISIC = 52 if ind1_c == 5080
				replace ISIC = 50 if ind1_c == 5090
				replace ISIC = 52 if ind1_c == 5170
				replace ISIC = 52 if ind1_c == 5180
				replace ISIC = 52 if ind1_c == 5190
				replace ISIC = 52 if ind1_c == 5270
				replace ISIC = 52 if ind1_c == 5280
				replace ISIC = 52 if ind1_c == 5290
				replace ISIC = 52 if ind1_c == 5370
				replace ISIC = 52 if ind1_c == 5380
				replace ISIC = 52 if ind1_c == 5390
				replace ISIC = 52 if ind1_c == 5470
				replace ISIC = 52 if ind1_c == 5480
				replace ISIC = 52 if ind1_c == 5490
				replace ISIC = 52 if ind1_c == 5570
				replace ISIC = 52 if ind1_c == 5580
				replace ISIC = 52 if ind1_c == 5590
				replace ISIC = 52 if ind1_c == 5591
				replace ISIC = 52 if ind1_c == 5592
				replace ISIC = 52 if ind1_c == 5670
				replace ISIC = 52 if ind1_c == 5680
				replace ISIC = 52 if ind1_c == 5690
				replace ISIC = 52 if ind1_c == 5790
				replace ISIC = 62 if ind1_c == 6070
				replace ISIC = 60 if ind1_c == 6080
				replace ISIC = 61 if ind1_c == 6090
				replace ISIC = 60 if ind1_c == 6170
				replace ISIC = 60 if ind1_c == 6180
				replace ISIC = 60 if ind1_c == 6190
				replace ISIC = 60 if ind1_c == 6270
				replace ISIC = 61 if ind1_c == 6280
				replace ISIC = 63 if ind1_c == 6290
				replace ISIC = 64 if ind1_c == 6370
				replace ISIC = 64 if ind1_c == 6380
				replace ISIC = 63 if ind1_c == 6390
				replace ISIC = 22 if ind1_c == 6470
				replace ISIC = 22 if ind1_c == 6480
				replace ISIC = 72 if ind1_c == 6490
				replace ISIC = 92 if ind1_c == 6570
				replace ISIC = 22 if ind1_c == 6590
				replace ISIC = 92 if ind1_c == 6670
				replace ISIC = 92 if ind1_c == 6675
				replace ISIC = 64 if ind1_c == 6680
				replace ISIC = 64 if ind1_c == 6690
				replace ISIC = 64 if ind1_c == 6692
				replace ISIC = 72 if ind1_c == 6695
				replace ISIC = 92 if ind1_c == 6770
				replace ISIC = 65 if ind1_c == 6870
				replace ISIC = 65 if ind1_c == 6880
				replace ISIC = 65 if ind1_c == 6890
				replace ISIC = 65 if ind1_c == 6970
				replace ISIC = 66 if ind1_c == 6990
				replace ISIC = 70 if ind1_c == 7070
				replace ISIC = 71 if ind1_c == 7080
				replace ISIC = 71 if ind1_c == 7170
				replace ISIC = 71 if ind1_c == 7180
				replace ISIC = 71 if ind1_c == 7190
				replace ISIC = 74 if ind1_c == 7270
				replace ISIC = 74 if ind1_c == 7280
				replace ISIC = 74 if ind1_c == 7290
				replace ISIC = 74 if ind1_c == 7370
				replace ISIC = 72 if ind1_c == 7380
				replace ISIC = 74 if ind1_c == 7390
				replace ISIC = 73 if ind1_c == 7460
				replace ISIC = 74 if ind1_c == 7470
				replace ISIC = 85 if ind1_c == 7480
				replace ISIC = 74 if ind1_c == 7490
				replace ISIC = 74 if ind1_c == 7570
				replace ISIC = 74 if ind1_c == 7580
				replace ISIC = 74 if ind1_c == 7590
				replace ISIC = 63 if ind1_c == 7670
				replace ISIC = 74 if ind1_c == 7680
				replace ISIC = 74 if ind1_c == 7690
				replace ISIC = 1 if ind1_c == 7770
				replace ISIC = 74 if ind1_c == 7780
				replace ISIC = 90 if ind1_c == 7790
				replace ISIC = 80 if ind1_c == 7860
				replace ISIC = 80 if ind1_c == 7870
				replace ISIC = 80 if ind1_c == 7880
				replace ISIC = 80 if ind1_c == 7890
				replace ISIC = 85 if ind1_c == 7970
				replace ISIC = 85 if ind1_c == 7980
				replace ISIC = 85 if ind1_c == 7990
				replace ISIC = 85 if ind1_c == 8070
				replace ISIC = 85 if ind1_c == 8080
				replace ISIC = 85 if ind1_c == 8090
				replace ISIC = 85 if ind1_c == 8170
				replace ISIC = 85 if ind1_c == 8180
				replace ISIC = 85 if ind1_c == 8190
				replace ISIC = 85 if ind1_c == 8270
				replace ISIC = 85 if ind1_c == 8290
				replace ISIC = 85 if ind1_c == 8370
				replace ISIC = 85 if ind1_c == 8380
				replace ISIC = 85 if ind1_c == 8390
				replace ISIC = 85 if ind1_c == 8470
				replace ISIC = 92 if ind1_c == 8560
				replace ISIC = 92 if ind1_c == 8570
				replace ISIC = 92 if ind1_c == 8580
				replace ISIC = 92 if ind1_c == 8590
				replace ISIC = 55 if ind1_c == 8660
				replace ISIC = 55 if ind1_c == 8670
				replace ISIC = 55 if ind1_c == 8680
				replace ISIC = 55 if ind1_c == 8690
				replace ISIC = 50 if ind1_c == 8770
				replace ISIC = 50 if ind1_c == 8780
				replace ISIC = 52 if ind1_c == 8880
				replace ISIC = 52 if ind1_c == 8890
				replace ISIC = 93 if ind1_c == 8970
				replace ISIC = 93 if ind1_c == 8980
				replace ISIC = 93 if ind1_c == 8990
				replace ISIC = 18 if ind1_c == 9070
				replace ISIC = 93 if ind1_c == 9080
				replace ISIC = 74 if ind1_c == 9090
				replace ISIC = 91 if ind1_c == 9160
				replace ISIC = 91 if ind1_c == 9170
				replace ISIC = 91 if ind1_c == 9180
				replace ISIC = 91 if ind1_c == 9190
				replace ISIC = 95 if ind1_c == 9290
				replace ISIC = 75 if ind1_c == 9370
				replace ISIC = 75 if ind1_c == 9380
				replace ISIC = 75 if ind1_c == 9390
				replace ISIC = 75 if ind1_c == 9470
				replace ISIC = 75 if ind1_c == 9480
				replace ISIC = 75 if ind1_c == 9490
				replace ISIC = 75 if ind1_c == 9570
				replace ISIC = 75 if ind1_c == 9590
				replace ISIC = 75 if ind1_c == 9670
				replace ISIC = 75 if ind1_c == 9680
				replace ISIC = 75 if ind1_c == 9690
				replace ISIC = 75 if ind1_c == 9770
				replace ISIC = 75 if ind1_c == 9780
				replace ISIC = 75 if ind1_c == 9790
				replace ISIC = 75 if ind1_c == 9870
				replace ISIC = 75 if ind1_c == 9890


			}
			else if "`ccyy'" == "co07" | "`ccyy'" == "co10" {
				*Divide by 100 to convert from 4-digit to 2-digit ISIC3
				gen ISIC = floor(ind1_c/100)
			}
			else if "`ccyy'" == "sk07" | "`ccyy'" == "ie07" | "`ccyy'" == "ie10" | "`ccyy'" == "gr07" ///
				| "`ccyy'" == "de07" | "`ccyy'" == "de10" | "`ccyy'" == "fi07" | "`ccyy'" == "sk04" {
				*Don't need to alter industry codes for these datasets.
				gen ISIC = ind1_c
			}
			else {
				*Extract ISIC industry code from second-to-last pair of digits
				*(Works for de00, uk99 and other NACE/SIC/ISIC encoded datasets)
				gen ISIC = mod(floor(ind1_c/10),100)
				replace ISIC = . if(ind1_c > 17990)
			}


			*Generate aggregated (labeled) industries
			generate ISIClab = "Residual (all remaining industries)"
			replace ISIClab = "Agriculture, hunting, forestry and fishing" if ISIC >= 1 & ISIC <= 5
			replace ISIClab = "Mining" if ISIC >= 10 & ISIC <= 14
			replace ISIClab = "Food products, beverages and tobacco" if ISIC >= 15 & ISIC <= 16
			replace ISIClab = "Textiles" if ISIC == 17
			replace ISIClab = "Wearing apparel, dressing and dyeing of fur" if ISIC == 18
			replace ISIClab = "Leather, leather products and footwear" if ISIC == 19 
			replace ISIClab = "Wood and products of wood and cork" if ISIC == 20 
			replace ISIClab = "Pulp, paper and paper products" if ISIC == 21 
			replace ISIClab = "Printing and publishing" if ISIC == 22 
			replace ISIClab = "Chemical, rubber, plastics and fuel products" if ISIC >= 23 & ISIC <= 25
			replace ISIClab = "Other non-metallic mineral products" if ISIC == 26
			replace ISIClab = "Basic metals and fabricated metal products" if ISIC >= 27 & ISIC <= 28
			replace ISIClab = "Machinery and equipment, n.e.c." if ISIC == 29
			replace ISIClab = "Electrical and optical equipment" if ISIC >= 30 & ISIC <= 33
			replace ISIClab = "Motor vehicles, trailers, and semi-trailers" if ISIC == 34
			replace ISIClab = "Other transport equipment" if ISIC == 35 
			replace ISIClab = "Manufacturing n.e.c. and recycling" if ISIC >= 36 & ISIC <= 37
			replace ISIClab = "Electricity, gas and water supply" if ISIC >= 40 & ISIC <= 41
			replace ISIClab = "Construction" if ISIC == 45
			replace ISIClab = "Wholesale and retail trade - repairs" if ISIC >= 50 & ISIC <= 52
			replace ISIClab = "Hotels and restaurants" if ISIC == 55
			replace ISIClab = "Transport and storage" if ISIC >= 60 & ISIC <= 63
			replace ISIClab = "Post and telecommunications" if ISIC == 64
			replace ISIClab = "Financial intermediation" if ISIC >= 65 & ISIC <= 67
			replace ISIClab = "Real estate activities" if ISIC == 70
			replace ISIClab = "Renting of mach. and equip. - other business activities" if ISIC >= 71 & ISIC <= 74
			replace ISIClab = "Public admin. and defence - compulsory social security" if ISIC == 75
			replace ISIClab = "Education" if ISIC == 80
			replace ISIClab = "Health and social work" if ISIC == 85
			replace ISIClab = "Other community, social and personal services" if ISIC >= 90 & ISIC <= 93

			
		
		
		}
		
		
	
	
	
			
			*Combine pooled data
			
			append using $mydata\bhacki_pooled
			
			save $mydata\bhacki_pooled, replace
		}
	
	}

	
		*********************************Do pooled analysis here*********************************
	capture noisily {	
		
		use $mydata\bhacki_pooled, clear
		
		gen income = ln(pil)
		
		
		******* CLEAN the dataset (following Lemieux 2006) *******
		
		

		*Drop workers with missing income or industry code
		drop if pil==0 | pil==. | ISIC==.

		* Keep workers of age 16-65
		keep if age>=16 & age<=65

		*NOTE: Ideally we would also eliminate workers who have potential work experience <0
		*		based on age and highest grade completed. But this eduaction data is not generally available.

		*(a) INCOME INEQUALITY

		***Construct industry-specific weighted mean
		egen ibar = mean(income*ppopwgt), by(ISIClab)

		***Generate industry-specific weighted variance and CV
		gen dev2 = (income-ibar)^2
		egen varincome = sum(dev2), by(ISIClab)
		gen sdincome = sqrt(varincome)
		gen cvincome = sdincome/ibar

		***Generate industry specific 95th-5th percentiles
		egen p5 = pctile(income), p(5) by(ISIClab)
		egen p95 = pctile(income), p(95) by(ISIClab)
		gen incomepctile = p95-p5
		gen incomepctilen = incomepctile/ibar


		*(b) RESIDUAL income INEQUALITY
		*xi, noomit: reg income hcap i.ISIClab i.state i.smsastat age age2 age3 i.sex i.veteran i.race i.ethnic i.prcitshp i.class94 [fw=weightr], r 
		gen age2 = age*age
		gen age3 = age2*age
		quietly xi, noomit: reg income i.educ i.ISIClab age age2 age3 i.sex i.region_c [pw=ppopwgt], r 
		predict r_income, res

		***Repeat construction of dispersion measures from regression residual
		***Construct industry-specific weighted mean
		egen r_ibar = mean(r_income*ppopwgt), by(ISIClab)

		***Generate industry-specific weighted variance and CV
		gen r_dev2 = (r_income-r_ibar)^2
		egen r_varincome = sum(r_dev2), by(ISIClab)
		gen r_sdincome = sqrt(r_varincome)
		gen r_cvincome = r_sdincome/r_ibar

		***Generate industry specific 95th-5th percentiles
		egen r_p5 = pctile(income), p(5) by(ISIClab)
		egen r_p95 = pctile(income), p(95) by(ISIClab)
		gen r_incomepctile = r_p95-r_p5
		gen r_incomepctilen = incomepctile/r_ibar
		
		
		***Save rank data for merging***
		egen `cc'Obs = count(`outputVar'), by(ISIClab)
		
		collapse `outputVar' `cc'Obs, by(ISIClab) fast
		
		egen `cc'Rank = rank(`outputVar')
		
		keep `cc'Rank `cc'Obs ISIClab
		sort ISIClab

		merge 1:m ISIClab using $mydata\bhacki_ranks
		
		drop _merge

		save $mydata\bhacki_ranks, replace
		
		
		display "`cc' completed!"
		
		
		***********************************************************************************************
	}

}

******* PRINT results *******
use $mydata\bhacki_ranks, clear

tabstat *Rank *Obs, by(ISIClab)



