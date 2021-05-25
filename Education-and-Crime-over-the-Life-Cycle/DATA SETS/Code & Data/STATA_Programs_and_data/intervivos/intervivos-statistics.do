/** THIS DO FILE CONSTRUCTS TOTAL INTERVIVOS TRANSFERS AND PERFORMS SOME BASIC CALCULATIONS. 
THE DATA COMES FROM THE INCOME SECTION OF THE NLSY97. THE SET OF VARIABLES IN THE INITIAL FILE "ivt_sample.dta"  CORRESPOND TO THE NLSY97 TRANSFER VARIABLES 
CONSTRUCTED BY ABBOTT, GALLIPOLI, MEGHIR AND VIOLANTE (NBER WORKING 18782, 2013). **/
clear
set mem 100m
*****************************************************************************
** STEP 1: COMBINE VARIABLES CORRESPONDING TO VARIOUS SOURCES OF TRANSFERS
*****************************************************************************
* Code as zeros those cases in which individuals report receiving no intervivos transfers. 
* Include separate information on ivts received separately from the individual's mother and father.  
clear 
#delimit; 
drop _all; 
set mem 200000;
use "ivt_sample.dta", replace; 
sort id97;
/* INTERVIVOS - adding up general measure plus separate transfers from mother or father, if available */
/* All values in year 2000 dollars */
gen real_intervivos = amount_inter_vivos;
replace real_intervivos = amount_inter_vivos/cpi2000 if amount_inter_vivos>0 ;
gen real_intervivosmum = amt_inter_vivos_from_mum;
replace real_intervivosmum = amt_inter_vivos_from_mum/cpi2000 if amt_inter_vivos_from_mum>0 ;
gen real_intervivosdad = amt_inter_vivos_from_dad;
replace real_intervivosdad = amt_inter_vivos_from_dad/cpi2000 if amt_inter_vivos_from_dad>0 ;
/*   ALLOWANCE  */
gen real_allowance = total_allowance;
replace real_allowance = total_allowance/cpi2000 if total_allowance > 0;
replace real_allowance = . if real_allowance < 0;
/*   Rent payments     */
replace rent_payment = . if rent_payment < 0;
replace rent_payment = rent_payment/cpi2000;
rename rent_payment real_rent_payment;
/*   Indicators for: lives with mom, dad, both parents */
replace live_with_parents = . if live_with_parents < 0;
replace liveswithmum = . if liveswithmum < 0;
replace liveswithdad = . if liveswithdad < 0;
/*   Generate a variable indicating whether R lives at home with parents or not. */
gen lives_at_home = .;
replace lives_at_home = 1 if liveswithmum==1 | liveswithdad == 1;
replace lives_at_home = 0 if liveswithmum==0 & liveswithdad==0;
replace live_with_parents = 1 if liveswithmum == 1 &  liveswithdad == 1;
/*   Identify whether respondent lives at home or not   */
gen lives_at_home_extra = 0;
/*   R lives at home if either (s)he lives with adopted mom or dad, or lives with biological mom or do  */
replace lives_at_home_extra = 1 if lives_with_adopteddad==1 | lives_with_adoptedmum==1 | lives_with_biodad == 1 | lives_with_biomum==1;
/*  If all 4 variables are missing (NLSY (-) non-response codes, then code lives_at_home_extra as missing  */
replace lives_at_home_extra = . if lives_with_adopteddad<0 & lives_with_adoptedmum<0 & lives_with_biodad < 0  & lives_with_biomum <0 ;
/*  This method increases the observations, while preserving accuracy of the lives_at_home indicator   */
replace lives_at_home = 1 if lives_at_home_extra == 1;
replace lives_at_home = 0 if lives_at_home_extra == 0 & lives_at_home~= 1;

/* Amount intervivos: codes  Refusal(-1),Don't Know(-2),VALID SKIP(-4),NON-INTERVIEW(-5) */
/*  This sets real_intervivos(mum/dad) to 0 if the respondent reported receiving no intervivos transfers from both parents, the mother, and/or the father  */
replace real_intervivos = 0 if inter_vivos==0;
replace real_intervivosmum = 0 if inter_vivos_from_mum==0;
replace real_intervivosdad = 0 if inter_vivos_from_dad==0;
/*  This sets real_allowance to 0 if the respondent reported receiving no allowance   */
replace real_allowance = 0 if receives_allowance == 0;
drop if year==2004 | year==2005;    // No info on intervivos amounts available for 2004 or 2005
sort id97 year;
count if id97 ~=id97[_n-1];
keep if SAMPLE_TYPE == 1 ; // Only cross-sectional sample
count if id97 ~=id97[_n-1];
/*   This line keeps only observations for which at least one measure (of the 3) of intervivos transfers was available   */
keep if real_intervivos>=0 | real_intervivosmum>=0 | real_intervivosdad>=0;
replace real_intervivos = . if real_intervivos < 0;
replace real_intervivosmum = . if real_intervivosmum < 0;
replace real_intervivosdad = . if real_intervivosdad < 0;
/****************
Create variable real_totalintervivos which draws from all questions regarding 
intervivos transfers in "Income Section": YINC-5800/YINC-6600/YINC-7200
***************/
gen real_totalintervivos = real_intervivos;
replace real_totalintervivos =  real_intervivosmum if real_intervivosmum < . & real_intervivos >= .;
replace real_totalintervivos =  real_intervivosdad if real_intervivosdad < . & real_intervivos >= .;
replace real_totalintervivos = real_intervivosmum + real_intervivosdad if real_intervivosmum < . & real_intervivosdad < . & real_intervivos >=.;
gen intervivosfrom_mumonly = real_intervivosmum < . & real_intervivosdad>=. & real_intervivos >=.;
gen intervivosfrom_dadonly = real_intervivosdad < . & real_intervivosmum>=. & real_intervivos >=.;  
/*********************************************
* Generate indicator variables outlining where the data on intervivos
* transfers comes from (in the variable real_totalintervivos)        
*********************************************/
gen rec_bothparents_simult = 0;
replace rec_bothparents_simult = 1 if real_intervivos < .;
gen rec_mum = 0;
replace rec_mum = 1 if real_intervivos >= . & real_intervivosmum < .;
gen rec_dad = 0;
replace rec_dad = 1 if real_intervivos >= . & real_intervivosdad < .;
gen rec_bothparents_indiv = 0;
replace rec_bothparents_indiv = 1 if real_intervivos >= . & real_intervivosmum < . & real_intervivosdad < .;
gen rec_mumonly = 0;
replace rec_mumonly = 1 if real_intervivosmum < . & real_intervivosdad>=. & real_intervivos >=.;
gen rec_dadonly = 0;
replace rec_dadonly = 1 if real_intervivosdad < . & real_intervivosmum>=. & real_intervivos >=.;    
drop if youth_age<16; // Drop if youth is of age less than 16
count if id97 ~=id97[_n-1];
count;

*****************************************************************************
** STEP 2: COMPUTE TRANSFERS per year and TOTAL of Transfers
*****************************************************************************
sort  id97 youth_age  parent_age;
count if id97 ~=id97[_n-1];
count;
/*  Drop Outliers for intervivos transfers    */
drop if real_totalintervivos > 40000; /* 13 outliers (more than 100 times the average), out of 21136 obs */
/* Generate rental proxy for youth who live at home  */
gen real_rent_payment1=. ;
/*  CONVERT EACH FREQUENCY RATE INTO ANNUAL MEASURE  */
replace real_rent_payment1=(real_rent_payment)*12 if howoften_payrent==3 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment) if howoften_payrent==-4 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment)*52 if howoften_payrent==1 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment)*26 if howoften_payrent==2 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment)*2 if howoften_payrent==4 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment) if howoften_payrent==5 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment) if howoften_payrent==6 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment)*2 if howoften_payrent==7 & lives_at_home==0;
replace real_rent_payment1=(real_rent_payment) if howoften_payrent==-5 & lives_at_home==0;
replace real_rent_payment1 = . if real_rent_payment1<=600; 
replace real_rent_payment1 = . if real_rent_payment1>=15000;
/*   GENERATE RENT PROXY BY AGE GROUP   */
gen sixteenrent=real_rent_payment1 if youth_age==16;
gen seventeenrent=real_rent_payment1 if youth_age==17;
gen eightteenrent=real_rent_payment1 if youth_age==18;
gen nineteenrent=real_rent_payment1 if youth_age==19;
gen twentyrent=real_rent_payment1 if youth_age==20;
gen twentyonerent=real_rent_payment1 if youth_age==21;
gen twentytworent=real_rent_payment1 if youth_age==22;
egen meansixteenrent=mean(sixteenrent);
egen meanseventeenrent=mean(seventeenrent);
egen meaneightteenrent=mean(eightteenrent);
egen meannineteenrent=mean(nineteenrent);
egen meantwentyrent=mean(twentyrent);
egen meantwentyonerent=mean(twentyonerent);
egen meantwentytworent=mean(twentytworent);
/*  COMBINE RENT PROXY WITH INTERVIVOS TRANSFERS BY AGE GROUP   */
gen real_intervivosminusrent=real_totalintervivos;
replace real_totalintervivos = real_totalintervivos + meansixteenrent if youth_age==16 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meanseventeenrent if youth_age==17 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meaneightteenrent if youth_age==18 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meannineteenrent if youth_age==19 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meantwentyrent if youth_age==20 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meantwentyonerent if youth_age==21 & lives_at_home==1;
replace real_totalintervivos = real_totalintervivos + meantwentytworent if youth_age==22 & lives_at_home==1;
/*******************************************************/
/** Compute statistics for Fella & Gallipoli, 2014 */
/** distributioon of yearly transfers  */
sum real_totalintervivos, det;
sort id97 youth_age;
bysort id97: gen real_totalgrossintervivos=sum(real_totalintervivos); // Sum transfers intervivos (ALL) in 2000 $ for each id97
sum real_totalgrossintervivos;
save "ivt_sample_final.dta", replace;
/*** Count how many people in the sample have an aggregate transfer (over 7 years) of zero */
clear; 
#delimit cr;
use "ivt_sample_final.dta", replace
sort id97 youth_age
keep if id97~=id97[_n+1]
count
sort id97 youth_age
/* We consider any transfer below $350 as a zero. This corresponds to less than $50 per year. */
/* The share of people with zero transfers is 9%. The value remains 9% if one chooses to set this 
threshold at any value between $300 and $500 (that is, between $40 and $70 per year) */
/* In the paper we perform robustness experiments where we consider values of this share 
much lower and higher than the 9% obtained here. */
count if real_totalgrossintervivos<=350

