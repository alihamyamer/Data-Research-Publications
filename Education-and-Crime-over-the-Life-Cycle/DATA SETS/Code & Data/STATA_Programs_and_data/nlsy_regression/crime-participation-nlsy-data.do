/* This file estimates the effect of High School graduation on reported property
    crime participation used for: 
    (1) Targets for calibration regression (Table 2)
    (2) Robustness check (Table 8, "Data" column)
Data from the 1980 wave of NLSY79 (nlsy_crime.dta) kindly provided  by Lance Lochner. */
set mem 1000m
clear
use nlsy_crime, replace
set more off
* Age is expressed in months (as of 1980). Transform in years.
cap drop newage
gen newage=age/12
gen newagesq=newage^2
* Generate 0/1 property crime indicator
** the value of variable propcr is coded as 2 if one is a non-criminal and 1 if one is a criminal. We transform it into a 0/1 binary variable.
gen propertyc = propcr
replace propertyc=0 if propertyc==2
** (1)
*********************************************************************************************
*! Targets for calibration regression (Table 2)
reg propertyc hsgrad afqt newage
** (2)
*********************************************************************************************
*! Robustness check (Table 8, "Data" column)
reg propertyc hsgrad afqt smsa newage newagesq regnc regne regs teenmom fam hgm hgf enrly 




