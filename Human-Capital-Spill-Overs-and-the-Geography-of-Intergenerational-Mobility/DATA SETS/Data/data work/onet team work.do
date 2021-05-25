********************
* PREPARE O*NET DATA
********************
* Note: We use ONET version 12.0 and get several proxies for complementarity. Some variables are not available in version 4.0. 
* Data for version 12.0 is from around 2006 (see dataset for exact detail). The main source is incumbent (as opposed to job analyst).
* We save different measures. Only three of them are appropriate and usable to measure the type of complementarity between workers defined in the paper.
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
* From "Work Context" file.
* 3 variables are saved, but not used because they capture things beyond complementarity.
* (1) Measure of Team Work 
* (a) A measure of teamwork intensity by occupation can be obtained from the answer to the question:
* "How important are interactions that require you to work with or contribute to a work group or team to perform your current job?"
* Answers range from 1 to 5: 1 " Not important at all", 2 "Fairly important", 3 "Important", 4 "Very Important", 5 "Extremely Important"
* As explained in the data dictionary (p.32), scaleid=="CX" is the weighted average of data values for categories 1 to 5. 
* (b) Another possible way to measure the impact of one's work on coworkers comes from the question:
* "In your current job, what results do your decisions usually have on other people or the image or reputation or financial resources of your employer?"
* (c) Finally, the frequency of decisions regarding others.
* "In your current job, how often do your decisions affect other people or the image or reputation or financial resources of your employer?"
clear
set more off
cd "${USdir}\Data_files-Abbott-Gallipoli-RED\ONET"
insheet using "Work Context.TXT", tab
keep if scaleid=="CX"
* Drop unnecessary variables and keep obs of interest
keep if elementname=="Work With Work Group or Team" | elementname=="Impact of Decisions on Co-workers or Company Results" | elementname=="Frequency of Decision Making"
keep onetsoccode elementname datavalue
gen j = 1 if elementname=="Work With Work Group or Team"
replace j=2 if elementname=="Impact of Decisions on Co-workers or Company Results"
replace j=3 if elementname=="Frequency of Decision Making"
drop elementname
reshape wide datavalue, i(onetsoccode) j(j)
rename datavalue1 teamworkonet
rename datavalue2 impactofdecisions
rename datavalue3 frequency
keep onetsoccode teamworkonet impactofdecisions frequency
sort onetsoccode
save temp, replace

* From "Occupations to DWAs.TXT" file.
* (2) Team member - this is a BINARY (zero/one) variable, asking people whether they work in teams.
* Complementarity requires teamwork, so this is sure to be associated with the prevalence of complementarity!
* Simple enough question, no other hidden meanings. 
* Here, we use data for ONET 13.0. Read about DWAs in DWA_summary.pdf. Work Activities as listed in the O*NET Content Model are broad descriptors of work.
clear
insheet using "Occupations to DWAs.TXT",tab
count if dwacode==1876
sort onetsoccode
count if onetsoccode!= onetsoccode[_n-1]
gen teamm = 1 if dwacode==1876
egen teammember = mean(teamm), by(onetsoccode) 
replace teammember = 0 if teammember!=1
drop dwacode teamm
duplicates drop onetsoccode teammember, force
sort onetsoccode
merge onetsoccode using temp
drop _
sort onetsoccode
save temp, replace

* From "Work Styles" file.
* (3) "Lack of" Independence (1.C.6). From the questionnaire: Job requires developing one's own ways of doing things, guiding oneself with little or no supervision, 
* and depending on oneself to get things done. How important is INDEPENDENCE to the performance of your current job?
* From this, we generate a "lack of" independence variable.
clear
insheet using "Work Styles.TXT", tab
* Drop unnecessary variables and keep obs of interest
keep if elementname=="Independence"
gen noindependence = 6 - datavalue
keep onetsoccode noindependence
sort onetsoccode
merge onetsoccode using temp
drop _
sort onetsoccode
save temp, replace

* From "Work Activities" file.
* (4) The four variables below are possibly related to complementarity but not used because they capture too many different things.
* (a) Communicating with supervisors, peers or subordinates (4.A.4.a.2)
* From the questionnaire: Providing information to supervisors,coworkers, and subordinates by telephone, in written form, e-mail, or in
* person. What level is COMMUNICATING WITH SUPERVISORS, PEERS, OR SUBORDINATES to the performance of your current job?
* (b) Establishing and Maintaining Interpersonal Relationships (4.A.4.a.4)
* From the questionnaire: Developing constructive and cooperative working relationships with others and maintaining them over time.
* What level of ESTABLISHING AND MAINTAINING INTERPERSONAL RELATIONSHIPS is needed to perform your current job?
* (c) Getting members of a group to work together to accomplish tasks.
* How important is COORDINATING THE WORK AND ACTIVITIES OF OTHERS to the performance of your current job?
* (d) Encouraging and building mutual trust, respect, and cooperation among team members. 
* How important is DEVELOPING AND BUILDING TEAMS to the performance of your current job? 
clear
insheet using "Work Activities.TXT", tab
* For both variables, I'm using the LEVEL scale (I could have used the IMPORTANCE scale)
**keep if scaleid=="LV"
keep if scaleid=="IM"
* Drop unnecessary variables and keep obs of interest
keep if elementname=="Communicating with Supervisors, Peers, or Subordinates" | elementname=="Establishing and Maintaining Interpersonal Relationships" | elementname=="Coordinating the Work and Activities of Others" | elementname=="Developing and Building Teams"
keep onetsoccode elementname datavalue
gen j = 1 if elementname=="Communicating with Supervisors, Peers, or Subordinates"
replace j=2 if elementname=="Establishing and Maintaining Interpersonal Relationships"
replace j=3 if elementname=="Coordinating the Work and Activities of Others"
replace j=4 if elementname=="Developing and Building Teams"
drop elementname
reshape wide datavalue, i(onetsoccode) j(j)
rename datavalue1 communication
rename datavalue2 relationships
rename datavalue3 coordinating
rename datavalue4 developing
sort onetsoccode
merge onetsoccode using temp
drop _
sort onetsoccode
save temp, replace

* From "Work Context" file.
* Of the next three variables, only "responsibility" directly measures the effect of complementarity on output and team productivity.
* (5) Contact With Others (4.C.1.a.4)
* From the questionnaire: How much contact with others (by telephone, face-to-face, or otherwise) is required to perform your current job?
* (6) Responsibility for Outcomes and Results (4.C.1.c.2)
* From the questionnaire: How responsible are you for work outcomes and results of other workers on your current job?
* (7) Impact of Decisions on Co-workers or Company Results (4.C.3.a.2.a)	
* From the questionnaire: How do the decisions an employee makes impact the results of co-workers, clients or the company?
clear
insheet using "Work Context.TXT", tab
* This is the measure we'll use for teamwork intensity at the occupation level
keep if scaleid=="CX"
* Drop unnecessary variables and keep obs of interest
keep if elementname=="Contact With Others" | elementname=="Responsibility for Outcomes and Results" | elementname=="Impact of Decisions on Co-workers or Company Results"
keep onetsoccode elementname datavalue
gen j = 1 if elementname=="Contact With Others"
replace j=2 if elementname=="Responsibility for Outcomes and Results"
replace j=3 if elementname=="Impact of Decisions on Co-workers or Company Results"
drop elementname
reshape wide datavalue, i(onetsoccode) j(j)
rename datavalue1 contact
rename datavalue2 responsibility
rename datavalue3 impact
sort onetsoccode
merge onetsoccode using temp
drop _
sort onetsoccode
save temp, replace
****************************************************************************
* Match the resulting data to the Census Occupational classification. 
* The link between the two classifications is very straightforward and explained in the pdf "Updating Taxonomy Summary".
gen occsoc5 = substr(onetsoccode,1,7)
sort onetsoccode occsoc5 
* Some 6 digit soc codes are repeated after substracting the last 2 onet digits. Identify these.
duplicates tag occ, gen(duplicate)
order dup occsoc5 onetsoccode
sort duplicate occsoc5 onetsoccode
* Duplicated soc codes correspond to a 8 digit onet code ending in 0X. 
* These codes represent the soc occupation and any subcategory should be dropped (again, for details see "Updating Taxonomy Summary.pdf")
gen d = 1 if (substr(onetsoccode,-2,.)=="01" | substr(onetsoccode,-2,.)=="02" | substr(onetsoccode,-2,.)=="03" | substr(onetsoccode,-2,.)=="04") & duplicate!=0
egen dd = mean(d) if duplicate!=0, by (occsoc5)
drop if d==. & dd==1
drop d dd
egen teamsoc = mean(teamworkonet), by (occsoc5)
drop duplicate onetsoccode teamworkonet

* To improve number of matches with the Census soc codes, aggregate onet codes and calculate teamwork intensity
* for 4 and 5 digits and perform a merge to Census at 3, 4, 5 AND 6 digits
sort occsoc5 
save temp, replace
count

foreach n of numlist 5/3 {
	replace occsoc5 = substr(occsoc5,1,`n'+1)
	egen teamsoctemp = mean(teamsoc), by (occsoc5)
	egen contacttemp =mean(contact), by (occsoc5)
	egen responsibilitytemp = mean(responsibility), by (occsoc5)
	egen impacttemp = mean(impact), by (occsoc5)
	egen communicationtemp = mean(communication), by(occsoc5)
	egen relationshipstemp = mean(relationships), by (occsoc5)
	egen noindependencetemp = mean(noindependence), by (occsoc5)
	egen teammembertemp = mean(teammember), by (occsoc5)
	egen impactofdecisionstemp = mean(impactofdecisions), by (occsoc5)
	egen frequencytemp = mean(frequency), by (occsoc5)
	egen coordinatingtemp = mean(coordinating), by (occsoc5)
	egen developingtemp = mean(developing), by (occsoc5)

	drop teamsoc contact responsibility impact communication relationships noindependence teammember frequency impactofdecisions coordinating developing
	duplicates drop
	count
	rename teamsoctemp teamsoc
	rename contacttemp contact
	rename responsibilitytemp responsibility
	rename impacttemp impact
	rename communicationtemp communication
	rename relationshipstemp relationships
	rename noindependencetemp noindependence
	rename teammembertemp teammember
	rename impactofdecisionstemp impactofdecisions
	rename frequencytemp frequency
	rename coordinatingtemp coordinating
	rename developingtemp developing

	append using temp
	sort occsoc5 
	save, replace
	count
}

* Finally, add zero at the end of the codes because this is how soc codes look in the Census.
gen indselect=1 if  length(occ)==6
replace indselect=0 if length(occ)==7 /* Indicator for original data average (when avaialble) or data average computed by us */
replace occsoc5 = plural(2, occ, "+0") if length(occ)==6
replace occsoc5 = plural(2, occ, "+00") if length(occ)==5
replace occsoc5 = plural(2, occ, "+000") if length(occ)==4

* Last check: make sure that there is only one value for each occsoc5
sort occsoc5 
egen groupocc=group(occsoc5)
sort groupocc
by groupocc: egen occsoc5mean1=mean(responsibility)
by groupocc: egen occsoc5mean2=mean(noindependence)
by groupocc: egen occsoc5mean3=mean(teammember)
replace responsibility=occsoc5mean1
replace noindependence=occsoc5mean2
replace teammember=occsoc5mean3

save temp, replace
* Keep only variables used in the cross-country analysis
keep occsoc5 responsibility noindependence teammember
sort occsoc5 
save "ONET-occsoc5-a.dta", replace 

