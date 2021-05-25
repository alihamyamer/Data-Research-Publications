*** JUNE 10, 2016. This file describes steps to reproduce the empirical results and graphs in the paper "Human Capital Spill-Overs and the Geography of Intergenerational Mobility" (authors: Brant Abbott and Giovanni Gallipoli.) ***

TO RUN THE FILES MAKE SURE TO APPROPRIATELY CHANGE THE WORKING DIRECTORY FROM ${USdir} TO YOUR CHOSEN DIRECTORY. 


(step 0) "merge_STAN_shares.do": this is a preliminary procedure which reads and merges the industry shares (value-added by ISIC3 classification) for different countries. Output file: Stan-Industry-Weights-2001-05-val.dta.
 
(step 1) "onet team work.do": this do file reads and organizes ONet measures of complementarity. Measures come from different ONET data files. Each occupation has a ONET occupation-code variable which is used for linking with Census occupation codes. Results are saved in output file "ONET-occsoc5-a.dta".

(step 2) "merge-ONET.do": this file merges ONet measures to CPS data set. This is necessary because only then we can have ISIC3 measures which are matched 
to occ-soc codes. The ISIC3 measures are what we use to match the ONet data to the STAN data (industry classification of the STAN is in ISIC3). 
In summary we have: (i) ONET => Census; (ii) Census => STAN. This means that we can recover: ONET => STAN. 
This do file also performs other actions. namely:
	(a) using ISIC identifiers, it generates industry specific measures of complementarity;
	(b) using STAN industry weights, it generates country-specific measures of skill complementarity;
	(d) it merges these measures with file "ige-country.dta", containing country-specific measurements of IGE (intergenerational elasticity of earnings);
	(e) it runs regressions and generates graphs, documenting correlation between skill-complementarity and IGE.

NOTE: You can select the sample of countries by changing the command "gen igeearn=igeearn_XYZ". This provides robustness checks of the relationship.
The alternatives are:
gen igeearn=igeearn_corak (core sample by Corak)
gen igeearn=igeearn_corak_low (low end estimates)
gen igeearn=igeearn_corak_high (high end estimates)
gen igeearn=igeearn_corak_plus5 (extended sample)

(step 3) "cross-country-robustness.do": this file produces plots documenting the correlation between education achievement (years of schooling) and complementarity (common factor, based on ONET proxies) across countries => Figure 6 in the paper.
NOTE: The data file "country_measures_all.dta" was produced by manually adding the Barro-Lee measures to the complementarity ONET measures. 


(step 4) "CPS.do": this do-file computes measures of industry-specific wage dispersion by ISIC3 industry category (ISIClab); this is done both for raw and residual wages. These alternative measures of complementarity are merged with the file containing STAN industry shares. This allows us to generate an index of skill complementarity for several countries (using only the wage dispersion measures as the proxy of skill complementarity).
Next, we perform a cross-country analysis just like we did using ONET proxies (regressions and graphs).

(step 5) "do_file_for_ISIClab_edu_complem.do". This file does the following:
-- it links the CPS (2000) to the ISIClab industry codes;
-- it merges the estimated complementarity for each industry (based on ONET common factor);
-- it computes average years of education of workers within each industry;
-- it produces the graph used in Figure 4 of the paper, documenting the link between complementarity and education achievement across industries;
    

(step 6) Analysis based on Luxembourg Income Study Database (LIS) data
*** Appendix A3: Cross-Country Patterns in Industry Wage and Income Dispersion ***
Appendix A3 documents that the ranking of 
within-industry wage dispersion follows a consistent pattern across countries 
by using the Luxembourg Income Study Database (LIS). The LIS provides a set 
of cross-sectional datasets describing household and individual income and
 other characteristics for a large number of countries and years. These
 datasets have been harmonized to make variables 
directly comparable across countries and years.
 
Unfortunately, while publicly available, the LIS data cannot be downloaded locally but can only be accessed remotely. The folder "ReplicationFiles" contains details instruction in the file "WageDispersionreplicationSteps.txt" (we also included a MS WORD version of these instructions). The instructions provide all the sequence of commands to remotely replicate the results based on LIS data.  

(step 7) The data needed to replicate Figure 5 is included in tables within the article. Specifically, columns 1 and 5 of Table 7 will reproduce the right-hand panel, and column 1 of Table 7 combined with data from Table 1 will reproduce the left-hand panel.





