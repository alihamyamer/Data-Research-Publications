!!! Summary !!!

This folder contains the experiments for Table 11.
The correspondences between the subfolders and columns are
FOLDER		COLUMN
------		----------------------
Benchmark	column (1) in Table 11
HSsubPE		column (2) in Table 11
HSsubGE		column (3) in Table 11
HSsubGE2	column (4) in Table 11
prisGE		column (5) in Table 11 and column (4) in Table 14
prisGE2		column (6) in Table 11


!!! Implementation !!!

NOTE: There is a detailed Readme.txt in each individual subfolder.
      Here is just a brief overview.
FOLDER		What's Changed
------		----------------------
HSsubPE		fixed_params.txt
HSsubGE		fixed_params.txt
HSsubGE2	fixed_params.txt
prisGE		fixed_params.txt
prisGE2		fixed_params.txt


!!! How to read the relevant information from output files !!!

Table 11 and Table 14 report a lot of results for each experiment.
Here are the correspondences:
OUTPUT			Corresponding Line In File	File
------			----------------------		-------
Crime Victimization	victimization rate		select_output.txt
Arrest Rate HSD		Arrest rate in edu       1	save_output.txt
Arrest Rate HSG		Arrest rate in edu       2	save_output.txt
HSD share of crim.	Line 6				modelmoments.txt
Output			Gross output			select_output.txt
Agg. Consumption	Net output			select_output.txt
Welfare			Cons. eq. of ex ante welfare	select_output.txt
Prison Expenditure	Aggregate Prison Costs		select_output.txt
Subsidy Expenditure	Total Transfer Expenditures	save_output.txt
Price HSD/HSG/CG	Price of edu 1/2/3		select_output.txt

NOTE: Some results reported in the tables are expressed as a share of benchmark.
