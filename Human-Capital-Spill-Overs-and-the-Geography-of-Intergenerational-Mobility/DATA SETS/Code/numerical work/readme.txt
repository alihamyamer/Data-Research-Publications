To replicate the model calibration (Table 3) proceed as follows:

1)	Ensure that lines 76 to 86 of subs.f90 are as follows:

! Industry Parameters and data
open(unit=38, file="textfiles/gamma/USgam.txt",action='read',position='rewind')
read(38,*),gam1
close(unit=38)
open(unit=38, file="textfiles/capitalshare/UScap.txt",action='read',position='rewind')
read(38,*),cap1
close(unit=38)
open(unit=38, file="textfiles/taxes/US.txt",action='read',position='rewind')
read(38,*),tax
read(38,*),subrat
close(unit=38)

2)	Compile global.f90, subs.f90, sminpack.f and zufall.f in Visual Studio and run in release mode without debugging. This should take several minutes. We used intel compilers on a Windows Server 2008 machine.

Parameters will be read from the text file parameters.txt, contained in the textfiles folder.

3)	Upon convergence (and at every iteration) the following stats will be printed on the screen. These are the calibration targets from Table 3.

IGE – [Inter-Generational Earnings Elasticity]
ies – [Ratio of Post-tas to Pre-tax earnings]
z-y – [sigma(ln z)/sigma(ln y)]
std(log(earn)) – [sigma(ln y)]
reg – [cov(sigma_n(ln y),ONet_n)]
mT – [M_T]
pvt ed spd – [% of GDP as Private Edu Spending]

Upon convergence these statistics will match the calibration stats reported in table 3. Note that E[z]=1 by construction in the code








To replicate the counterfactual experiments (Table 5) proceed as follows:
•	There are two dimensions along which the experimental results vary. The first is the country, and the second is the experiment version. These variables are changed by editing lines 76-86 of subs.f90. 

-	To replicate column 2 (Output Shares) edit line 77 of subs.f90. For example, to use the Canadian output shares replace 

open(unit=38, file="textfiles/gamma/USgam.txt",action='read',position='rewind')

with

open(unit=38, file="textfiles/gamma/CANgam.txt",action='read',position='rewind') .


-	To replicate column 3 (Output and Capital Shares) edit lines 77 and 80 of subs.f90. For example, to use the Finnish output and capital shares replace (line 77)

open(unit=38, file="textfiles/gamma/USgam.txt",action='read',position='rewind')

with

open(unit=38, file="textfiles/gamma/FINgam.txt",action='read',position='rewind')

and	replace (line 80)

open(unit=38, file="textfiles/capitalshare/UScap.txt",action='read',position='rewind')

with

open(unit=38, file="textfiles/capitalshare/FINcap.txt",action='read',position='rewind')


-	To replicate column 4 (Observed Policies and Shares) edit lines 77, 80 and 83 of subs.f90. For example, to use the Norwegian policies and shares USgam.txt should be replaced with NORgam.txt at line 77, UScap.txt should be replaced with NORcap.txt at line 80, and US.txt should be replaced with NOR.txt at line 83.

-	To replicate column 5 (Observed Policies Only) edit line 83 of subs.f90 only. So lines 77 and 80 should include USgam.txt and UScap.txt, but line 83 should refer to the country in question, i.e. FRA.txt to replicate France column 5.

•	With the correct input data being called one can recompile the source code and run it to convergence. This can take up to an hour per experiment.

