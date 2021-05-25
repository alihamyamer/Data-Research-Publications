Elasticity value = 5 (GE)

Goal: General equilibrium high school subsidy for high elasticity of
substitution between high school and college

File changes: 

- fixed_params.txt
  - recalibrated various parameters
  - 1	! polcosting
  - 0	! y_wealth
  - 0	! tfpnorm
  - 0	! ucost
  - 2	! subregime
  - 1	! tuitquant
  - 0	! cbarflag 
  - 0	! emmeflag
- in_conditions.txt
  - wmin = -.49 


Code change : See global1.f90 REAL(long), PARAMETER :: jamma = 0.8d0	
