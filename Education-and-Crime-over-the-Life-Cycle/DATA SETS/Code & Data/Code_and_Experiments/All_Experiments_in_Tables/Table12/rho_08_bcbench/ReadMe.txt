Elasticity value = 5 (benchmark)

Goal: benchmark model for high elasticity of substitution between high school and college

File changes:  

- fixed_params.txt
  - recalibrated various parameters
- in_condition.txt
  - wmin = -.98
Code change : global1.f90 REAL(long), PARAMETER :: jamma = 0.8d0	
