Elasticity value = 2 (benchmark)

Goal: benchmark model for low elasticity of substitution between high school and college

File changes:  

- fixed_params.txt
  - recalibrated various parameters
- in_condition.txt
  - wmin = -.49
Code change : global1.f90 REAL(long), PARAMETER :: jamma = 0.5d0	
