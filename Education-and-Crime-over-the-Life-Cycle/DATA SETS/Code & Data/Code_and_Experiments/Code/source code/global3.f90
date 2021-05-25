Module Global3
  
  USE global1
  implicit none

  integer :: calibselect, algselect
  integer, parameter :: n=12  !number of parameters which need to calibrate
  integer, parameter :: iprint=3
  real(long), dimension(n) :: X, parscale
  integer :: calib_iteration

  !Parameters for AMOEBA (Downhill simplex Method)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER :: ndim  ! number of parameters need to be calibrate
  INTEGER, PARAMETER:: NMAX = 20 !WATCH OUT: maximum number of dimensions
  
end module global3
