

module glob

implicit none

integer, parameter          ::  calib=0
integer, parameter          ::  optim=0

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! Model Parameters
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


integer, parameter			::	nind=31
real						::	lam(nind)       ! Industry substitutability
real						::  gam(nind)	    ! Industry aggregation weight
real                        ::  cap(nind)       ! Capital Share within industry
real                        ::  kap(nind)       ! Capital stock within industry
real                        ::  onet(nind)      ! O*Net Factor Scores
real                        ::  scalw(nind)     ! Scale wages to account for idiosyncratic risk
real						::	y(nind)		    ! Intermediate Industry Output
real                        ::  yy, yysv        ! Aggregate Output

integer, parameter          ::  nthta = 7
real                        ::  thta(nthta)
real                        ::  pi_thta(nthta,nthta)    ! Genetic Transmission Probabilities
real                        ::  pi_thta_lr(nthta)       ! Long-run Genetic Distribution

integer, parameter          ::  na=6
real                        ::  ag(na)

integer, parameter			::	nskl=70             ! Size of skill grid
real						::	hcap(nskl)		    ! Grid of skill levels
real                        ::  wage(nskl),w(nskl)  ! Skill-Wages
real						::  qq(nskl)		    ! Aggregate measures of skill-labor types
integer                     ::  indh(nskl)
real						::	q(nind,nskl)	    ! Industry measures of skill-labor types
real                        ::  qshr(nind)          ! Industry total employment share
real                        ::  qshr_emp(nind)      ! Industry total employment share
real                        ::  rwage(nind)         ! Relative Industry Wage
real                        ::  varwage(nind)        ! Relative Industry Wage
real                        ::  cvwage(nind)        ! Relative Industry Wage
real                        ::  rwage_emp(nind)     ! Relative Industry Wage
real                        ::  ypw(nind)           ! Output per worker
real                        ::  ypw_emp(nind)       ! Empirical Output per Worker

integer, parameter          ::  nz=7                ! Number if idiosyncratic shock points
real                        ::  zgrd(nz),pi_z(nz)   ! idiosyncratic shock grid

real    ::  cons(nz,nskl,na,nthta,nskl,na), util(nz,nskl,na,nthta,nskl,na)
real    ::  V(nz,nthta,nskl,na)
integer ::  adec(nz,nthta,nskl,na),hdec(nz,nthta,nskl,na)

real    ::  tax,subrat
real    ::  trs, z_mult, tfp, rho, scale, taxrev, trs_fac, sub, gov_res     !Read Parameters                
real    ::  welfare, IGESS, gov_yy, reg_lag

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! Simulation Parameters
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

integer, parameter   :: nsim=10000
real                 :: hcaps(nsim)
real                 :: zsims(nsim), wagesim(nsim)
integer              :: hcapi(nsim),industry(nsim),industryp(nsim)
real                 :: QQs(nind), QQs_sav(nind), scalT(nind), Qs(nind,nskl)
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
! Preferences
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




real, parameter             :: sigma=2.0
real, parameter             :: beta=0.5
real                        :: del
real, parameter             :: cmin=0.000001
real, parameter             :: kret=6.85
real                        :: a1, a2

end module glob
