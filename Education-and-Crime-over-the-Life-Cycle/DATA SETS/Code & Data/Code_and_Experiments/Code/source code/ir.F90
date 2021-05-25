function ir(lage)
! This subroutine computes the interest rate with partial annuity adjustment
	use global1
	use global2

	implicit none

	integer, intent(in) :: lage
    real(long) :: ir  

    ir=((1.d0+rrate/(1.d0-taxK))*(ann/surprob(lage)+(1.d0-ann)) - 1.d0)*(1.d0-taxK)

end function ir 