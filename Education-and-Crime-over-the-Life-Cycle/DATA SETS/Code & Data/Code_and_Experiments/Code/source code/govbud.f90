subroutine govbud(lgaggeffs,laggwealth,lTransfexp,laggprexp,laggpens,lGexp,lTAXrev)
! COMPUTE GOVERNMENT BUDGET CONSTRAINT
	use global1
	use global2
!	use numerical_libraries
	implicit none

	real(long), intent(in) :: lgaggeffs(nedu,ngpst) , laggwealth, lTransfexp, laggprexp,laggpens
	real(long), intent(out) :: lGexp , lTAXrev

	!	Locals
	integer :: i,j
	real(long)	:: Krev , Lrev, temp, sumeffs(nedu), grossWBill, grossKBill

	! Compute sum of aggregate efficiency for each education group
	sumeffs = 0.d0
	do i=2,ngpst
	sumeffs(:) = sumeffs(:) + lgaggeffs(:,i)
	enddo

	grossKBill = (rrate/(1.d0-taxk))*laggwealth
	Krev = max(grossKBill*taxk,0.0d0)	! Revenues from taxation of capital income
	grossWBill = sum(sumeffs(:)*wagetge(:)/(1.d0-taxn))	! Gross Wage Bill (before taxn) 
	Lrev = grossWBill*taxn					! Revenues from taxation of labor income

	lTAXrev = Krev + Lrev		! Total tax revenues

	select case (polcosting)
case (0)
	lGexp	= lTAXrev - lTransfexp - lAggprexp - laggpens		! Non-valued expenditure
	Gexp = lGexp

	case (1)
	lGexp = Gexp

	taxn = taxn - (lTAXrev-lGexp-lTransfexp-lAggprexp-laggpens)/grossWBill

	! Revenues
	Lrev = grossWBill*taxn	! Revenues from taxation of labor income
	lTAXrev = Krev + Lrev						! Total tax revenues

	continue

	case (2)
	lGexp = Gexp
	taxk = taxk - (lTAXrev-lGexp-lTransfexp-lAggprexp-laggpens)/max(grossKBill*taxk,0.1d0)

	Krev = max(grossKBill*taxk,0.0d0)			! Revenues from taxation of capital income
	! Revenues
	Lrev = grossWBill*taxn	! Revenues from taxation of labor income
	lTAXrev = Krev + Lrev						! Total tax revenues

	end select

end subroutine govbud
