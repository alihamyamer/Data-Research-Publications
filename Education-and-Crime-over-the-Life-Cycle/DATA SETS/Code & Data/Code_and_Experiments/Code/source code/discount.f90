real(long) function discount(tasso,periodo,lage,lflag)

	use global1
	use global2

	implicit none

	real(long), intent(in) :: tasso
	integer, intent(in) :: periodo, lage, lflag

	! Locals
	integer :: i, j
	real(long) :: survive

	SELECT CASE (lflag)

	CASE (1)	! BITA times SURVIVAL PROBABILITIES (STARTING FROM AGE)

	discount =  1.d0 ; survive = 1.d0
	do i=1,periodo-1
	survive = survive * surprob(lage+i-1)
	discount = discount + (tasso**i) * survive
	enddo

	CASE (2)	! Dicount factor for multiple periods

	discount = 0.d0
	do i=1,periodo
	discount = discount + (1.d0/(1.d0+tasso))**i
	enddo

	CASE (3)	! Composed Survival Probability

	discount = 1.d0
	do i=1,periodo
	discount = discount*surprob(lage+i-1)
	enddo

	END SELECT


end function discount