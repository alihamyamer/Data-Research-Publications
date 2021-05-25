subroutine gridsa

	use global1
	use global2

	implicit none

	real(long), external :: FW,ir
	
	! local variables
	integer :: i,j,jj,info
	! Parameters for grid spacing
	real(long) :: pw2, ascale, shft, wmin_save
	real(long), dimension(1) :: xw 
	real(long), dimension(lifet) :: nat_bor
    
    

				!!!!!!!!!!!!!!!!!!!!!!!!!!!
				! Constructing asset grid !
				!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Construct natural borrowing limit for pensioners 
    ! (towards end of life it is tighter than ad hoc one)
    ! natbor(age) is the minimum stock of assets one can 
    ! have at the beginning of age. This is the natural
    ! borrowing constraint at age-1.
    nat_bor(:)=wmin
    nat_bor(lifet)=-penv(lifet)/(1.d0+rrate)
    do i= lifet-1,retage,-1
    nat_bor(i)=(nat_bor(i+1)-penv(i))/(1.0d0+ir(i))
    enddo

	! First grid point is wmin , second is PW1
	agrid(1,:) = wmin
	agrid(1,1:retage-1) = wmin
	agrid(1,retage:lifet) = max(wmin,nat_bor(retage:lifet)) ! 0.d0 !

	wmin_save=wmin
	do jj=1,lifet
	shft=-agrid(1,jj)+5.d-1 ! SHFT  cannot be smaller than wmin. 
	!The closer it is to wmin, the finer the grid around wmin
	wmin=agrid(1,jj)+shft
	wmax=wmax0
	wmax=wmax+shft
	    DO i=1,ngpa
		IF (wmin<=0.0D0) THEN
        agrid(i,jj)= EXP( LOG(0.1D-14) + (real(i,long)-1.0d0)/(real(ngpa,long)-1.0d0)*(LOG(wmax)-LOG(0.1D-14)) ) - SHFT
		agrid(i,jj)=agrid(i,jj) - ABS(wmin)
		ELSE
        agrid(i,jj)= EXP( LOG(wmin) + (real(i,long)-1.0d0)/(real(ngpa,long)-1.0d0)*(LOG(WMAX)-LOG(WMIN)) ) - SHFT
		END IF
		enddo
	END DO

    wmin=wmin_save
	continue

    continue

end subroutine gridsa


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(long) function fw (a)

	use global1
	use global2

	real(long) :: a

	a=min(max(1.d-7,a),100.d0)
	
	fw = wmax - agrid(1,1) - pw1*(1.d0 - exp(a) ** (ngpa-1)) / (1.d0 - exp(a))

	end function fw
