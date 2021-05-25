module schoolchoice

	use global1
	use global2
	use procs

	implicit none

	contains

subroutine enrolment(ledu,lage,lwealthi,lifixed,ldraw,lstracking,lcr_st)

	integer,intent(in) :: ledu,lage
	real(long),dimension(gensize),intent(in) :: lwealthi,lifixed,ldraw,lcr_st
	integer,dimension(gensize),optional,intent(in) :: lstracking

	! Local variables
	integer :: id,i,j,l,k,i_zet,permvec(gensize),identi(gensize)
	real(long),dimension(ledu,gensize) :: zetai
	real(long),dimension(gensize) :: lstcash,lschship
	real(long),dimension(gensize) :: numero
	real(long),external :: transf,VfunctionENRL,discount
	real(long) :: ratio1,spread,lenrol,prelenrol, LPVtuit,valvec(ngpz,maxcrim+1),temp,marg1,marg2
	real(long),dimension(gensize) :: diffs,useobs,valwork,valstudy,difval1,difval2,useval1,useval2
	
    character(100) :: series1
    valstudy=0.d0
     
	! COMPUTE individual states for all cohort
   	LPVtuit=tuit(ledu) *(1.d0+DISCOUNT(ir(lage),eduyr(ledu+1)-1,lage,2)) ! Present Discounted Value of Tuition
	do id = 1,gensize
        identi(id)=id
    
		! Compute scholarship
		lschship(id)=transf(ledu,lage,lwealthi(id),medwealth,rndtuit(id),tuit(ledu))

		do i=1,eduyr(ledu+1)-1
		lschship(id)=lschship(id)+transf(ledu,lage,lwealthi(id),medwealth,rndtuit(id),tuit(ledu))/((1.d0+ir(lage+i-1))**i)
		enddo

	lstcash(id)=lwealthi(id)+lschship(id)-LPVtuit	! ** STUDENT CASH (CONDITIONAL ON BEING A STUDENT) **
	enddo

	! Expected value of working for all agents
	do id=1,gensize
	    valwork(id)=0.d0
        do i_zet=1,ngpz
            do j=0,maxcrim
            valvec(i_zet,j+1) = VfunctionENRL(ledu,lage,lwealthi(id),i_zet,i_fixed(id),j,lcr_st(id))
            enddo
            !Education choice before knowing realization of z shock
            !Shock is white noise upon entering the labour market
            valwork(id) = valwork(id)+zstat(i_zet,ledu)*maxval(valvec(i_zet,:)) 
        enddo    
        
        valstudy(id)= VfunctionENRL(ledu,lage,lstcash(id),1,lifixed(id),-1,lcr_st(id))+ldraw(id)
	    enddo

	do I=1,ngpf-1   ! Ability BINS

	if (ledu==1) then
!	TARGET HS dropouts Distribution among Workers by Ability Groups
	lenrol=enrol(I,1)
	else if (ledu==2) then
!	TARGET HS graduates Distribution among Workers by Ability Groups
	lenrol=enrol(I,2)
	prelenrol=enrol(I,1)
	endif


 	numero=0.0d0 ; j=1; diffs=-1.d14; ; difval1=-1.d14; difval2=-1.d14

    ! STUDY VS WORK DECISION
		do id=1,gensize
		continue

		if (lifixed(id)>=fgrid(I) .and. lifixed(id)<fgrid(I+1)) then

	! COMPARE EXPECTED VALUE OF CURRENTLY WORKING TO CURRENTLY STUDYING
	
	if ( valwork(id) > valstudy(id) ) then

				IF (LEDU==1) THEN
				numero(id)=1.d0
				ELSE IF (LEDU==2 .and. lstracking(id)==1) then
				numero(id)=1.d0
				ENDIF
	else
				continue
	endif

			IF (LEDU==1) THEN
			diffs(j)= valwork(id) - valstudy(id)
	        difval1(j)=valstudy(id)
	        difval2(j)=valwork(id)

			j=j+1
			ELSE IF (LEDU==2 .and. lstracking(id)==1) then
			diffs(j)= valwork(id) - valstudy(id)
	        difval1(j)=valstudy(id)
	        difval2(j)=valwork(id)

			j=j+1
			ENDIF
		endif

		continue
		enddo

	ratio1=sum(numero)/(real(gensize,long)*abgroup(I))

    permvec(:)=identi(:)    ! Initialize id vector, which is used for permutation
	CALL DSVRGP (gensize,diffs,diffs,permvec)
    continue
    CALL DPERMU (gensize,difval1,permvec,1,difval1)
    CALL DPERMU (gensize,difval2,permvec,1,difval2)

	continue

	l=1; useobs=0; useval1=0; useval2=0
	do k=1,gensize
		if (diffs(k)>-1.d12) then
		useobs(l) = diffs(k)
		useval1(l)= difval1(k)
		useval2(l)= difval2(k)
		l=l+1
		endif
	enddo

	if (ledu==1) then
	k=nint((1.d0-lenrol)*abgroup(I)*real(gensize,long))
	else if (ledu==2) then
	k=nint((1.d0-lenrol/(1.d0-prelenrol))*(1.d0-prelenrol)*abgroup(I)*real(gensize,long))
	endif

	continue
	k=max(k,1)
	spread=useobs(k)
	marg1=useval1(k)
	marg2=useval2(k)
	continue


! Save basic statistics of value spreads - marginfo(ngpf-1,2,5,5)
marginfo(I,LEDU,1,1)= sum(useobs(1:l-1))/real(l-1,long) ! Mean
marginfo(I,LEDU,1,2)=(sum(useobs(1:l-1)**2.d0)/real(l-1,long)-marginfo(I,LEDU,1,1)**2.d0)**.5d0 !SD
marginfo(I,LEDU,1,3)= minval(useobs(1:l-1))         ! Min
marginfo(I,LEDU,1,4)= maxval(useobs(1:l-1))       ! Max

marginfo(I,LEDU,2,1)= sum(useval1(1:l-1))/real(l-1,long)    ! Mean
marginfo(I,LEDU,2,2)= minval(useval1(1:l-1))          ! Min
marginfo(I,LEDU,2,3)= maxval(useval1(1:l-1))          ! Max
marginfo(I,LEDU,2,4)= marg1                 ! Value edu=1 of marginal agent

marginfo(I,LEDU,3,1)= sum(useval2(1:l-1))/real(l-1,long)    ! Mean
marginfo(I,LEDU,3,2)= minval(useval2(1:l-1))          ! Min
marginfo(I,LEDU,3,3)= maxval(useval2(1:l-1))          ! Max
marginfo(I,LEDU,3,4)= marg2                 ! Value edu=2 of marginal agent

!   Save leisure and relative size of quasilinear terms

	!*******************************************************************************
	IF (UCOST==1) THEN	! Update the utility cost of schooling

			IF (LEDU==1) THEN
!			leisure(I)=leisure(I)+spread
			leisure(I)=spread
			ELSEIF (LEDU==2) THEN
!			leisure(NGPF-1+I)=leisure(NGPF-1+I)+spread
			leisure(NGPF-1+I)=spread
			ENDIF
	continue
	END IF ! (UCOST==1) - Update the utility cost of schooling
	!*******************************************************************************
	
	continue

	enddo   ! DO I=1,ngpf-1

 end subroutine enrolment   

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_enrolment(ledu)

	integer,intent(in) :: ledu
    integer :: i
 
if (ledu==1) then
    print *, "Psychic costs for education HS: last two iterations"
    do i=1,ngpf-1
    print '(F17.2,F17.2,A25,I4)', leisure(i), leisure_save(i), " ability bin ", i
    enddo
    print*, "   "
    else if (ledu==2) then

    print *, "Psychic costs for education COL: last two iterations" 
    do i=ngpf,2*(ngpf-1)
    print '(F17.2,F17.2,A25,I4)', leisure(i), leisure_save(i), " ability bin ", i-ngpf+1
	enddo
    print*, "   "
 endif


    print *, "Value differences (mean,SD,min,max):"
    do i=1,ngpf-1
    print '(4F12.2,A6,I2)', marginfo(i,LEDU,1,1), marginfo(i,LEDU,1,2), &
    marginfo(i,LEDU,1,3), marginfo(i,LEDU,1,4), " bin ", i 
    enddo
    print *, "Value of studying (mean,min,max,marginal):"
    do i=1,ngpf-1
    print '(4F17.2,A6,I2)', marginfo(i,LEDU,2,1), marginfo(i,LEDU,2,2), &
    marginfo(i,LEDU,2,3), marginfo(i,LEDU,2,4), " bin ", i 
    enddo
    print *, "Value of working (mean,min,max,marginal):"
    do i=1,ngpf-1
    print '(4F17.2,A6,I2)', marginfo(i,LEDU,3,1), marginfo(i,LEDU,3,2), &
    marginfo(i,LEDU,3,3), marginfo(i,LEDU,3,4), " bin ", i 
    enddo


	continue


if (ledu==1) then
    OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='REWIND')
	WRITE(38,*),'***********************************'
    WRITE(38,*), "Psychic costs for education HS: last two iterations"
    do i=1,ngpf-1
    WRITE (38,'(F17.2,F17.2,A25,I4)'), leisure(i), leisure_save(i), " ability bin ", i
    enddo
    print*, "   "
else if (ledu==2) then
    OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND')
    WRITE(38,*), "Psychic costs for education COL: last two iterations" 
    do i=ngpf,2*(ngpf-1)
    WRITE (38,'(F17.2,F17.2,A25,I4)'), leisure(i), leisure_save(i), " ability bin ", i-ngpf+1
	enddo
    print*, "   "
endif
 
    WRITE(38,*), "Value of studying (mean, min, max, marginal):"
    do i=1,ngpf-1
    WRITE (38,'(4F17.2,A6,I2)'), marginfo(i,LEDU,2,1), marginfo(i,LEDU,2,2), &
    marginfo(i,LEDU,2,3), marginfo(i,LEDU,2,4), " bin ", i 
    enddo
    WRITE(38,*), "Value of working (mean, min, max, marginal):"
    do i=1,ngpf-1
    WRITE (38,'(4F17.2,A6,I2)'), marginfo(i,LEDU,3,1), marginfo(i,LEDU,3,2), &
    marginfo(i,LEDU,3,3), marginfo(i,LEDU,3,4), " bin ", i 
    enddo
  
    CLOSE (unit=38)

end subroutine write_enrolment   



end module schoolchoice
