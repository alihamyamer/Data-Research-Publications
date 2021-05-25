subroutine steadystate (ftracking,vtracking)	! arguments in the order for DNEQBF or DUMPOL or DBCPOL
! 
    use global1
    use global2
    use procs
    use READ_parameters
    use decisions
    use numerical_libraries
	
	implicit none

	real(long), intent(out),dimension(gensize) :: ftracking,vtracking

	! Local variables
    real(long) :: temp, uncavgearn_lag
    real(long), allocatable :: wvec(:)
    real(8):: time0,time1,time2,delta_t,rtc
    integer :: i,j,	id, flagconv(nedu), refresh
    character(100) :: name, series, plot

	print *, ' '; print *, ' '

	! Initializations
	flagconv = 0; refresh=2
	
	rrate = irate ! INITIALIZE RATES OF RETURN
	MRK = rrate

    ! RUNNING THE ECONOMY: COMPUTE INDIVIDUAL DECISIONS, SIMULATE THEM AND AGGREGATE OUTCOMES
	iterazioni=0
	do while (iterazioni<199)

	iterazioni = iterazioni + 1
	
		print *, "    ********************************		  "
		print *, "         Iteration #" , iterazioni
		print *, "                "

!		PENSION: Set pension value for different ages
	if (pensind==0) then             
	penv(1:retage-1)=0.d0
    penv(retage+1:lifet) = penv(retage) ! Fixed amount (as read from file)
    elseif (pensind==1) then
	penvlag = penv
    penv(:) = 0.d0
    penv(retage:lifet) = pensreprate*UNCavgearn ! Fixed share of average earnings
    endif 

    ! CONSUMPTION IN JAIL - CBAR *****
    if (cbarflag==1) then
    ! 	Set cbar as % of av. wage (to keep same units when wages scaled by costanta)
 	cbar= cbarshare*UNCavgearn
    else if (cbarflag==0) then
    ! Keep CBAR constant to the one in the benchmark (use last recorded cbarshare and UNCavgearn)
    cbar=cbarshare*emme/((1+0.18d0/19*12)*0.67d0-cbarshare)
    continue
    endif   
        
    ! UPDATE QUASILINEAR UTILITY COST OF SCHOOLING 
    IF (UCOST==1) THEN	! Update the utility cost of schooling
    leisure_save=leisure    ! update "internal part" of the psychic cost (which enters continuation values)
        if (tfpnorm==0) then
        print *, "UCOST and TFPNORM are not consistent! Experiment or benchmark?"
        stop
        endif
    continue
    ELSE IF (UCOST==0) THEN
        if (tfpnorm==1) then
        print *, "UCOST and TFPNORM are not consistent! Experiment or benchmark?"
        stop
        endif
    continue
    ENDIF
    
    continue


    ! SAVE PRICES FROM PREVIOUS ITERATION
	wagetgelag = wagetge	! SAVE last iteration prices (after tax and depreciation)
	rratelag = rrate

	! ***************** UPDATE NEXT ITERATION PRICES, if required ***************** 
	if (iterazioni>1) then

	if (geneq==1) then	! Geneq=1 - solving for general equil. MPL (labour prices)

	if (dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))<dabs(-1.d0+MRHC(1)/wagetge(1)) .and. flagconv(1)<3) then
	refresh = 1
	elseif (dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))<dabs(-1.d0+MRHC(1)/wagetge(1)) .and. flagconv(1)>3-1) then
	refresh = 2
	elseif (dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))>dabs(-1.d0+MRHC(1)/wagetge(1)) .and. flagconv(2)<3) then
	refresh = 2
	elseif (dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))>dabs(-1.d0+MRHC(1)/wagetge(1)) .and. flagconv(2)>3-1) then
	refresh = 1
	endif

	SELECT CASE (refresh)
	CASE(1)
	wagetge(1)  = pctprice*MRHC(1)+(1.d0-pctprice)*wagetgelag(1)
	flagconv(1) = flagconv(1) + 1
	flagconv(2) = 0
	CASE (2)
	wagetge(3)  = pctprice*MRHC(NEDU)+(1.d0-pctprice)*wagetgelag(NEDU)
	flagconv(2) = flagconv(2) + 1
	flagconv(1) = 0
	END SELECT

    ! BOUNDS TO PRICES
    wagetge(1)      = min(scalewage,wagetge(1))
    wagetge(NEDU)   = min(2.d0*scalewage,wagetge(NEDU))

	! INTEREST RATE - updated or left fixed
	SELECT CASE (ZERONW) 
	CASE (0)
	rrate = pctint*MRK+(1.d0-pctint)*rratelag
	rrate=max(-0.02d0,min(rrate,0.125d0))
	if (MRK>0.12499d0 .or. MRK<-0.0199) then
	print*, "********* Beware: MRK is hitting a boundary!"	
	endif
	CASE (1)	! ZERONW = 1 if interest rates exogenous (zero net wealth)
	continue
	END SELECT

    ! TECHNOLOGY FUNCTION
	SELECT CASE (TECHOICE)
	CASE (0) ! SIMPLE CES
	wagetge(2)=(1.d0-taxn)*prodout*(1.d0-theta)*sh(2)*(HC(2)**(jamma-1.d0))*NN**(-jamma)
	CASE (1) ! COBB DOUGLAS
	wagetge(2)=(1.d0-taxn)*prodout*(1.d0-theta)*sh(2)/HC(2)
	END SELECT

    ! TFP ENDOGENOUS(NORMALIZED)OR EXOGENOUS
	if (tfpnorm/=1)		wagetge(2) = pctprice*MRHC(2)+(1.d0-pctprice)*wagetgelag(2)


	else if (geneq==0) then	! Solving for partial equilibrium (constant prices)
		! INTEREST RATE - updated or left fixed
	SELECT CASE (ZERONW) 
	CASE (0)
	rrate = pctint*MRK+(1.d0-pctint)*rratelag
	rrate=max(-0.02d0,min(rrate,0.125d0))	
	CASE (1)	! ZERONW = 1 if interest rates exogenous (zero net wealth)
	continue
	END SELECT
	continue
    
    else if (geneq==2) then   ! Solving for PE but stopping after just one iteration (ceteris paribus experiment in PE) 
    print*, "STOPPED after 1st iteration !"
    stop    
	endif   ! end if (geneq==1)

	endif   ! end if (iterazioni>1) 


	! ***************** END OF UPDATE OF NEXT ITERATION PRICES ***************** 

	! Reconstruct grid on Assets (wmin / wmax might change)
	CALL GRIDSA

	print *, "************************************************************************"
	print '(A19,TR7,A15,TR4,A18)', "  HC and PhK","Wagetge/rrate","Wagetge/rrate lag"
					do i=1,nedu
					print '(3(TR4 , F14.5))', HC(i) , MRHC(i), wagetgelag(i)
					enddo
					print '(3(TR4 , F14.5))', PhK, MRK, rratelag
	print *, "************************************************************************"


	! ********** Save Initial Conditions file **********
	CALL save_params  ! ********** 
    CALL save_leisure

    allocate(eval(nedu,lifet,ngpa,ngpz,ngpf,maxjail))
    allocate(edval(nedu,lifet,ngpa,ngpz,ngpf,maxjail))
    
	! COMPUTE OPTIMAL POLICIES !
	!***************************************************
	time0=rtc( )
	time1=rtc( )
	! Workers
	CALL DECISIONS_WRK
	
	! Students
	allocate(seval(nedu,sum(eduyr(:))+1,ngpa,ngpf,maxjail))
    allocate(sedval(nedu,sum(eduyr(:)+1),ngpa,ngpf,maxjail))
	CALL DECISIONS_ST
	time2=rtc( )
	delta_t=time2+time0-2.0d0*time1
	print *, 'Decision rules and value functions have been computed' ;	print *, ""
    print *, 'Elapsed time:' ,	delta_t

    
    uncavgearn_lag = UNCavgearn
	! SIMULATE ECONOMY AND COMPUTE MOMENTS
	CALL SIMUL(ftracking,vtracking)
	print *, 'Simulation has been computed' ;	print *, ""
	print*, '' ; print '(TR7 , A55)', "    Marginal Products after Taxes and Depreciation  "
	print '(TR7 , F15.7)' , MRHC	;	print*, ''
	print '(TR7 , F15.7)' , MRK	;	print*, ''


    ! CHECK FOR MARKET CLEARING
! ******* CHECK FOR MARKET CLEARING PRICES ******* 
 IF (failed_conv==0) THEN ! Market clearing only if leisure has converged
    IF (GENEQ==1 .AND. zeroNW==0) THEN  ! Market clear in both human and physical capital markets
    
    
       IF ( dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))<TOLPR .AND.&
            dabs(-1.d0+MRHC(1)/wagetge(1))<TOLPR .AND. &
            dabs(-1.d0+MRK/rrate)<TOLIR ) THEN
          ! CONVERGENCE IS ACHIEVED
          PRINT *, 'MRHC(1)', MRHC(1)	;	PRINT *, 'MRHC(NEDU)', MRHC(NEDU)
          DEALLOCATE(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
          ! ********** Save Initial Conditions file **********
          leisure_save = leisure
          uncavgearn = uncavgearn_lag
          CALL save_params  ! **********
          CALL save_leisure
          RETURN
          
       ELSE    ! NO CONVERGENCE
          DEALLOCATE(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR
          !  WEALTH-PLOT
          PRINT *, ' '
       ENDIF   ! END IF FOR CONVERGENCE


    ELSE IF (geneq==1 .AND. zeroNW==1) THEN ! Check for labor market CLEARING only (while rrate exogenous)

       if ( dabs(-1.d0+MRHC(NEDU)/wagetge(NEDU))<TOLPR .and.&
            dabs(-1.d0+MRHC(1)/wagetge(1))<TOLPR ) then
          print *, 'MRHC(1)', MRHC(1)	;	print *, 'MRHC(NEDU)', MRHC(NEDU)
          deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
          ! ********** Save Initial Conditions file **********
          leisure_save = leisure
          uncavgearn = uncavgearn_lag
          CALL save_params  ! ********** 
          CALL save_leisure
          return
       else     ! NO CONVERGENCE
          continue
          deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
          print *, ' '
       endif


    else if (geneq==0) then ! Labor prices held constant (partial equlibrium in labor market)

       if (zeronw>0) then  ! Exogenous interest rate (price of physical capital)
          if ( dabs(-1.d0+HClag(1)/HC(1))<TOLHC .and.&
               dabs(-1.d0+HClag(2)/HC(2))<TOLHC) then
             deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
             ! ********** Save Initial Conditions file **********
             leisure_save = leisure
             uncavgearn = uncavgearn_lag
             CALL save_params  ! **********
             CALL save_leisure
             return
          else     ! NO CONVERGENCE IN HUMAN CAPITAL AGGREGATES
             deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
             continue
          end if ! if DABS

       else 	! else if zeroNW<1 (endogenous interest rate)

          if ( dabs(-1.d0+MRK/rrate)<TOLIR .and.& 
               dabs(-1.d0+HClag(1)/HC(1))<TOLHC .and.&
               dabs(-1.d0+HClag(2)/HC(2))<TOLHC) then
             deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
             ! ********** Save Initial Conditions file **********
             leisure_save = leisure
             uncavgearn = uncavgearn_lag
             CALL save_params  ! **********
             CALL save_leisure
             return
          else    ! NO CONVERGENCE IN INTEREST RATES
             continue
             deallocate(wvecglobal) ! DEALLOCATE WEALTH VECTOR WHICH IS USED FOR WEALTH-PLOT
             print *, ' '
          endif   ! END IF CONVERGENCE IN INTEREST RATE
       endif	! End if zeroNW>0

       print *, ' '
	endif

ELSE
   iterazioni = 199
ENDIF  ! End if (Failed_conv==0)

	end do ! END OF do while (iterazioni<199)

    
end subroutine steadystate
