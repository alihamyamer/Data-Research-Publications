subroutine nllsfun(nt, xt, sum_v_err)
! Computes maximand (sum of squared deviations from target moments) for
! calibration
	use global1
	use global2
	use global3
    implicit none
    

integer, intent(in) :: nt
real, dimension(nt), intent(in) :: xt
real, intent(out) :: sum_v_err

integer, parameter:: mvt=12
real, dimension(mvt) :: v, vtar, v_err, weight
integer :: ii,i
real :: wnz

    calib_iteration=calib_iteration+1
    print*, "*****************************"
    print*, "Calibration Iteration:", calib_iteration
    print*, "*****************************"
  
  print*, nt
  print*, xt
  
    ! (1) Rescale parameters back to original values before updating them
    ! (2) Update Calibration Parameters
    open (unit=38, file=trim(drive)//trim(path1)//TRIM(path2)//"calib_params.txt",action='write',position='REWIND')
    write(38,*) xt(1)/parscale(1)        ," !wmin "
    write(38,*) xt(2)/parscale(2)        ," !bita  "
    write(38,*) xt(3)/parscale(3)        ," !gamma_1 "
    write(38,*) xt(4)/parscale(4)        ," !gamma_2 "
    write(38,*) xt(5)/parscale(5)        ," !eta "
    write(38,*) xt(6)/parscale(6)        ," !cbarshare "
    write(38,*) xt(7)/parscale(7)        ," !chi_l  "   
    write(38,*) xt(8)/parscale(8)        ," !chi_h "
    write(38,*) xt(9)/parscale(9)        ," !s_alpha "
    write(38,*) xt(10)/parscale(10)      ," !s_beta "
    write(38,*) xt(11)/parscale(11)      ," !corrind "
    WRITE(38,*) xt(12)/parscale(12)      ," !NOT USED"
        
    close(38)

    ! Call the main subroutine
    call schoolingOLG
    
    IF (failed_conv == 0) THEN    
    open (unit=34,  file=trim(drive)//trim(path1)//TRIM(path2)//"modelmoments.txt",action='read',position='REWIND')
    ! Update v_err
    do i = 1,mvt
        read(34,*)  v(i)
    enddo
       close(34)
    
    where (isnan(v))
        v = 0.0
    endwhere    
    
       
    ! Get Targets
    open (unit=36, file=trim(drive)//trim(path1)//TRIM(path2)//"targets.txt",action='read',position='REWIND')
    do i = 1,mvt
        read(36,*)  vtar(i)
    enddo
    close(36)
    
    ! Read error weights 
    open (unit=38, file=trim(drive)//trim(path1)//TRIM(path2)//"weights.txt",action='read',position='REWIND')
    do i = 1,mvt
        read(38,*) weight(i)
    enddo
    close(38)
    
    ! Compute v_err
    wnz=0
    do i=1,mvt
    v_err(i) = ((v(i)/vtar(i))-1)
    enddo
    
    
    sum_v_err = sum(v_err*v_err*weight)
    
    print *, " model moments  ", v
    print *, " sum_v_err      ",  sum_v_err
    

    
    if (calib_iteration==1)then
    open (unit=37, file=trim(drive)//trim(path1)//TRIM(path2)//"iteration.txt",action='write',position='REWIND')
    else
    open (unit=37, file=trim(drive)//trim(path1)//TRIM(path2)//"iteration.txt",action='write',position='APPEND')
    endif
    write(37,*) calib_iteration          ,"  calibration iteration"
    write(37,*) xt(1)/parscale(1)     ,"  !wmin "
    write(37,*) xt(2)/parscale(2)     ,"  !beta"
    write(37,*) xt(3)/parscale(3)     ,"  !gamma_1"
    write(37,*) xt(4)/parscale(4)     ,"  !gamma_2"
    write(37,*) xt(5)/parscale(5)     ,"  !eta"
    write(37,*) xt(6)/parscale(6)     ,"  !cbarshare"
    write(37,*) xt(7)/parscale(7)     ,"  !chi_l"
    write(37,*) xt(8)/parscale(8)     ,"  !chi_h"
    write(37,*) xt(9)/parscale(9)     ,"  !s_alpha"
    write(37,*) xt(10)/parscale(10)   ,"  !s_beta"
    WRITE(37,*) xt(11)/parscale(11)   ,"  !corrind"
    WRITE(37,*) xt(12)/parscale(12)   ,"  !NOT USED"
        
    write(37,*) '   '
    write(37,*) 'model moments:'
    write(37,*) v(1), " ! wealthzero - share of agents with wealth <=0"
    write(37,*) v(2), " ! wealth/income ratio"
    write(37,*) v(3), " ! share of newborns with zero wealth"
    write(37,*) v(4), " ! ratio of avg. bequest to avg. earnings"
    write(37,*) v(5), " ! victim_agg - aggregate victimization rate"
    write(37,*) v(6), " ! LTHSjailed_agg - share of prisoners with LTHS"
    WRITE(37,*) v(7), " ! NOT USED (zero weight in maximand)"
    write(37,*) v(8), " ! education regression coefficient"
    write(37,*) v(9), " ! ability regression coefficient"
    write(37,*) v(10)," ! age regression coefficient"
    write(37,*) v(11)," ! coefficient of variation for bequests"
    write(37,*) v(12)," ! regression constant coefficient"
     
    write(37,*) '   '
    write(37,*) 'Errors:'
    write(37,*)  v_err(1)  , "Target = 0.15 ! wealthzero - % of agents with wealth <=0"
    write(37,*)  v_err(2)  , "Target = 2.3  ! wealth/income ratio"
    write(37,*)  v_err(3)  , "Target = 0.095! share of newborns with zero wealth"
    write(37,*)  v_err(4)  , "Target = 1.27 ! ratio of avg. bequest to avg. earnings"
    write(37,*)  v_err(5)  , "Target = 5.6  ! victim_agg-aggregate victimization rate"
    write(37,*)  v_err(6)  , "Target = 0.53 ! LTHSjailed_agg-% of prisoners with LTHS"
    WRITE(37,*)  v_err(7)  , "Target = 0    ! NOT USED (zero weight in maximand)"
    write(37,*)  v_err(8)  , "Target =-6.33 ! education regression coefficient "
    write(37,*)  v_err(9)  , "Target = 0.044! ability regression coefficient "
    write(37,*)  v_err(10) , "Target =-0.71 ! age regression coefficient "
    write(37,*)  v_err(11) , "Target = 0.75 ! coefficient of variation for bequests"
    write(37,*)  v_err(12) , "Target = 25.2 ! constant regression coefficient"
    write(37,*)  sum_v_err      , "   squared error in NLLS"
    write(37,*) '  '
    close(37)

 ELSE  ! FAILED_CONV=1. LEISURE TERMS DID NOT CONVERGE
    IF (calib_iteration==1) THEN
        OPEN (unit=37, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"iteration.txt",action='write',position='REWIND')
    ELSE
        OPEN (unit=37, file=TRIM(drive)//TRIM(path1)//TRIM(path2)//"iteration.txt",action='write',position='APPEND')
    ENDIF
    write(37,*) calib_iteration          ,"  calibration iteration"
    WRITE(37,*) xt(1)     ,"  !wmin "
    WRITE(37,*) xt(2)     ,"  !beta"
    WRITE(37,*) xt(3)     ,"  !gamma_1"
    WRITE(37,*) xt(4)     ,"  !gamma_2"
    WRITE(37,*) xt(5)     ,"  !eta"
    WRITE(37,*) xt(6)     ,"  !cbarshare"
    WRITE(37,*) xt(7)     ,"  !chi_l"
    WRITE(37,*) xt(8)     ,"  !chi_h"
    WRITE(37,*) xt(9)     ,"  !s_alpha"
    WRITE(37,*) xt(10)    ,"  !s_beta"
    WRITE(37,*) xt(11)    ,"  !corrind "
    WRITE(37,*) xt(12)    ,"  !NOT USED"
    
    WRITE(37,*) '   '

    WRITE(37,*) " LEISURE DID NOT CONVERGE!"
    WRITE(37,*) '  '
    CLOSE(37)

ENDIF

return
end subroutine nllsfun
