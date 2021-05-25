subroutine Complex_calibration
! Calibration subroutine (complex method, using IMSL BCPOL subroutine)
	use global1
	use global2
	use global3
	use numerical_libraries
    implicit none
    
    integer :: i
    real :: XLBND(n),XUBND(n),XG(n)
    real :: sol(n),fsol
    external :: nllsfun
 
    
    ! Starting Values for Calibration Parameters


    open (unit=38, file=trim(drive)//trim(path1)//TRIM(path2)//"calib_start.txt",action='read',position='REWIND')
    
    do i = 1,n   
        read(38,*) XG(i) 
    enddo
    close(38)
  

    ! Lower Boundaries
    ! read boundaries
    open (unit=34,  file=trim(drive)//trim(path1)//TRIM(path2)//"lower_b.txt",action='read',position='REWIND')
    do i = 1,n
        read(34,*)  XLBND(i)
    enddo
    close(34)
    open (unit=34,  file=trim(drive)//trim(path1)//TRIM(path2)//"upper_b.txt",action='read',position='REWIND')
    do i = 1,n
        read(34,*)  XUBND(i)
    enddo
    close(34)
    
    ! Rescale parameters to ensure equal step
    ! Rescaling factors
    parscale = 1.d0

!**************************************************************************************
! Section to run BCPOL: Amoeba type Complex (2n points) method from IMSL
  call BCPOL(nllsfun,n,XG,0,XLBND,XUBND,1.e-3,300,sol,fsol)
!**************************************************************************************  
                          
Print*, "solution:", sol

end subroutine Complex_calibration
