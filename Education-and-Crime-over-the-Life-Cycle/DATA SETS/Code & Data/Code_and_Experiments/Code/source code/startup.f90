PROGRAM STARTUP
! Startup programme, selects calibration or experimental mode

use global1
use global2
use global3
implicit none

real :: calib_select

1 PRINT*, 'Enter 0 for experimental mode, or 1 for calibration mode'
  READ*, calib_select

IF (calib_select.ne.1.and.calib_select.ne.0) THEN
PRINT*, '  '
PRINT*, 'Don`t be silly!'
PRINT*, calib_select, 'is not 0 or 1'
PRINT*, '  '
GOTO 1
ENDIF


	!Read name of current Drive and Directory 
    open(unit=34,file="paths\drive.drv",action='read',position='rewind')
	read(34,*)  drive     !name of Drive in computer
    continue
	close(unit=34)
    drive = trim(drive)
    
    open(unit=34,file="paths\directory.drv",action='read',position='rewind')
	read(34,*)  path1     !name of path to code and working files
    continue
	close(unit=34)
    path1 = trim(path1)

    open(unit=34,file="paths\common-files.drv",action='read',position='rewind')
	read(34,*)  path2     !name of path to code and working files
    continue
	close(unit=34)
    path1 = trim(path1)

	!Read name of current experiment - Just prepare a subdir with appropriate name
    open(unit=34,file=trim(drive)//TRIM(path1)//TRIM(path2)//"expname.txt",action='read',position='rewind')
	read(34,*)  experiment     !name of experiment
    continue
	close(unit=34)
	parg  = trim(experiment)		! name of folder in which results are stored


calibselect = aint(calib_select)

SELECT CASE (calibselect)

    CASE(0)
        ! All parameters come from fixed_params.txt and in_conditions.txt
        print*,'***************************'
        print*, 'Solving the Model'
        print*,'***************************'

        ! Call main subroutine
        CALL schoolingOLG

    CASE(1)
        print*,'***************************'
        print*, 'Starting Calibration'
        print*,'***************************'

        ! Call calibration subroutine
        Call Complex_calibration

END SELECT


CALL CPU_TIME(T1)
PRINT*, "Time:", T1
PRINT*, "All Done"
!PAUSE
END PROGRAM STARTUP
