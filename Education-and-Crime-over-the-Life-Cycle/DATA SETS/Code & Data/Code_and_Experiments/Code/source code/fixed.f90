module FIXED

 USE global1
 USE global2
 USE procs
 IMPLICIT NONE

CONTAINS


SUBROUTINE fixeffect(draw,li_fixed)

	real(long), intent(in) :: draw(gensize)
	real(long), intent(out) :: li_fixed(gensize)
	! Local
	integer :: flow , fhigh, i, countdown(ngpf-1),id,lnumab(ngpf-1)
	real(long) :: t,x,y,z,incrab(ngpf-1),labgroup(gensize),labshare(ngpf-1)

	IF (FIXIND==1) THEN	! CONTINUOUS FIXED EFFECTS

	! ASSIGN PEOPLE TO DIFFERENT ABILITY BINS (FROM 1 TO NGPF-1)
	labgroup=0; lnumab=0
	do id=1,gensize
		do i = 1,ngpf-1
		x=real(i)*(1.d0/real(ngpf-1))
		y=(real(i)-1.d0)*(1.d0/real(ngpf-1))
		if (draw(id) .gt. y .and.draw(id) .le. x) then
		labgroup(id) = i
		lnumab(i)=lnumab(i)+1
		continue
		endif
		enddo
	enddo

    ! Compute the share of people in different ability groups
    do i=1,ngpf-1
    labshare(i)=real(lnumab(i),long)/real(gensize,long)
    enddo

    ! COMPUTE ABILITY OF PEOPLE WITHIN ABILITY BINS
    countdown=lnumab
    incrab=0.d0
    do i = 1,ngpf-1
    incrab(i)=dabs(fgrid(i+1)-fgrid(i))/real(lnumab(i))
    enddo

	do id=1,gensize
        do i=1,ngpf-1
    if (labgroup(id)==i) then
    li_fixed(id) = fgrid(i)+incrab(i)*real(countdown(i))
    countdown(i)=countdown(i)-1
    continue
    endif
        enddo
    enddo

    continue

	ELSE IF (FIXIND==2) THEN !	DISCRETE BINS

	labgroup=0
	y=(0.5d0/real(ngpf-1))
	x=(1.0d0/real(ngpf-1))

	do id=1,gensize
		
		if (draw(id) .le. y) then
		labgroup(id) = 1 
		else if (draw(id) .gt. y .and.draw(id) .le. x) then
		labgroup(id) = 2
		continue
		endif
		
		do i = 2,ngpf
		x=real(i)*(1.d0/real(ngpf-1))
		y=(real(i)-1.d0)*(1.d0/real(ngpf-1))
		if (draw(id) .gt. y .and.draw(id) .le. x) then
		labgroup(id) = i+1
		continue
		endif
		enddo
	enddo

    ! Redefine the FGRID(:) values for the discrete case
    incrab(1)=dabs(fgrid(2)-fgrid(1))/3.d0

    do i=2,ngpf-1
    incrab(i)=dabs(fgrid(i+1)-fgrid(i))/2.d0
    enddo

    fgrid(1)=fgrid(1)+incrab(1)

    do i=ngpf,3,-1
    fgrid(i)=fgrid(i-1)+incrab(i-1)
    CONTINUE
    enddo

    fgrid(2)=fgrid(1)+incrab(1)
    CONTINUE

    ! ASSIGN THE ABILITY
   	do id=1,gensize
        do i=1,ngpf
        if (labgroup(id) == i) then
    li_fixed(id) = fgrid(i)
        endif
        enddo
    enddo


	ENDIF

    continue

END SUBROUTINE fixeffect


!_______________________________________________________________________________
SUBROUTINE fixcrim(draw,li_crim)

  USE qsort_c_module

	real(long), intent(in) :: draw(gensize)
	real(long), intent(out) :: li_crim(gensize)
	! Local
	integer :: flow , fhigh,i,ii,j,id,countdown(maxjail/2-1),lnumab(maxjail/2-1)
	real(long) :: t,x,y,z,incrab(maxjail/2-1),labgroup(gensize),labshare(maxjail/2-1)
    real(long), allocatable :: ldraw(:)
	INTEGER, ALLOCATABLE :: draw_ind(:), index(:)
	! ASSIGN PEOPLE TO DIFFERENT CRIME BINS (FROM 1 TO MAXJAIL/2-1)
	labgroup=0; lnumab=0
	do id=1,gensize
		do i = 1,maxjail/2-1
		x=sum(borncrim(1:i))
		if (i==1) then
		    y = 0.d0
		else
		    y=sum(borncrim(1:i-1))
		endif
		if (draw(id) .gt. y .and. draw(id) .le. x) then
        labgroup(id) = i
		lnumab(i)=lnumab(i)+1
		continue
		endif
		enddo
	enddo

    ! Compute the share of people in different crime groups
    do i=1,maxjail/2-1
    labshare(i)=real(lnumab(i),long)/real(gensize,long)
    enddo

    ! COMPUTE CRIME UTILITY FLOW OF PEOPLE WITHIN CRIME BINS
    countdown=lnumab
    incrab=0.d0
    do i = 1,maxjail/2-1
        incrab(i)=dabs(cgrid(i+1)-cgrid(i))/real(lnumab(i))
    enddo



    DO i=1,maxjail/2-1
      ALLOCATE (ldraw(lnumab(i)),draw_ind(lnumab(i)),index(lnumab(i)))
      j = 0
      DO id=1,gensize
        IF (labgroup(id)==i) THEN
          j = j+1
          ldraw(j) = draw(id)
          draw_ind(j) = id
         ENDIF
      ENDDO
      j=lnumab(i)
      index = (/ (i,i=1,j) /)
      CALL QsortC(ldraw,index)
      draw_ind = draw_ind(index)
      DO ii = 1,lnumab(i)
        li_crim(draw_ind(ii)) = cgrid(i)+incrab(i)*REAL(countdown(i))
        countdown(i)=countdown(i)-1
      ENDDO
      DEALLOCATE (ldraw,draw_ind,index)
    ENDDO


END SUBROUTINE fixcrim

END MODULE FIXED
