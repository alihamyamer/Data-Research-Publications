subroutine Demographics

	use global1
	use global2

	implicit none

					!!!!!!!!!!!!!!!!!!!
					! Local variables !
					!!!!!!!!!!!!!!!!!!!

	integer :: i
!	real(long) :: check

 			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Construct the survival probabilities for the ages covered in model       !
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
	surprob = 1.0d0 - morprobr(youngest : maxage)	! just select the age range of the model

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Rescale the survival probabilities up to bequest age                     !
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
   surprob(1:beqage) = 1.d0

			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! Compute size of population at each age !
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	popsize(1) = 1.0d0		! Every cohort has a normalized initial size of 1
	
		do i=1,lifet-1
			popsize(i+1) = surprob(i) * popsize(i)
		end do


! For the year 2000. Change popsize (based on Census changes -- Demographic Trends in the 20th century - Census 2000 special reports) :
!    popsize(1:10) =    popsize(1:10)*0.74d0
!    popsize(11:30)=    popsize(11:30)*1.09d0
!    popsize(31:50)=    popsize(31:50)*1.12d0
!    popsize(51:80)=    popsize(51:80)*1.10d0
    
	print '(A40,F10.7)', 'Population sizes sum up to  ', sum(popsize)

end subroutine Demographics

