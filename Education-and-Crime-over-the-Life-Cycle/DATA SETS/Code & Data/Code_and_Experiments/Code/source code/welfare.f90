SUBROUTINE WELFARE(ftracking,vtracking)

use global1
use global2
use procs

implicit none


! locals
real(long),intent(in),dimension(gensize) :: ftracking,vtracking
real(long) :: avwexante,temp,aucd,gini,cum_discount,fel,con_eq
real(long), external :: discount
real(long),dimension(gensize) :: wsort,wdsc,wdscr
integer				 :: i
! Ex-ante expected welfare (before any uncertainty is realized)
avwexante = sum(vtracking)/real(gensize,long)

print *, "average ex ante welfare", avwexante

cum_discount = discount(bita,lifet,1,1)
temp = discount(bita,30,1,3)*( gamma_1*sum(gamma_2+wealthi_age1(:))**(1.d0-eta)/(1.d0-eta) )/real(gensize,long)

fel = ( avwexante - temp)/cum_discount

call PINV_UTIL(fel,con_eq)

print *, "Permanent consumption equivalent of ex ante welfare", con_eq

OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"save_output.txt",action='write',position='APPEND')  

WRITE(38,*),' '
WRITE(38,*),'***********************************'
WRITE (38,'(A40,F14.5)'), "Average ex ante welfare", avwexante
WRITE (38,'(A40,F14.5)'), "Consumption eq. of ex ante welfare", con_eq

CLOSE(unit=38) 		

OPEN (unit=38, file=TRIM(drive)//TRIM(path1)//TRIM(parg)//"select_output.txt",action='write',position='APPEND')  
WRITE (38,'(A40,F14.5)'),    "Average ex ante welfare           ", avwexante
WRITE (38,'(A40,F14.5)'),    "Cons. eq. of ex ante welfare      ", con_eq
do i=1,nedu
WRITE (38,'(A35,i5,F14.5)'), "Price of edu                 ", i, wagetge(i)
enddo

CLOSE (unit=38)                                                                    

END SUBROUTINE WELFARE