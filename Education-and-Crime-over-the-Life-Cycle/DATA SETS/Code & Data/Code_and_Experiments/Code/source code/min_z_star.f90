FUNCTION min_z_star(c)
! Called by a non-linear solver to find the lowest value of cash at hand z_min  
! for which the optimum is at the borrowing constrained. 
! If the lowest optimal value of future assets i_we_sol(1)=i_we_glob found by 
! the egm is larger than a_1, the borrowing constraint, z_min is the zero of
! u(z-a_1)+surprob(age)*bita*eval(a_1) = 
! = u(z-i_we_glob)+surprob(age)*bita*eval(i_we_glob)
! v_glob = surprob(age)*bita*(eval(i_we_glob)-eval(a_1))
USE global1 
USE global2
USE fun_putility
USE  ZREAL_INT

INCLUDE 'link_fnl_shared.h' 

IMPLICIT NONE
REAL(long), INTENT(in):: c 
REAL(long) :: min_z_star
REAL(long) :: c_sol, x_temp(1), x_guess(1)
REAL(long), EXTERNAL :: con_beq

IF (age.NE.beqage) THEN 
   IF (sigma==1.d0) THEN 
      min_z_star = c - exp(v_glob)*(c+agrid(1,age+1)-agrid(i_we_glob,age+1))
   ELSE
      min_z_star = c**(1.d0-sigma) - (c+agrid(1,age+1)-agrid(i_we_glob,age+1))**(1.d0-sigma) &
           &-(1.d0-sigma)*v_glob
   END IF
ELSE
   const_beq = c + MAX(0.d0,gamma_1**(1.d0/eta)*c**(sigma/eta)-gamma_2) + &
       agrid(1,age+1) - agrid(i_we_glob,age+1)
   x_guess = 1.d-2
   call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
        xguess=x_guess)     
   c_sol = x_temp(1)     
   min_z_star = utils(age,c,2,1) - utils(age,c_sol,2,1) - v_glob 
END IF

END FUNCTION min_z_star


