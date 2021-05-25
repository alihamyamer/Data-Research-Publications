module decisions


  USE global1
  USE global2
  USE procs
  USE fun_putility
  USE int_nodes
  USE, INTRINSIC:: ieee_arithmetic
  USE  ZREAL_INT
  IMPLICIT NONE
  
  INCLUDE 'link_fnl_shared.h' 

contains

SUBROUTINE decisions_wrk
  !DEC$ OBJCOMMENT LIB:'libiomp5md.lib'
  !#ifdef _OPENMP 
   include 'omp_lib.h'  !needed for OMP_GET_NUM_THREADS()
  !#endif
  

  REAL(long), EXTERNAL :: con_beq, min_z_star, transf, VfunctionD

 ! Local variables
 INTEGER :: i_we,i_we_min,i_we_max,i_we_last,i_we_guess,i_we_store,i_zet,edubase,ledu
 INTEGER :: i,ii,i1,i2,j,j_0,j_bar,countage,index,cr_st,max_loc
 INTEGER, DIMENSION(ngpa) :: i_we_sol, node 
 INTEGER, ALLOCATABLE, DIMENSION(:)  :: ind_arr
 
 ! Temporary storage matrix
 REAL(LONG) :: EXPVAL(nedu,1:retage-1,ngpa,2,ngpf,(ngpst-1)/2,maxjail)
    
 REAL(long) :: emucp,evaluep,fel1,gap,muc,wexcess,max_euler_err,v_1,v_2,v_bar,z_bar,z_iwe1,u_cr
 REAL(long), DIMENSION(ngpa) :: c_star,z_star,w_star,v_star,int_edval,c_eul	
 REAL(long) :: Vtemp,Ctemp,Stemp,Btemp,edval_jumpup,edval_jumpdown,c_guess,z_guess,w_guess,euler_err
 REAL(long) :: bound,c_bar,x_temp(1),x_guess(1)
 REAL(long), ALLOCATABLE, DIMENSION(:) :: c_temp,val_temp
 
 CHARACTER(100) :: series0,series1,series2,series3,series4
 LOGICAL, DIMENSION(ngpa-1)     :: mask

    x_guess = 1.d-2
     !   ** INITIALIZE POLICIES **
    CON=0.0d0
    max_euler_err = 0.0d0
    SAV = wmin
    SAV(:,lifet,:,:,:,:,:) = 0.0d0  ! LAST PERIOD OF LIFE

    ! INITIALIZE VALUE FUNCTION AND CONTINUATION VALUE AND THEIR DERIVATIVES
    VAL = -1.0d14     !(:,:,:,:,:,:,:)	current optimal consumption, labor, savings, value
    DVAL = INADERIV   !   initialize derivatives to the INADERIV conditions
    EVAL = -1.0d14 !(:,:,:,:,:,:)
    EDVAL = INADERIV
    
    
    ! BACKWARD SOLUTION for policies    

    ! LAST PERIOD OF LIFE ** WORKERS ** 
 
    AGE = LIFET ! Last period

    ! CATEGORIES OF ST_IND:
    ! 1 = student
    ! 2=crime/nonrobbed  
    ! 3=crime/robbed
    ! 4=nocrime/nonrobbed
    ! 5=nocrime/robbed
    ! 6=2 CRIMES/NONROBBED
    ! 7=2 CRIMES/ROBBED
    ! AND SO ON...... 

    ! CATEGORIES OF JAIL
    ! JAIL =ODD - not entering jail in current period
    ! JAIL =EVEN - enters in jail in current period
    ! MAXJAIL is # of states for state variable jail. 
    ! maxjail equals 2 (in jail/out of jail at beginning of period) times 
    ! the # of states for crime taste shock CRIMUT.  
    DO st_ind=2,NGPST  
        DO edu=1,NEDU
            DO ialp=1,NGPF
                DO i_zet = 1,NGPZ
           			DO jail=1,MAXJAIL
                        ! Set wages
	                    IF (age >= retage) THEN
	                        wg = 0.d0 + penv(age)	! Wage=0 IN retirement OR jail, pension paid to pensioners also in jail
	                    ELSE
                            wg= wagetge(edu)*hour(age,i_zet,jail,edu)*&
                            EXP(eff(edu,age)+abweight(edu)*fgrid(ialp))* &
                            rescale(st_ind)+shrob(st_ind)*UNCavgearn
                        ENDIF
                        DO i_we = 1,NGPA
                            wealth = AGRID(i_we,age)
    
	                        IF (MOD(jail,2)==1) THEN    ! Agents out of jail (i.e. optimizing cons) at beginning of period
                                con(edu,age,i_we,i_zet,ialp,st_ind,jail) = (1.d0+ir(age))*wealth+wg    ! last period consumption
                                con(edu,age,i_we,i_zet,ialp,st_ind,jail) = MAX(CLOW,con(edu,age,i_we,i_zet,ialp,st_ind,jail))
                                sav(edu,age,i_we,i_zet,ialp,st_ind,jail) = (1.d0+ir(age))*wealth+wg-con(edu,age,i_we,i_zet,ialp,st_ind,jail)

                                ! Compute Value Functions and its derivative CONDITIONAL ON BEING A WORKER TODAY
                                CALL PUTILITY(age,con(edu,age,i_we,i_zet,ialp,st_ind,jail),st_ind,jail,&
                                VAL(edu,age,i_we,i_zet,ialp,st_ind,jail))
                                call MUOC( age,con(edu,age,i_we,i_zet,ialp,st_ind,jail),muc)
                                DVAL(edu,age,i_we,i_zet,ialp,st_ind,jail)=muc*(1.d0+ir(age))    
                            ELSE    ! AGENTS ENDING THEIR LIFE IN JAIL
                                con(edu,age,i_we,i_zet,ialp,st_ind,jail)=cbar
                                sav(edu,age,i_we,i_zet,ialp,st_ind,jail)=(1.d0+ir(age))*wealth+wg

                                CALL PUTILITY(age,con(edu,age,i_we,i_zet,ialp,st_ind,jail),st_ind,jail,&
                                VAL(edu,age,i_we,i_zet,ialp,st_ind,jail))
                            ENDIF   ! End if (MOD(jail,2)==1)
                        ENDDO  ! end do wealth

   !For agents in jail we need to take finite difference of the value function as consumption is 
   !independent of assets
   if (mod(jail,2)==0) CALL NUMDERIV(ngpa,agrid(:,age),VAL(edu,age,:,i_zet,ialp,st_ind,jail),&
   DVAL(edu,age,:,i_zet,ialp,st_ind,jail))
   
                        
                    ENDDO   ! end do jail
                ENDDO   ! end do robbed
            ENDDO  ! end do i_zet
        ENDDO   ! end do ialp
    ENDDO   ! end do
 
   !   SOLVE FOR ALL remaning ages between LIFET-1 and 1 - NO STUDENTS
    DO ii=1,LIFET - 1  
     AGE = LIFET - ii !Current age to solve for policy functions
       countage=0 ! counting the # of approximations error which are above limit, by age
       wexcess=0.d0
 
       DO edu=1,nedu
          DO i_zet = 1,ngpz
             IF (age.GE.retage .AND. i_zet>1) THEN
                DO i = 2,ngpz
                   edval(edu,age+1,:,i,:,:)= edval(edu,age+1,:,1,:,:)
                   eval(edu,age+1,:,i,:,:) = eval(edu,age+1,:,1,:,:)
                   con(edu,age,:,i,:,:,:)  = con(edu,age,:,1,:,:,:)
                   sav(edu,age,:,i,:,:,:)  = sav(edu,age,:,1,:,:,:)
                   val(edu,age,:,i,:,:,:)  = val(edu,age,:,1,:,:,:)
                   dval(edu,age,:,i,:,:,:) = dval(edu,age,:,1,:,:,:)
                ENDDO
                EXIT
             ENDIF
call OMP_SET_NUM_THREADS(4)
!$OMP PARALLEL DEFAULT(private) &
!$OMP SHARED(age,edu,i_zet,bita,eta,gamma_1,gamma_2,psi,sigma,inada,inaderiv) & 
!$OMP SHARED(agrid,abweight,cgrid,clow,eff,hour,fgrid,penv,rescale,shrob) &
!$OMP SHARED(PI_A,PI_V,medwealth,tuit,prisT,prisTres)&
!$OMP SHARED(surprob,UNCavgearn,wagetge,cbar,ztrans) &
!$OMP SHARED(edval,eval,con,sav,val,dval) 
!$omp single
 i=0
    if (age==lifet-1 .and. edu==1 .and. i_zet==1) then
        i = OMP_GET_NUM_THREADS()!
        print *, ' We are using',i,' thread(s)'
    endif
!$omp end single

!$OMP DO 
             DO ialp=1,NGPF
                fixedeffect=fgrid(ialp)

                 DO jail=1,MAXJAIL
                   i_we_min = ngpa
                   i_we_max = 1
                   edval_jumpup = 0.d0
                   edval_jumpdown = INADERIV
                      
                   ! check whether value function is concave/differentiable or not
                   IF (MOD(jail,2)==1) THEN
                      st_ind =2 !Irrelevant here but used by evalue to distinguish between students/workers
                      DO i_we = 1,NGPA   
                         CALL emuc_evalue(edu,age,i_we,i_zet,ialp,jail,emucp,evaluep)                       
                         edval(edu,age+1,i_we,i_zet,ialp,jail) = emucp
                         eval(edu,age+1,i_we,i_zet,ialp,jail) = evaluep
                      END DO
                    

                      !-------------------------------------------------------------------
                      mask = ( edval(edu,age+1,2:ngpa,i_zet,ialp,jail) > &
                           edval(edu,age+1,1:ngpa-1,i_zet,ialp,jail) )
                      IF (COUNT(mask)>0) THEN 
                         !Non-concavity. Computes bounds of region.
                         edval_jumpup = MAXVAL(PACK(edval(edu,age+1,2:ngpa,i_zet,ialp,jail),mask))
                         edval_jumpdown = MINVAL(PACK(edval(edu,age+1,1:ngpa-1,i_zet,ialp,jail),mask))
                         IF (COUNT(edval(edu,age+1,:,i_zet,ialp,jail)>edval_jumpup)==0) THEN 
                            i_we_min=1
                         ELSE
                            i_we_min = MINLOC( edval(edu,age+1,:,i_zet,ialp,jail),1, & 
                                 edval(edu,age+1,:,i_zet,ialp,jail)>edval_jumpup )+1
                         END IF
                         IF (COUNT(edval(edu,age+1,:,i_zet,ialp,jail)<edval_jumpdown)==0) THEN 
                            i_we_max=ngpa
                         ELSE
                            i_we_max = MAXLOC(edval(edu,age+1,:,i_zet,ialp,jail),1, &
                                 edval(edu,age+1,:,i_zet,ialp,jail)<edval_jumpdown) - 1
                         END IF
                      END IF
                       
                       !2. Solve for cons and value fns on the endogenous grid points
                       !------------------------------------------------------------------
                       !   For people at age beqage (those who leave bequests), cash at hand  is
                       !   defined as net of the bequest; i.e. z_t = c_t+a_(t+1)
                       !   JAIL =odd is out of jail at end of current period
                       !   JAIL =even enters in jail in current period

                       ! Initializing policy and value fns for the egm algorithm
                       c_star = -1.0d11
                       z_star = -1.0d11
                       v_star = -1.0d11
                       i_we_sol = -11
  
                       ! Initializing parameters for the egm algorithm
                       index = 0
                       j_0 = 1 
                       j_bar = 0
                       z_bar = 10.d6
                       v_bar = edval_jumpup + 1.d-5

                       !2.1 Solves for the cons fun on the end. grid
                       !------------------------------------------------
                       DO i_we=1,ngpa

                          IF (i_we<i_we_min .OR. i_we>i_we_max) THEN  !if_1
                             ! Concave region - standard egm 
                             !----------------------------------
                             index = index + 1
                             CALL localegm(i_we,bita*surprob(age)*edval(edu,age+1,i_we,i_zet,ialp,jail),&
                               &c_star(index),z_star(index))
                             i_we_sol(index) = i_we
                          ELSE
                             !Non-concave region
                             !---------------------------------
                             IF (index>0) THEN 
                                !If the policy function has been solved for at least one point
                                !update lower bounds used to exploit monotonicity
                                const_beq =MAX(z_star(index)-agrid(i_we,age+1),1.d-5)
                                x_guess = c_star(index)
                                call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                                        xguess=x_guess)
                                CALL MUOC(age,x_temp(1),v_bar)
                                j_0 = i_we_sol(index) 
                             END IF
                             const_beq = z_bar-agrid(i_we,age+1)
                             x_guess = 1.d-2
                             call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                                   xguess=x_guess)
                             CALL MUOC(age,x_temp(1),bound)
          
                             !Exploit monotonicity to select candidate points 
                             IF (  ( v_bar>edval(edu,age+1,i_we,i_zet,ialp,jail) .AND.  &    
                                  edval(edu,age+1,i_we,i_zet,ialp,jail).GE.bound ) .OR. &
                                  (edval(edu,age+1,i_we,i_zet,ialp,jail)<bound .AND. i_we.GE.j_bar)  ) THEN !if_2
                                ! A. Solve for local max
                                !-----------------------
                                CALL localegm(i_we,bita*surprob(age)*edval(edu,age+1,i_we,i_zet,ialp,jail),&
                                     &c_guess,z_guess)
                                !B. Verify whether local max is global given zguess
                                !----------------------------------
!                                
                                ALLOCATE(c_temp(j_0:i_we_max))
                                DO j = j_0,i_we_max
                                   IF ( (z_guess - agrid(j,age+1)).LE.0 .AND. j>1) EXIT
                                   if (age.ne.beqage) then
                                      c_temp(j) = z_guess - agrid(j,age+1)
                                   else
                                      const_beq = z_guess - agrid(j,age+1)
                                      x_guess = 1.d-2
                                      call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                                           xguess=x_guess)
                                      c_temp(j) = x_temp(1)
                                   end if
                                END DO

                                ALLOCATE (val_temp(j_0:j-1))
                                CALL PUTILITY_ARR(age,c_temp(j_0:j-1),st_ind,jail,val_temp)
                                val_temp =  val_temp+ bita*surprob(age)*eval(edu,age+1,j_0:j-1,i_zet,ialp,jail)
                                max_loc = MAXLOC(val_temp,1)+j_0-1
                                DEALLOCATE (c_temp,val_temp)

                                IF (max_loc==i_we) THEN            !if_3
                                   !a_grid(i_we,age+1) is global max given z_guess
                                   index = index+1
                                   c_star(index) = c_guess
                                   z_star(index) = z_guess
                                   i_we_sol(index) = i_we
                                   z_bar = 10.d6
                                ELSEIF (max_loc>i_we) THEN 
                                   ! Store location of (approximate) global max given z_temp
                                   ! By monotonicity no need to consider points to its left.
                                   j_bar=max_loc
                                   z_bar = z_guess
                                END IF   !if_3
                             END IF      !if_2
                          END IF         !if_1
                       END DO  ! i_we

                       !2.2 Solves for the value fun on the end. grid
                       !-----------------------------------------------
                       CALL PUTILITY_ARR(age,c_star(1:index),st_ind,jail,v_star(1:index))
                       v_star(1:index) = v_star(1:index) + bita*surprob(age)*&
                            eval(edu,age+1,i_we_sol(1:index),i_zet,ialp,jail)
                       
                       !2.3 Correct the policy and value fns on the first
                       !    end. cash-at-hand node if the choice of future assets
                       !    is to the right of the borrowing constraint
                       !------------------------------------------------
                       
                       IF (i_we_sol(1)>1) THEN
                          c_star(2:ngpa) = c_star(1:ngpa-1)
                          z_star(2:ngpa) = z_star(1:ngpa-1)
                          i_we_sol(2:ngpa) = i_we_sol(1:ngpa-1)
                          v_star(2:ngpa) = v_star(1:ngpa-1)

                          ! Solve for the highest value of cash at hand for which the
                          ! borrowing constraint is binding 
                          i_we_glob = i_we_sol(1)
                          v_glob = bita*surprob(age)* &
                            (eval(edu,age+1,i_we_sol(1),i_zet,ialp,jail)-eval(edu,age+1,1,i_zet,ialp,jail))
                          x_guess = c_star(1) 
                          call d_zreal(min_z_star,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                            xguess=x_guess)
                          c_star(1) = x_temp(1) - 1.d-4
                          z_star(1) = x_temp(1) + agrid(1,age+1)
                          if (age==beqage) then 
                             z_star(1) = z_star(1) + &
                                MAX(0.d0,gamma_1**(1.d0/eta)*c_star(1)**(sigma/eta)-gamma_2)
                          endif
                          i_we_sol(1) = 1
                          CALL PUTILITY(age,c_star(1),st_ind,jail,v_star(1))
                          v_star(1) = v_star(1) + bita*surprob(age)* &
                                  eval(edu,age+1,1,i_zet,ialp,jail)
                          index = index + 1 
                       END IF



                       IF ( ANY( c_star(:)/=c_star(:) ).OR. ANY( ABS(c_star(:))>HUGE(c_star(:)) ) ) THEN
                          PRINT *,"c_star IS NAN at"
                          PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,&
                               "ialp=",ialp,"st_ind=",st_ind,"jail=",jail
                          STOP
                       ENDIF
                       
                       IF (ANY(v_star(1:index-1)-v_star(2:index)>1.d-5)) THEN
                          PRINT *,"V_STAR IS decreasing"
                          PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"jail=",jail
                          stop
                       ENDIF
                    END IF ! IF (MOD(jail,2)==1)

                    ! 3. Recover policy and value fn on the EXOGENOUS grid points 
                    !----------------------------------------------------------
                    DO st_ind=2,NGPST
                       Vtemp=-1.d17;   Ctemp=0.d0; Stemp=agrid(1,age+1)
                       IF (age >= retage) THEN
                          WG = 0.d0 + penv(age)	! Wage is zero at retirement and in jail, 
                          						!  pension is paid also to pensioners in jail
                       ELSE
                          WG= wagetge(edu) * hour(age,i_zet,jail,edu)*EXP(eff(edu,age)+&
                               abweight(edu)*fgrid(ialp))*rescale(st_ind)+shrob(st_ind)*UNCavgearn 
                          ! hour(age,i_zet,jail=2)=0. An individual apprehended loses labour income
                          ! and cannot be robbed but keeps income from crime
                       ENDIF
                       
                       IF ( MOD(jail,2)==1) THEN 
                          ! Recover beggining of period wealth from cash at hand for individuals out of jail
                          w_star(1:index) = (z_star(1:index)-wg)/(1.d0+ir(age))
                       ELSE 	!Individual with (MOD(jail,2)==0) i.e. in jail at beginning of period
                          CON(edu,age,:,i_zet,ialp,st_ind,jail)=cbar
                          SAV(edu,age,:,i_zet,ialp,st_ind,jail)=(1.d0+ir(age))*agrid(:,age)+wg 
                          DO i_we=1,ngpa
                            !!! !$OMP CRITICAL 
                             CALL VFUNC(edu,age,i_we,i_zet,ialp,st_ind,jail,val(edu,age,i_we,i_zet&
                                  &,ialp,st_ind,jail))
                            !!! !$OMP END CRITICAL     
                          ENDDO
                       ENDIF	!End if (MOD(jail,2)==1)

                       ! 2. For optimizing agent, interpolate to recover the policy and value functions on 
                       !    the exogenous grid points. Note that we need to exclude retired with st_ind>2 
                       !    who have already been assigned the same value as for st_ind=2
                       !         
                       IF (MOD(jail,2)==1) THEN 
                          call int_node(w_star(1:index),agrid(:,age),node)
                          node = node - 1
                          
                          CALL  PUTILITY (age,c_star(1),st_ind,jail,fel1)	
                          
                          if (age.ne.beqage) then
                             where (node==0)
                                ! Borrowing constrained individual
                                con(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     wg + agrid(:,age)*(1.d0+ir(age))-agrid(1,age+1)
                                val(edu,age,:,i_zet,ialp,st_ind,jail)=&
                                     utils(age,con(edu,age,:,i_zet,ialp,st_ind,jail),st_ind,jail) 
                                val(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     min(val(edu,age,:,i_zet,ialp,st_ind,jail),fel1)-fel1 +&
                                     v_star(1)
                             elsewhere
                                con(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     c_star(node)+ ( c_star(node+1)-c_star(node) )/&
                                     ( w_star(node+1)-w_star(node) ) *&
                                     ( agrid(:,age)-w_star(node) )
                                val(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     v_star(node)+ ( v_star(node+1)-v_star(node) )/&
                                     ( w_star(node+1)-w_star(node) ) *&
                                     ( agrid(:,age)-w_star(node) )
                             endwhere
                             sav(edu,age,:,i_zet,ialp,st_ind,jail)=agrid(:,age)*(1.d0+ir(age))+wg- &
                                  & con(edu,age,:,i_zet,ialp,st_ind,jail)
                          else ! Age=beqage. Bequests.
                             const_beq = 0.d0
                             x_temp = 1.d-2
                             do i=1,ngpa
                                if (node(i)==0) then
                                   !Borrowing constrained individuals
                                   !Solve for consumption using intratemporal condition coded in 
                                   !con_beq. const_beq is a global variable used by con_beq 
                                   const_beq = wg + agrid(i,age)*(1.d0+ir(age))-agrid(1,age+1)
                                   x_guess = x_temp
                                   call d_zreal(con_beq,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                                        xguess=x_guess)
                                   con(edu,age,i,i_zet,ialp,st_ind,jail) = x_temp(1)
                                   call putility(age,con(edu,age,i,i_zet,ialp,st_ind,jail),&
                                        st_ind,jail,val(edu,age,i,i_zet,ialp,st_ind,jail)) 
                                   val(edu,age,i,i_zet,ialp,st_ind,jail) = &
                                        min(val(edu,age,i,i_zet,ialp,st_ind,jail),fel1)-fel1 +&
                                        v_star(1)
                                else 
                                   exit     
                                endif
                             enddo
                             where (node>0) 
                                !Unconstrained individuals     
                                con(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     c_star(node)+ ( c_star(node+1)-c_star(node) )/&
                                     ( w_star(node+1)-w_star(node) ) *&
                                     ( agrid(:,age)-w_star(node) )
                                val(edu,age,:,i_zet,ialp,st_ind,jail) = &
                                     v_star(node)+ ( v_star(node+1)-v_star(node) )/&
                                     ( w_star(node+1)-w_star(node) ) *&
                                     ( agrid(:,age)-w_star(node) )
                             endwhere
                                sav(edu,age,:,i_zet,ialp,st_ind,jail)=agrid(:,age)*(1.d0+ir(age))+wg- &
                                  & con(edu,age,:,i_zet,ialp,st_ind,jail) -max(0.d0,gamma_1**(1.d0/eta)* &
                                  con(edu,age,:,i_zet,ialp,st_ind,jail)**(sigma/eta) - gamma_2)
                          endif
                          IF (ANY(con(edu,age,:,i_zet,ialp,st_ind,jail)/=&
                               con(edu,age,:,i_zet,ialp,st_ind,jail))) THEN
                             PRINT *,"CON IS NAN at"
                             PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                                  &,st_ind,"jail=",jail
                             STOP
                          ENDIF

                          !-------------------------------------------------------------------------------                                
                          ! For agents committing crime in the current period,
                          ! add quasi-linear utility from crime
                          if (age<retage) then
                             if (mod(jail,2)==1 .and. st_ind>1) then
                                if (st_ind==2 .or. st_ind==3 .or. st_ind>5) then
                                   cr_st = int((jail+1)/2)
                                   u_cr = cgrid(cr_st)
                                   !                               if (age<7) then
                                   !                               endif
                                   val(edu,age,:,i_zet,ialp,st_ind,jail)=&
                                        val(edu,age,:,i_zet,ialp,st_ind,jail)+u_cr     
                                endif
                             endif
                          endif


                          DO i_we=1,ngpa
                             CALL MUOC( age,con(edu,age,i_we,i_zet,ialp,st_ind,jail),muc)
                             DVAL(edu,age,i_we,i_zet,ialp,st_ind,jail) = muc*(1.d0+ir(age))     
                          END DO
                       ELSE !For non-optimizing agents we need to use finite differences of value fn
                          CALL numderiv(ngpa,agrid(:,age),val(edu,age,:,i_zet,ialp,st_ind,jail),&
                               & DVAL(edu,age,:,i_zet,ialp,st_ind,jail) )
                       ENDIF

                       IF (ANY(VAL(edu,age,:,i_zet,ialp,st_ind,jail)/=&
                            VAL(edu,age,:,i_zet,ialp,st_ind,jail))) THEN
                          PRINT *,"VAL IS NAN at"
                          PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                               &,st_ind,"jail=",jail
                          STOP
                       ENDIF

                       IF (ANY(DVAL(edu,age,:,i_zet,ialp,st_ind,jail)/=&
                            DVAL(edu,age,:,i_zet,ialp,st_ind,jail))) THEN
                          PRINT *,"DVAL IS NAN at"
                          PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                               &,st_ind,"jail=",jail
                          STOP
                       ENDIF
                       IF (ANY(VAL(edu,age,1:(ngpa-1),i_zet,ialp,st_ind,jail)-&
                            & VAL(edu,age,2:ngpa,i_zet,ialp,st_ind,jail)>1.d-5)) THEN
                          PRINT *,"VAL IS decreasing"
                          PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                               &,st_ind,"jail=",jail
                          stop
                       ENDIF

                    ENDDO   !end do st_ind

                 ENDDO ! end do jail
              ENDDO ! end do ialp
!$OMP END DO      
!$OMP END PARALLEL 
           ENDDO ! end do i_zet
        ENDDO   ! end do edu
     ENDDO   ! end do over age

     print *, "END OF WORKERS"


END SUBROUTINE decisions_wrk



!*********************DECISIONS_ST*******************************


SUBROUTINE decisions_st

  REAL(long), EXTERNAL :: con_beq, min_z_star, transf, VfunctionD

 ! Local variables
 INTEGER :: i_we,i_we_min,i_we_max,i_we_last,i_we_guess,i_we_store,i_zet,edubase,ledu
 INTEGER :: i,ii,i1,i2,j,j_0,j_bar,count1,countage,index,cr_st,max_loc
 INTEGER, DIMENSION(ngpa) :: i_we_sol, node 
 INTEGER, ALLOCATABLE, DIMENSION(:)  :: ind_arr
 
 ! Temporary storage matrix
 REAL(LONG) :: EXPVAL(nedu,1:retage-1,ngpa,2,ngpf,(ngpst-1)/2,maxjail)
    
 REAL(long) :: emucp,evaluep,fel1,gap,muc,wexcess,max_euler_err,v_1,v_2,v_bar,z_bar,z_iwe1,u_cr
 REAL(long), DIMENSION(ngpa) :: c_star,z_star,w_star,v_star,int_edval,c_eul	
 REAL(long) :: Vtemp,Ctemp,Stemp,Btemp,edval_jumpup,edval_jumpdown,c_guess,z_guess,w_guess,euler_err
 REAL(long) :: bound,c_bar,x_temp(1),x_guess(1)
 REAL(long), ALLOCATABLE, DIMENSION(:) :: c_temp,val_temp
 
 CHARACTER(100) :: series0,series1,series2,series3,series4
 LOGICAL, DIMENSION(ngpa-1)     :: mask

    x_guess = 1.d-2
     !   ** INITIALIZE POLICIES **
    CON(:,:,:,:,:,1,:)=0.0d0
    max_euler_err = 0.0d0
    SAV(:,:,:,:,:,1,:) = wmin


    ! INITIALIZE VALUE FUNCTION AND CONTINUATION VALUE AND THEIR DERIVATIVES
    VAL(:,:,:,:,:,1,:) = -1.0d14     !	current optimal consumption, labor, savings, value
    DVAL(:,:,:,:,:,1,:)=  INADERIV   !   initialize derivatives to the INADERIV conditions
    SEVAL = -1.0d14
    SEDVAL= INADERIV
    
    ! BACKWARD SOLUTION for policies    

   

!************************************************************************************************
! SOLVE FOR ALL STUDENTS 
!************************************************************************************************

                                !!!!!!!!!!!!!!!!
                                    st_ind= 1
                                    i_zet=1
                                !!!!!!!!!!!!!!!!

DO EDU = 2,1,-1
edubase=0
if (edu==2) edubase=eduyr(2) ! used to select ages at which we solve students' problem
    DO AGE = edubase+eduyr(edu+1),edubase+1,-1                                
    ledu=edu
    jaildo: DO jail=1,maxjail-1,2   ! Only ODD,because no student commits any crime 
                                !(and you don't go to jail if you are innocent!)
       DO ialp=1,ngpf
          fixedeffect=fgrid(ialp)

              
          i_we_min = ngpa+1
          i_we_max = 0
       
          IF (age==edubase+eduyr(edu+1)) ledu=edu+1  ! Increments education next period for 
          ! students in last year of any current education cycle
       
          IF (age==SUM(eduyr(:))) THEN  
             seval(ledu,age+1,:,ialp,jail) = 0.d0   
             sedval(ledu,age+1,:,ialp,jail) = 0.d0
             DO i1 = 1,ngpz  ! i1=i_zet 
                seval(ledu,age+1,:,ialp,jail) = seval(ledu,age+1,:,ialp,jail) + &
                     zstat(i1,edu+1)* eval(edu+1,age+1,:,i1,ialp,jail)        
                sedval(ledu,age+1,:,ialp,jail) = sedval(ledu,age+1,:,ialp,jail) + &
                     zstat(i1,edu+1)* edval(edu+1,age+1,:,i1,ialp,jail)        

             ENDDO
          ELSE
             DO i_we=1,ngpa
                CALL emuc_evalue(edu,age,i_we,(ngpz+1)/2,ialp,jail,emucp,evaluep)
                sedval(ledu,age+1,i_we,ialp,jail)=emucp
                seval(ledu,age+1,i_we,ialp,jail)=evaluep      
             ENDDO
          ENDIF    ! age=sum(eduyr)
       
          i_we_min = ngpa
          i_we_max = 1
          edval_jumpup = 0.0d0
          edval_jumpdown = INADERIV 

          !1. Check for non-concavity and compute bounds of non-concave region
          !-------------------------------------------------------------------

          mask = ( sedval(ledu,age+1,2:ngpa,ialp,jail) > &
               sedval(ledu,age+1,1:ngpa-1,ialp,jail) )
          IF (COUNT(mask)>0) THEN 
             !Non-concavity. Computes bounds of region.
             edval_jumpup = MAXVAL(PACK(sedval(ledu,age+1,2:ngpa,ialp,jail),mask))
             edval_jumpdown = MINVAL(PACK(sedval(ledu,age+1,1:ngpa-1,ialp,jail),mask))
             IF (COUNT(sedval(ledu,age+1,:,ialp,jail)>edval_jumpup)==0) THEN 
                i_we_min=1
             ELSE
                i_we_min = MINLOC( sedval(ledu,age+1,:,ialp,jail),1, & 
                     sedval(ledu,age+1,:,ialp,jail)>edval_jumpup )+1
             END IF
             IF (COUNT(sedval(ledu,age+1,:,ialp,jail)<edval_jumpdown)==0) THEN 
                i_we_max=ngpa
             ELSE
                i_we_max = MAXLOC(sedval(ledu,age+1,:,ialp,jail),1, &
                     sedval(ledu,age+1,:,ialp,jail)<edval_jumpdown) - 1
             END IF
          END IF

          !2. Solve for cons and value fns on the endogenous grid points
          !------------------------------------------------------------------
          ! Initializing policy and value fns for the egm algorithm
          c_star = -1.0d11
          z_star = -1.0d11
          v_star = -1.0d11
          i_we_sol = -11
          ! NOTE: labor income is zero for students. Transfers and Tuition costs are considered as part
          ! of start-of-period wealth.
          wg = 0.d0

          ! Initializing parameters for the egm algorithm
          index = 0
          j_0 = 1 
          j_bar = 0
          z_bar = 10.d6
          v_bar = edval_jumpup + 1.d-5

          !2.1 Solves for the cons fun on the end. grid
          !------------------------------------------------
          DO i_we=1,ngpa

             IF (i_we<i_we_min .OR. i_we>i_we_max) THEN  !if_1
                ! Concave region - standard egm 
                !----------------------------------
                index = index + 1
                CALL localegm(i_we,bita*surprob(age)*sedval(ledu,age+1,i_we,ialp,jail),&
                     &c_star(index),z_star(index))
                i_we_sol(index) = i_we
             ELSE
                !Non-concave region
                !---------------------------------
                IF (index>0) THEN 
                   !If the policy function has been solved for at least one point
                   !update lower bounds used to exploit monotonicity
                   c_bar =MAX(z_star(index)-agrid(i_we,age+1),1.d-5)
                   CALL MUOC(age,c_bar,v_bar)
                   j_0 = i_we_sol(index) 
                END IF
                CALL MUOC(age,z_bar-agrid(i_we,age+1),bound)

                !Exploit monotonicity to select candidate points 
                IF (  ( v_bar>sedval(ledu,age+1,i_we,ialp,jail) .AND.  &    
                     sedval(ledu,age+1,i_we,ialp,jail).GE.bound ) .OR. &
                     (sedval(ledu,age+1,i_we,ialp,jail)<bound .AND. i_we.GE.j_bar)  ) THEN !if_2
                   ! A. Solve for local max
                   !-----------------------
                   CALL localegm(i_we,bita*surprob(age)*sedval(ledu,age+1,i_we,ialp,jail),&
                        &c_guess,z_guess)
                   !B. Verify whether local max is global given zguess
                   !----------------------------------
                   ALLOCATE(c_temp(j_0:i_we_max))
                   DO j = j_0,i_we_max
                      c_temp(j) = z_guess - agrid(j,age+1)
                      IF (c_temp(j).LE.0 .AND. j>1) EXIT
                   END DO

                   ALLOCATE (val_temp(j_0:j-1))
                   CALL PUTILITY_ARR(age,c_temp(j_0:j-1),st_ind,jail,val_temp)
                   val_temp =  val_temp+ bita*surprob(age)*seval(ledu,age+1,j_0:j-1,ialp,jail)
                   max_loc = MAXLOC(val_temp,1)+j_0-1
                   DEALLOCATE (c_temp,val_temp)

                   IF (max_loc==i_we) THEN            !if_3
                      index = index+1
                      c_star(index) = c_guess
                      z_star(index) = z_guess
                      i_we_sol(index) = i_we
                      z_bar = 10.d6
                   ELSEIF (max_loc>i_we) THEN 
                      ! Store location of (approximate) global max given z_temp
                      ! By monotonicity no need to consider points to its left.
                      j_bar=max_loc
                      z_bar = z_guess
                   END IF   !if_3
                END IF      !if_2
             END IF         !if_1
          END DO  ! i_we

          !2.2 Solves for the value fun on the end. grid
          !-----------------------------------------------
          CALL PUTILITY_ARR(age,c_star(1:index),st_ind,jail,v_star(1:index))
          v_star(1:index) = v_star(1:index) + bita*surprob(age)*&
               seval(ledu,age+1,i_we_sol(1:index),ialp,jail)

          !2.3 Correct the policy and value fns on the first
          !    end. cash-at-hand node if the choice of future assets
          !    is to the right of the borrowing constraint
          !------------------------------------------------

          IF (i_we_sol(1)>1) THEN
             c_star(2:ngpa) = c_star(1:ngpa-1)
             z_star(2:ngpa) = z_star(1:ngpa-1)
             i_we_sol(2:ngpa) = i_we_sol(1:ngpa-1)
             v_star(2:ngpa) = v_star(1:ngpa-1)

             ! Solve for the highest value of cash at hand for which the
             ! borrowing constraint is binding 
             i_we_glob = i_we_sol(1)
             v_glob = bita*surprob(age)* &
                  (seval(ledu,age+1,i_we_sol(1),ialp,jail)-seval(ledu,age+1,1,ialp,jail))
             x_guess = c_star(1) 
             call d_zreal(min_z_star,x_temp,errabs=errabs,errrel=errrel,eps=eps,&
                  xguess=x_guess)
             c_star(1) = x_temp(1) - 1.d-5
             z_star(1) = c_star(1) + agrid(1,age+1)
             i_we_sol(1) = 1
             CALL PUTILITY(age,c_star(1),st_ind,jail,v_star(1))
             v_star(1) = v_star(1) + bita*surprob(age)* &
                  seval(ledu,age+1,1,ialp,jail)
             index = index + 1 
          END IF


          IF ( ANY( c_star(:)/=c_star(:) ).OR. ANY( ABS(c_star(:))>HUGE(c_star(:)) ) ) THEN
             PRINT *,"c_star IS NAN at"
             PRINT *,"edu=",edu,"age=",age,"i_we",i_we,&
                  "ialp=",ialp,"st_ind=",st_ind,"jail=",jail
             STOP
          ENDIF
        
          ! 3. Recover policy and value fn on the EXOGENOUS grid points 
          !----------------------------------------------------------
	
          !Recover initial wealth from cash at hand at end of period
          w_star(:) = z_star(:)/(1.d0+ir(age))

          !Interpolate
          call int_node(w_star(1:index),agrid(:,age),node)
          node = node - 1
          CALL  PUTILITY (age,c_star(1),st_ind,jail,fel1)	
          where (node==0)
            ! Borrowing constrained individual
            con(edu,age,:,i_zet,ialp,st_ind,jail) = &
                wg + agrid(:,age)*(1.d0+ir(age))-agrid(1,age+1)
            val(edu,age,:,i_zet,ialp,st_ind,jail)=&
                utils(age,con(edu,age,:,i_zet,ialp,st_ind,jail),st_ind,jail) 
            val(edu,age,:,i_zet,ialp,st_ind,jail) = &
                min(val(edu,age,:,i_zet,ialp,st_ind,jail),fel1)-fel1 +&
                v_star(1)
          elsewhere
            con(edu,age,:,i_zet,ialp,st_ind,jail) = &
                c_star(node)+ ( c_star(node+1)-c_star(node) )/&
                ( w_star(node+1)-w_star(node) ) *&
                ( agrid(:,age)-w_star(node) )
            val(edu,age,:,i_zet,ialp,st_ind,jail) = &
                v_star(node)+ ( v_star(node+1)-v_star(node) )/&
                ( w_star(node+1)-w_star(node) ) *&
                ( agrid(:,age)-w_star(node) )
          endwhere
          con(edu,age,:,i_zet,ialp,st_ind,jail) = MAX(CLOW,con(edu,age,:,i_zet,ialp,st_ind,jail))
          sav(edu,age,:,i_zet,ialp,st_ind,jail)=agrid(:,age)*(1.d0+ir(age))- &
              & con(edu,age,:,i_zet,ialp,st_ind,jail)


          IF (ANY(ieee_is_nan(con(edu,age,:,i_zet,ialp,st_ind,jail)))) THEN
             PRINT *,"CON IS NAN at"
             PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_iand="&
                  &,st_ind,"jail=",jail
             STOP
          ENDIF

          DO i_we=1,ngpa
             CALL MUOC( age,con(edu,age,i_we,i_zet,ialp,st_ind,jail),muc)
             DVAL(edu,age,i_we,i_zet,ialp,st_ind,jail) = (1.d0+ir(age))*muc
          END DO


          IF (ANY(VAL(edu,age,:,i_zet,ialp,st_ind,jail)/=&
               VAL(edu,age,:,i_zet,ialp,st_ind,jail))) THEN
             PRINT *,"VAL IS NAN at"
             PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                  &,st_ind,"jail=",jail
             STOP
          ENDIF

          IF (ANY(dVAL(edu,age,:,i_zet,ialp,st_ind,jail)/=&
               dVAL(edu,age,:,i_zet,ialp,st_ind,jail))) THEN
             PRINT *,"VAL IS NAN at"
             PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                  &,st_ind,"jail=",jail
             STOP
          ENDIF

          IF (ANY(VAL(edu,age,1:(ngpa-1),i_zet,ialp,st_ind,jail)-&
               & VAL(edu,age,2:ngpa,i_zet,ialp,st_ind,jail)>1.d-5)) THEN
             PRINT *,"VAL IS decreasing"
             PRINT *,"edu=",edu,"age=",age,"i_zet=",i_zet,"ialp=",ialp,"st_ind="&
                  &,st_ind,"jail=",jail
             STOP
          ENDIF

          do i1 = 2,ngpz   ! Assign policies and vals to all i_zet>1 - use index i1 to avoid resetting of i_zet
             con(edu,age,:,i1,ialp,st_ind,jail)=con(edu,age,:,1,ialp,st_ind,jail)
             sav(edu,age,:,i1,ialp,st_ind,jail)=sav(edu,age,:,1,ialp,st_ind,jail)
             val(edu,age,:,i1,ialp,st_ind,jail)=val(edu,age,:,1,ialp,st_ind,jail)
             dval(edu,age,:,i1,ialp,st_ind,jail)=dval(edu,age,:,1,ialp,st_ind,jail)
          enddo

          continue

       ENDDO   ! IALP end do fixed effect
    ENDDO jaildo  ! end do jail
 ENDDO   ! AGE
ENDDO   ! EDU

continue

END SUBROUTINE decisions_st

end module decisions
