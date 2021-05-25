program main

use glob
implicit none
!include 'mpif.h'

integer		            ::	i,j,h,a,t,ap,hp,tp,m,steady,z,indx(nind)
real                    ::  stp,temp, mu(nind), s(nind), qtemp(nskl)
real                    ::  lam1(nind), gam1(nind), qshr_emp1(nind) 
real                    ::  onet1(nind), cap1(nind), kap1(nind)
real, dimension(nind)   ::  rwage_emp1, ypw_emp1
real                    ::  hcap2(nskl)
integer                 ::  optimal

! Read Endogenous Parameters
open(unit=38, file="textfiles/parameters.txt",action='read',position='rewind')
read(38,*),trs_fac
read(38,*),z_mult
read(38,*),tfp
read(38,*),rho
read(38,*),scale
read(38,*),trs
read(38,*), sub
read(38,*), gov_res
read(38,*), del
read(38,*), gov_yy
read(38,*), reg_lag
close(unit=38)

!heritable traits parameters
call rouwenhorst(rho,nthta,pi_thta,thta)



!pi_thta=1.0
!print*, sum(pi_thta,2)
open(unit=38, file="textfiles/PI_lr.txt",action='read',position='rewind')
read(38,*),pi_thta_lr
close(unit=38)
!pi_thta_lr=1.0
! Set grid of idiosyncratic shocks
open(unit=38, file="textfiles/incomes.txt",action='read',position='rewind')
read(38,*),zgrd
read(38,*),pi_z
close(unit=38)
zgrd=zgrd*z_mult
zgrd=zgrd-0.5*z_mult**2
zgrd=exp(zgrd)





! Set Grid of Skills
hcap(1:7)=(/0.01,0.1,0.2,0.3,0.4,0.5,0.6/)
hcap(nskl)=80.0
stp=(hcap(nskl)-hcap(1))/real(nskl-1-6)
do h=8,nskl
	hcap(h)=hcap(h-1)+stp
enddo
hcap2=hcap
temp=(hcap(nskl)/hcap(7))**(1.0/real(nskl-1-6))
do h=8,nskl
	hcap2(h)=hcap2(h-1)*temp
enddo
hcap=((1.0/4.0)*hcap+(3.0/4.0)*hcap2)
!hcap=hcap*100.0

ag(1)=0.0
ag(2)=1.0
do h=3,na
 ag(h)=ag(h-1)*2.5
enddo

indh=10
! Industry Parameters and data
open(unit=38, file="textfiles/gamma/USgam.txt",action='read',position='rewind')
read(38,*),gam1
close(unit=38)
open(unit=38, file="textfiles/capitalshare/UScap.txt",action='read',position='rewind')
read(38,*),cap1
close(unit=38)
open(unit=38, file="textfiles/taxes/US.txt",action='read',position='rewind')
read(38,*),tax
read(38,*),subrat
close(unit=38)
if (optim==1) then
open(unit=38, file="textfiles/opt_tax.txt",action='read',position='rewind')
read(38,*),tax
close(unit=38)
endif
open(unit=38, file="textfiles/kap.txt",action='read',position='rewind')
read(38,*),kap1
close(unit=38)
!open(unit=38, file="textfiles/lambda.txt",action='read',position='rewind')
!read(38,*),lam1
!close(unit=38)
open(unit=38, file="textfiles/ONet.txt",action='read',position='rewind')
read(38,*),onet1
close(unit=38)
open(unit=38, file="textfiles/USq.txt",action='read',position='rewind')
read(38,*),qshr_emp1
close(unit=38)
open(unit=38, file="textfiles/USrwage.txt",action='read',position='rewind')
read(38,*),rwage_emp1
close(unit=38)
open(unit=38, file="textfiles/USypw.txt",action='read',position='rewind')
read(38,*),ypw_emp1
close(unit=38)
!open(unit=38, file="textfiles/indh.txt",action='read',position='rewind')
!read(38,*),indh
!close(unit=38)
open(unit=38, file="textfiles/a.txt",action='read',position='rewind')
read(38,*),a1
read(38,*),a2
close(unit=38)

lam1 = a1 + a2*onet1
if (minval(lam1).le.0.0) then
print*, "lam too low"
pause
endif

call indexx(nind,lam1,indx)
lam=lam1(indx)
onet=onet1(indx)
gam=gam1(indx)
qshr_emp=qshr_emp1(indx)
rwage_emp=rwage_emp1(indx)
ypw_emp=ypw_emp1(indx)
cap=cap1(indx)
kap=kap1(indx)

do i=1,nind
  scalw(i) = sum(pi_z*zgrd**lam(i))
enddo


open(unit=38, file="textfiles/wage.txt",action='read',position='rewind')
read(38,*),wage
close(unit=38)
!wage=200.0*log(1+hcap)
open(unit=38, file="textfiles/wagesim.txt",action='read',position='rewind')
read(38,*),wagesim
close(unit=38)

open(unit=38, file="textfiles/y.txt",action='read',position='rewind')
read(38,*),yy
close(unit=38)
open(unit=38, file="textfiles/V.txt",action='read',position='rewind')
read(38,*),V
close(unit=38)
open(unit=38, file="textfiles/QQs_sav.txt",action='read',position='rewind')
read(38,*),QQs_sav
close(unit=38)

!
!open(unit=38, file="textfiles/polsim.txt",action='write',position='rewind')
!write(38,*), ""
!close(unit=38)
!do optimal=1,25

steady=0
do while ((maxval(abs(w-wage)/wage,1)>5.e-3 .and. steady<1000) .or. steady<100) !  
steady=steady+1
print*,steady, maxval(abs(w-wage)/wage,1)

! Set consumption decision grid
  do h=1,nskl
    do hp=1,nskl
      do tp=1,nthta
        do z=1,nz
        do a=1,na
        do ap=1,na
          
          cons(z,h,a,tp,hp,ap) = ag(a) - ag(ap) + (1.0-tax)*wage(h)*(zgrd(z)) + &
          trs - max(((hcap(hp))/(exp(thta(tp))))**(1.0/del)-sub,0.0)  !*hcap(h)
          if (cons(z,h,a,tp,hp,ap).gt.0.0) then !**lam(indh(h))
            util(z,h,a,tp,hp,ap)=(cons(z,h,a,tp,hp,ap)**(1.0-sigma))/(1.0-sigma)
          else
            util(z,h,a,tp,hp,ap)=-1.e10
          endif 
        
        enddo
        enddo   
        enddo  
      enddo
    enddo
  enddo

!print*, "grids done"

call decisions
call simulation
  
!if (calib==0) tfp = 0.9*tfp + 0.1*tfp*(yysv/yy) 
!if (calib==0) print*, yy, yysv
open(unit=38, file="textfiles/wage.txt",action='write',position='rewind')
write(38,*),wage
close(unit=38)


enddo !steady

open(unit=38, file="textfiles/polsim.txt",action='write',position='append')
write(38,'(6f12.6)'), sub, sub/yy, tax, trs, welfare, IGESS
close(unit=38)

!sub = sub+10.0
!enddo !optimal

open(unit=38, file="textfiles/print.txt",action='write',position='rewind')
do i=1,nind
	mu(i)= sum(q(i,:)*hcap(:))/sum(q(i,:))
	s(i) = (sum(q(i,:)*hcap(:)**2))**(.5)
	!print*, lam(i), gam(i), QQs(i), cap(i)

write(38,*),lam(i),sum(q(i,:))
enddo
close(unit=38)
	
!! Write Outputs
open(unit=38, file="textfiles/y.txt",action='write',position='rewind')
write(38,*),yy
close(unit=38)
open(unit=38, file="textfiles/V.txt",action='write',position='rewind')
write(38,*),V
close(unit=38)
open(unit=38, file="textfiles/indh.txt",action='write',position='rewind')
write(38,*),indh
close(unit=38)

do i=1,nind
  do j=1,nind
    if (indx(j)==i) then
      lam1(i)=lam(j)
      kap1(i)=kap(j)
      exit
    endif
  enddo
enddo
open(unit=38, file="textfiles/lambda.txt",action='write',position='rewind')
do i=1,nind
write(38,*),lam1(i)
enddo
close(unit=38)

if(calib==1) then
 open(unit=38, file="textfiles/QQs_sav.txt",action='write',position='rewind')
 write(38,*),QQs
 close(unit=38)
endif

!if(optim==1) then
 open(unit=38, file="textfiles/opt_tax.txt",action='write',position='rewind')
 write(38,*),tax
 close(unit=38)
!endif

open(unit=38, file="textfiles/kap.txt",action='write',position='rewind')
write(38,*),kap1
close(unit=38)
open(unit=38, file="textfiles/parameters.txt",action='write',position='rewind')
write(38,*),trs_fac
write(38,*),z_mult
write(38,*),tfp
write(38,*),rho
write(38,*),scale
write(38,*),trs
write(38,*),sub
write(38,*),gov_res
write(38,*),del
write(38,*),gov_yy
write(38,*),reg_lag
close(unit=38)
open(unit=38, file="textfiles/a.txt",action='write',position='rewind')
write(38,*),a1
write(38,*),a2
close(unit=38)

open(unit=38, file="textfiles/hdec.txt",action='write',position='rewind')
write(38,*),"z"
do i=1,nz
  write(38,*) hcap(hdec(i,4,10,1)), zgrd(i)
enddo 
write(38,*)," "
write(38,*),"theta"
do i=1,nthta
  write(38,*) hcap(hdec(4,i,10,1)), thta(i)
enddo  
write(38,*)," "
write(38,*),"skill"
do i=1,nskl
  write(38,*) hcap(hdec(4,4,i,1)), hcap(i)
enddo 
write(38,*)," "
write(38,*),"assets"
do i=1,na
  write(38,*) hcap(hdec(4,4,10,i)), ag(i)
enddo 
close(unit=38)

	
print*, 'done'		
pause
end program main

!***********************************************************************

subroutine	prod_shares
use glob
implicit none

integer     ::  seed, t, i, ii, h, j, jj, tt, jjj, k
real(8)     ::  u(nsim)
integer     ::  hloc1(nsim), hloc2(nsim), indx(nsim), cut(nind-1)
integer     ::  temp, temp_lag, hsorti(nsim), switch(nsim)
real        ::  tol, w1(nind,nskl), w2(nskl), hsort(nsim), dev(nsim)
real        ::  ylag(nind), prod, yylag(10), tempw(nsim), tempc(nsim), tempt

tol=1.e-4

! Initialize (alternatively start from saved y's)
call indexx(nsim,hcaps,indx)
hsort=hcaps(indx)
hsorti=hcapi(indx)
zsims=zsims(indx)

open(unit=38, file="textfiles/QQs.txt",action='read',position='rewind')
read(38,*),QQs
close(unit=38)

!QQs=aint(real(nsim)/real(nind))
!do while (sum(QQs)<real(nsim))
! QQs(1)=QQs(1)+1.0
!enddo

do j=1,nsim
  do i=1,nind
    if (real(j).le.sum(QQs(1:i))) then
      hloc1(j)=i
      exit
    endif
  enddo
enddo
where (hloc1==0)
 hloc1=nind
end where


tt=0
jj=1
jjj=0
yylag=0.0
yy=tfp  !if(calib==1) 
do while (jj>0 .and. yylag(10).ne.yy .and. jjj<1000)  !
jj=0
do ii=1,nind
do t=1,1

Qs=0.0
do j=1,nsim
    Qs(hloc1(j),hsorti(j))=Qs(hloc1(j),hsorti(j))+1.0
enddo
Qs=Qs/real(nsim)

scalT=1.0
!*****************************************
!! Enable if controlling for scale effects
!if (calib==1) then
!  scalT=1.0
!else
!  scalT = (QQs/QQs_sav)**(1.0-1.0/lam)
!endif
!*****************************************

yy=tfp ! if(calib==1) 
do i=1,nind
    y(i) = scalT(i)*(sum(Qs(i,:)*(hcap**lam(i))) )**(1.0/lam(i)) 
    yy   = yy*((kap(i)**cap(i))*(y(i)**(1.0-cap(i))))**gam(i)  
enddo

do i=1,nind
  kap(i) = 0.98*kap(i) + 0.02*(gam(i)*cap(i)*yy)/kret
enddo



do i=1,nind
  do h=1,nskl
    w1(i,h) = (1.0-cap(i))*yy*(gam(i)/lam(i))*( (hcap(h)/y(i))**lam(i) ) !
  enddo
enddo

w2 = maxval(w1,1)

  do h=1,nskl
    if ( abs(Qs(ii,h)*(w1(ii,h)-w2(h))/w2(h)) .gt. tol) then
!      print*, w1(ii,h), w2(h), ii, maxloc(w1(:,h),1)
      exit
    endif
  enddo
  
  if (h==nskl+1) then
!    print*, ii, ii+1
    exit
  endif
    

if (maxloc(w1(:,h),1).ne.ii .and. QQs(ii)>1.0) then
  jj=jj+1
  QQs(ii)=QQs(ii)-1.0
  QQs(maxloc(w1(:,h),1))=QQs(maxloc(w1(:,h),1))+1.0
  do j=1,nsim
    do i=1,nind
      if (real(j).le.sum(QQs(1:i))) then
        hloc1(j)=i
        exit
      endif
    enddo
  enddo
  where (hloc1==0)
    hloc1=nind 
  end where
endif

  
enddo  
enddo
yylag(2:10)=yylag(1:9)
yylag(1)=yy
jjj=jjj+1
!print*,jjj,jj,yy
enddo


do i=1,nind
  qshr(i)=QQs(i)/real(nsim)
!  print*,sum(Qs(i,:))
enddo

!do h=1,nskl
!  print*, w2(h), wage(h), maxloc(w1(:,h),1), hcap(h), qq(h)
!enddo

indh=maxloc(w1,1)
w=w2

wage=.05*w+.95*wage
if (calib==1 ) tfp = 0.98*tfp + .02*tfp*(1200.0/w(4)) !  if (optim.ne.1)   
wagesim = 0.25*log(wage(hsorti)*(zsims)) + 0.75*wagesim  !    log(wage(hsorti)*(zsims)) ! 

do i=1,nind
tempc=0.0
tempw=0.0
where (hloc1==i)
tempc=1.0
tempw=wagesim
endwhere
rwage(i) = sum(tempw)/sum(real(tempc))
varwage(i) = sqrt(sum(tempw**2)/sum(real(tempc)) - &
  (sum(tempw)/sum(real(tempc)))**2)
cvwage(i) = sqrt(varwage(i))/rwage(i)
ypw(i) = gam(i)*real(nsim)/sum(real(tempc))
enddo
tempt=sum(varwage)/real(nind)
varwage=varwage/tempt
tempw=wage(hsorti)
tempt =sum(tempw)/real(nsim)
rwage = rwage/tempt

open(unit=38, file="textfiles/QQs.txt",action='write',position='rewind')
write(38,*),QQs
close(unit=38)

open(unit=38, file="textfiles/wagesim.txt",action='write',position='rewind')
write(38,*),wagesim
close(unit=38)

do j=1,nsim
  industry(indx(j)) = hloc1(j)
enddo

end subroutine prod_shares

!*****************************************************************************************************************




subroutine decisions
use glob
implicit none

real                ::  Vtemp(nz,nthta,nskl,na), cont(nskl,na)
integer             ::  hdtemp(nz,nthta,nskl,na)

integer             :: a, h, t, z, zp, aa, hh, tp, count, dec(2)

Vtemp=V
do count=1,50

  do h=1,nskl
    do t=1,nthta
      do z=1,nz
      do a=1,na
    
        cont = 0.0
        do tp=1,nthta
          do zp=1,nz
            cont=cont+pi_thta(t,tp)*pi_z(zp)*Vtemp(zp,tp,:,:)
          enddo  
        enddo
        dec = maxloc(util(z,h,a,t,:,:)+beta*cont)
        hdtemp(z,t,h,a)=dec(1)     
        adec(z,t,h,a)=dec(2)   
        V(z,t,h,a) = (util(z,h,a,t,dec(1),dec(2))+beta*cont(dec(1),dec(2)))
     
      enddo
      enddo
    enddo
  enddo

  if (maxval(abs(Vtemp-V))<0.0001) then
    exit
  else
    Vtemp = V
  endif
  
  print*, Vtemp(2,3,2,1)

enddo

!  do h=1,nskl
!    do t=1,nthta
!    
!      hdec(t,h) = max(hdec(t,h)-1,min(hdec(t,h)+1,hdtemp(t,h)))
!    
!    enddo
!  enddo

hdec=hdtemp

end subroutine decisions


!*****************************************************************************************************************

subroutine simulation
use glob
implicit none

integer     ::  seed, t, i, h, j
real(8)     ::  u(nsim),uz(nsim),u_b(nsim),uz_b(nsim)
integer     ::  hsim_l(nsim), hsim(nsim),asim_l(nsim), asim(nsim)
integer     ::  tsim(nsim), zsim(nsim), zsim_l(nsim), tsim_l(nsim)
integer     ::  tsim_ll(nsim),indx(nsim)
integer     ::  qc(nskl), cldi(nsim), pari(nsim), tsim_b(nsim)
integer     ::  hsim_b(nsim), zsim_b(nsim)
real        ::  qr(nskl), mT, mB,subspd, adist(na)
real        ::  ls1(nsim), ls2(nsim), ls3(nsim), ls4(nsim), ls5(nsim)
real        ::  b(2), wpt(nsim), msim(nsim), msim_l(nsim), msim_b(nsim)
real        ::  c1, c2, hhat(nsim), one(nsim), sse(nsim), hpsort(nsim)
real        ::  sst(nsim), temp, quint(5,5), cld(nsim), par(nsim)
real        ::  clds(nsim), pars(nsim), utils(nsim)
seed=1234
call zufalli(seed)

call zufall(nsim,u)
do i=1,nsim
 do j=1,nthta
   if (u(i).le.sum(pi_thta_lr(1:j))) then
     tsim(i)=j
     exit
   endif
 enddo
enddo
where (tsim==0)
tsim=nthta
end where

hsim=3
zsim=1
asim=1

do t=1,750

call zufall(nsim,u)
call zufall(nsim,u_b)
call zufall(nsim,uz)
call zufall(nsim,uz_b)
hsim_l = hsim
zsim_l = zsim
tsim_ll= tsim_l
tsim_l = tsim
asim_l = asim
msim_l = msim

    do i=1,nsim
    
        do j=1,nthta
          if (u(i).le.sum(pi_thta(tsim_l(i),1:j))) then
            tsim(i)=j
            exit
          endif
        enddo
        
        do j=1,nz
          if (uz(i).le.sum(pi_z(1:j))) then
            zsim(i)=j
            exit
          endif
        enddo
        
        utils(i) = V(zsim_l(i),tsim(i),hsim_l(i),asim_l(i))
        hsim(i) = hdec(zsim_l(i),tsim(i),hsim_l(i),asim_l(i))
        asim(i) = adec(zsim_l(i),tsim(i),hsim_l(i),asim_l(i))        
        msim(i) = max(((hcap(hsim(i)))/&
                  (exp(thta(tsim(i)))))**(1.0/del)-sub,0.0)        

        if (t==750) then

        do j=1,nthta
          if (u_b(i).le.sum(pi_thta(tsim_l(i),1:j))) then
            tsim_b(i)=j
            exit
          endif
        enddo
        
        do j=1,nz
          if (uz_b(i).le.sum(pi_z(1:j))) then
            zsim_b(i)=j
            exit
          endif
        enddo        
        
        hsim_b(i) = hdec(zsim_l(i),tsim_b(i),hsim_l(i),asim_l(i))
        msim_b(i) = max(((hcap(hsim_b(i)))/&
                    (exp(thta(tsim_b(i)))))**(1.0/del)-sub,0.0) 
                
        endif                
                   
    enddo

enddo

hcapi=hsim
hcaps=hcap(hsim)
zsims = zgrd(zsim)

!print*, "h:",sum(hcap(hsim))/real(nsim)
!print*, "a:",sum(ag(asim))/real(nsim)

adist=0.0
do i=1,nsim
adist(asim(i))=adist(asim(i))+1.0
enddo
adist=adist/real(nsim)
!print*, adist 

qc=0
do i=1,nsim
  qc(hsim(i))=qc(hsim(i))+1
enddo
qr=real(qc)/real(nsim)
qq=qr

call prod_shares

call indexx(nsim,hcap(hsim_l),indx)
do j=1,nsim
  do i=1,nind
    if (real(j).le.sum(QQs(1:i))) then
      industryp(indx(j))=i
      exit
    endif
  enddo
enddo



! IGE earnings
ls1=0.0;ls2=0.0;ls3=0.0;ls4=0.0;ls5=0.0;
do i=1,nsim
  ls1(i)=ls1(i)+log(wage(hsim(i))*(zgrd(zsim(i)))+trs)*&
          log(wage(hsim_l(i))*(zgrd(zsim_l(i)))+trs)
  ls2(i)=ls2(i)+log(wage(hsim(i))*(zgrd(zsim(i)))+trs)
  ls3(i)=ls3(i)+log(wage(hsim_l(i))*(zgrd(zsim_l(i)))+trs)
  ls4(i)=ls4(i)+log(wage(hsim_l(i))*(zgrd(zsim_l(i)))+trs)**2
enddo
temp=(sum(ls1)-(1.0/real(nsim))*sum(ls2)*sum(ls3))/(sum(ls4)-&
  (1.0/real(nsim))*(sum(ls3))**2)
!print*, "IGE Earn"
!print*,"IGE:",temp
IGESS=temp*.47/.36
if (calib==1) rho = 0.99*rho + 0.01*rho*(0.36/temp)
if (calib==1) rho = max(min(rho,.99),.01)

! Scale correlation into elasticity
print*,"IGE:",temp*.47/.36

b(2) = temp
b(1) = (1.0/real(nsim))*sum(ls2) - b(2)*(1.0/real(nsim))*sum(ls3)
one=1.0
hhat = b(1)*one + b(2)*ls3
c1=(1.0/real(nsim))*sum(hhat)
c2=(1.0/real(nsim))*sum(ls2)
do i=1,nsim
  sse(i) = (hhat(i)-c1)**2
  sst(i) = (ls2(i)-c2)**2
enddo  
temp=sum(sse)/sum(sst) 
!print*, "R^2:",temp  


! IGE education
ls1=0.0;ls2=0.0;ls3=0.0;ls4=0.0;ls5=0.0;
!do i=1,nsim
!  ls1(i)=ls1(i)+log(hcap(hsim(i)))*log(hcap(hsim_l(i)))
!  ls2(i)=ls2(i)+log(hcap(hsim(i)))
!  ls3(i)=ls3(i)+log(hcap(hsim_l(i)))
!  ls4(i)=ls4(i)+log(hcap(hsim_l(i)))**2
!enddo
do i=1,nsim
  ls1(i)=ls1(i)+log((msim(i)+sub))*log((msim_l(i)+sub))
  ls2(i)=ls2(i)+log((msim(i)+sub))
  ls3(i)=ls3(i)+log((msim_l(i)+sub))
  ls4(i)=ls4(i)+log((msim_l(i)+sub))**2
enddo
temp=(sum(ls1)-(1.0/real(nsim))*sum(ls2)*sum(ls3))/&
  (sum(ls4)-(1.0/real(nsim))*(sum(ls3))**2)
!print*, "IGE Edu"
!print*,"IGE:",temp


! Brother income
ls1=0.0;ls2=0.0;ls3=0.0;ls4=0.0;ls5=0.0;
do i=1,nsim
  ls1(i)=ls1(i)+log(wage(hsim(i))*(zgrd(zsim(i)))+trs)*&
    log(wage(hsim_b(i))*(zgrd(zsim_b(i)))+trs)
  ls2(i)=ls2(i)+log(wage(hsim(i))*(zgrd(zsim(i)))+trs)
  ls3(i)=ls3(i)+log(wage(hsim_b(i))*(zgrd(zsim_b(i)))+trs)
  ls4(i)=ls4(i)+log(wage(hsim_b(i))*(zgrd(zsim_b(i)))+trs)**2
enddo
temp=(sum(ls1)-(1.0/real(nsim))*sum(ls2)*sum(ls3))/(sum(ls4)-&
  (1.0/real(nsim))*(sum(ls3))**2)
!print*, "IGE Brother"
!print*,"IGE:",temp

! Brother income
ls1=0.0;ls2=0.0;ls3=0.0;ls4=0.0;ls5=0.0;
do i=1,nsim
  ls1(i)=ls1(i)+log((msim(i)+sub))*log((msim_b(i)+sub))
  ls2(i)=ls2(i)+log((msim(i)+sub))
  ls3(i)=ls3(i)+log((msim_b(i)+sub))
  ls4(i)=ls4(i)+log((msim_b(i)+sub))**2
enddo
temp=(sum(ls1)-(1.0/real(nsim))*sum(ls2)*sum(ls3))/(sum(ls4)-&
  (1.0/real(nsim))*(sum(ls3))**2)
!print*, "IGE Brother Edu"
!print*,"IGE:",temp

taxrev=0.0
subspd=0.0
do i=1,nsim
  wpt(i) = (1.0-tax)*wage(hsim(i))*(zgrd(zsim(i)))+trs
  taxrev = taxrev + tax*wage(hsim(i))*(zgrd(zsim(i)))
  subspd = subspd + sub + trs
enddo
temp=(sum(log(wpt)**2)-(1.0/real(nsim))*(sum(log(wpt)))**2)/&
  (sum(log(wage(hsim)*(zgrd(zsim)))**2)-(1.0/real(nsim))*&
  (sum(log(wage(hsim)*(zgrd(zsim)))))**2)
print*, "ies:",temp
!trs = trs_fac*taxrev/real(nsim) !if (calib==1) 
!if (calib==1) trs_fac = 0.95*trs_fac + 0.05*trs_fac*(temp/0.61)**2 !if (calib==1) 

  trs = 0.9*trs + 0.1*trs*(temp/.61)  !    if (calib==1) 
  trs_fac = trs/(taxrev/real(nsim))

!if (calib==0) trs = trs_fac*(taxrev/real(nsim))
temp=(((1.0/real(nsim))*sum(log(zgrd(zsim))**2) - &
  (1.0/real(nsim)**2)*(sum(log(zgrd(zsim)))**2))) / &
  (((1.0/real(nsim))*sum((log(wage(hsim))+log(zgrd(zsim)))**2) - &
  (1.0/real(nsim)**2)*(sum((log(wage(hsim))+log(zgrd(zsim))))**2)))
print*, "z-y:",temp

if (calib==1) zgrd=(/-2.4495  , -1.6330  , -0.8165   ,   0.0 ,    0.8165  ,  1.6330  ,  2.4495/)
if (calib==1) z_mult = z_mult*0.95 + 0.05*z_mult*(0.4/temp)
if (calib==1) zgrd=zgrd*z_mult
if (calib==1) zgrd=zgrd-0.5*z_mult**2
if (calib==1) zgrd=exp(zgrd)
!print*, sum(((qshr-qshr_emp)/qshr_emp)**2)
!print*, 1000.0*sum((qshr-qshr_emp)**2)
!print*, sum(((ypw-ypw_emp)/ypw_emp)**2)
!print*, sum((ypw-ypw_emp)**2)/10.0
!temp=sum(rwage**2)/real(nind) - (sum(rwage)/real(nind))**2
!print*, temp



wpt = wage(hsim)*(zgrd(zsim))
call indexx(nsim,wpt,indx)
temp = sum(wpt(indx(9000:9100)))/sum(wpt(indx(1000:1100)))
!print*, "90-10:", temp
!Earnings Variation
temp = sqrt(sum((log(wage(hsim)*(zgrd(zsim)))-sum(log(wage(hsim)*&
  (zgrd(zsim))))/real(nsim))**2)/real(nsim)) 
print*, 'std(log(earn))',  temp

if (calib==1) then
lam = lam - a1
a1 = 0.99*a1 + 0.01*a1*(0.42/temp)
lam = lam + a1
endif


! IGE earnings
ls1=0.0;ls2=0.0;ls3=0.0;ls4=0.0;ls5=0.0;
do i=1,nind
  ls1(i)=ls1(i)+varwage(i)*onet(i)
  ls2(i)=ls2(i)+varwage(i)
  ls3(i)=ls3(i)+onet(i)
  ls4(i)=ls4(i)+onet(i)**2
enddo
temp=(sum(ls1(1:nind))-(1.0/real(nind))*sum(ls2(1:nind))*&
  sum(ls3(1:nind)))/(sum(ls4(1:nind))-(1.0/real(nind))*&
  (sum(ls3(1:nind)))**2)
if (isnan(temp)==0) then
reg_lag=0.8*reg_lag + 0.2*temp
end if
!print*, "Var-ONET"
print*,"reg:",reg_lag


cld = wage(hsim)*(zgrd(zsim))
par = wage(hsim_l)*(zgrd(zsim_l))


call indexx(nsim,cld,indx)
clds=cld(indx)
call indexx(nsim,par,indx)
pars=par(indx)

do i=1,nsim
  if (cld(i)<=clds(nsim/5)) then
    cldi(i)=1
  elseif (cld(i)<=clds(2*nsim/5)) then
    cldi(i)=2
  elseif (cld(i)<=clds(3*nsim/5)) then
    cldi(i)=3
  elseif (cld(i)<=clds(4*nsim/5)) then
    cldi(i)=4    
  else
    cldi(i)=5
  endif
enddo

do i=1,nsim
  if (par(i)<=pars(nsim/5)) then
    pari(i)=1
  elseif (par(i)<=pars(2*nsim/5)) then
    pari(i)=2
  elseif (par(i)<=pars(3*nsim/5)) then
    pari(i)=3
  elseif (par(i)<=pars(4*nsim/5)) then
    pari(i)=4    
  else
    pari(i)=5
  endif
enddo

mT=0.0
quint=0.0
do i=1,nsim
  if(pari(i)==cldi(i))then
    mT = mT+1.0
  endif
  quint(pari(i),cldi(i))=quint(pari(i),cldi(i))+1.0
enddo

quint=quint/real(nsim/5)

mT=mT/real(nsim/5)
mT = (5.0-mT)/4.0
print*,"mT:  ", mT
if (calib==1) scale = 0.999*scale + 0.001*scale*(mT/0.9)
if (calib==1) call rouwenhorst(rho,nthta,pi_thta,thta)

mB=0.0
do i=1,5
  do j=1,5
    mB = mB + 0.2*quint(i,j)*abs(real(i)-real(j))
  enddo
enddo
!print*,"mB:  ", mB
!if (calib==1) scale = 0.999*scale + 0.001*scale*(mB/1.198)
!if (calib==1) call rouwenhorst(rho,nthta,pi_thta,thta)

if (optim.ne.1) then
temp=sub/yy
sub = 0.95*sub + 0.05*sub*(subrat/temp) ! if (calib==1) 
!print*, "sub-yy", temp
endif

taxrev=0.0
do i=1,nind 
  taxrev = taxrev + gam(i)*(1-cap(i))*yy*tax !+ gam(i)*cap(i)*yy*0.4
enddo
if (optim.ne.1) then
  gov_res = taxrev-sub-trs
  gov_yy = gov_res/yy
else
  tax = 0.9*tax + 0.1*tax*(gov_res+500.0)/((taxrev-sub-trs)+500.0)
endif
!print*, 'tax', tax, 'sub', sub/yy, yy
!print*, 'bal', taxrev-sub-trs, gov_res
!print*, 'welfare: ', sum(utils)/real(nsim)
welfare = sum(utils)/real(nsim)
temp=(sum(msim)/real(nsim))/yy

print*, 'pvt ed spd', temp
if (calib==1)  del = 0.99*del + 0.01*del*0.023/temp
  
end subroutine simulation


!*********************************************************************

SUBROUTINE indexx(n,arr,indx)

IMPLICIT NONE

INTEGER :: n,indx(n)
REAL :: arr(n)
Integer, PARAMETER  :: M=7
Integer, Parameter	::	NSTACK=50
!Indexes an array arr(1:n), i.e., outputs the array indx(1:n) such that arr(indx(j))
!is in ascending order for j = 1, 2, . . . ,N. The input quantities n and arr are not changed.

INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
REAL a
do j=1,n
indx(j)=j
enddo 
jstack=0
l=1
ir=n
1 if(ir-l.lt.M)then
do j=l+1,ir
indxt=indx(j)
a=arr(indxt)
do i=j-1,l,-1
if(arr(indx(i)).le.a)goto 2
indx(i+1)=indx(i)
enddo
i=l-1
2 indx(i+1)=indxt
enddo 
if(jstack.eq.0)return
ir=istack(jstack)
l=istack(jstack-1)
jstack=jstack-2
else
k=(l+ir)/2
itemp=indx(k)
indx(k)=indx(l+1)
indx(l+1)=itemp
if(arr(indx(l)).gt.arr(indx(ir)))then
itemp=indx(l)
indx(l)=indx(ir)
indx(ir)=itemp
endif
if(arr(indx(l+1)).gt.arr(indx(ir)))then
itemp=indx(l+1)
indx(l+1)=indx(ir)
indx(ir)=itemp
endif
if(arr(indx(l)).gt.arr(indx(l+1)))then
itemp=indx(l)
indx(l)=indx(l+1)
indx(l+1)=itemp
endif
i=l+1
j=ir
indxt=indx(l+1)
a=arr(indxt)
3 continue
i=i+1
if(arr(indx(i)).lt.a)goto 3
4 continue
j=j-1
if(arr(indx(j)).gt.a)goto 4
if(j.lt.i)goto 5
itemp=indx(i)
indx(i)=indx(j)
indx(j)=itemp
goto 3
5 indx(l+1)=indx(j)
indx(j)=indxt
jstack=jstack+2
if(jstack.gt.NSTACK)print*,"NSTACK too small in indexx"
if(ir-i+1.ge.j-l)then
istack(jstack)=ir

istack(jstack-1)=i
ir=j-1
else
istack(jstack)=j-1
istack(jstack-1)=l
l=i
endif
endif
goto 1
END



subroutine rouwenhorst(pers,num,Pmat,grid)
use glob
implicit none

real			:: pers, p
integer		:: num, i

real			:: Pmat(num,num), Pmat_tmp(num,num),grid(num),nu,space

p = (1.0+pers)/2.0

Pmat(1,1) = p
Pmat(1,2) = 1.0-p
Pmat(2,1) = 1.0-p
Pmat(2,2) = p


do i=2,num-1
  
  Pmat_tmp = Pmat
  Pmat = 0.0
  Pmat(1:i,1:i) = Pmat_tmp(1:i,1:i)*p 
  Pmat(1:i,2:i+1) = Pmat(1:i,2:i+1) + Pmat_tmp(1:i,1:i)*(1.0-p)
  Pmat(2:i+1,1:i) = Pmat(2:i+1,1:i) + Pmat_tmp(1:i,1:i)*(1.0-p)
  Pmat(2:i+1,2:i+1) = Pmat(2:i+1,2:i+1) + Pmat_tmp(1:i,1:i)*p

  Pmat(2:i,:) = Pmat(2:i,:)/2.0

enddo

nu = sqrt((real(num)-1.0)/(1.0-pers**2))*scale
!nu = sqrt((real(num)-1.0)/(1.0-0.9101**2))*scale


space = 2.0*nu/(real(num)-1.0)
grid(1) = -nu
do i=2,num
  grid(i)=grid(i-1)+space
enddo


end subroutine rouwenhorst