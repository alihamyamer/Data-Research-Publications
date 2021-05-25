subroutine ztransit(lngpz,lp,lq,lztrans)
use global1

! Starts from the 2x2 Rouwenhorst matrix to generate the matrix for 
! the matrix for a grid of size n
! lp=>persistence of node 1; lq=>persistence of node lngpz

integer,intent(in)::lngpz
real(long), intent(in) :: lp,lq
real(long),intent(out):: lztrans(lngpz,lngpz)
integer :: i, loc1, loc2, nit
real(long),dimension(2,2) :: basemat
real(long), allocatable :: trans(:,:),add1(:,:),add2(:,:),add3(:,:),add4(:,:)

! Initialize basemat
basemat(1,1)=lp         ; basemat(1,2)=1.d0-lp
basemat(2,1)=1.d0-lq    ; basemat(2,2)=lq

! Allocate the utility matrix which stores all steps towards lztrans
allocate(trans(2,2))
trans=basemat

! Compute number of iterations necessary to obtain ztrans
nit=lngpz-2

! ITERATIONS

do i=2,2+nit-1
! Allocate
allocate(add1(i+1,i+1),add2(i+1,i+1),add3(i+1,i+1),add4(i+1,i+1))

! Compute the addenda
add1=0.d0; add2=0.d0; add3=0.d0; add4=0.d0

do loc1=1,i
    do loc2=1,i
add1(loc1,loc2)=trans(loc1,loc2)
add2(loc1,1+loc2)=trans(loc1,loc2)
add3(1+loc1,loc2)=trans(loc1,loc2)
add4(1+loc1,1+loc2)=trans(loc1,loc2)
    enddo
enddo

! Compute new trans
deallocate(trans)
allocate(trans(i+1,i+1))

trans = lp*add1 + (1.d0-lp)*add2 + (1.d0-lq)*add3 + lq*add4
! Divide all row, except the top and bottom ones, by two, so that they sum up to 1
trans(2:i,:) = trans(2:i,:)/2.d0

deallocate(add1,add2,add3,add4)
enddo   ! i=2,2+nit-1

lztrans=trans
continue


endsubroutine ztransit
