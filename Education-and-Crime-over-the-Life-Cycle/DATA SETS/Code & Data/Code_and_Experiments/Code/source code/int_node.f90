module int_nodes

use global1
implicit none

contains

SUBROUTINE int_node(x,xp,nodes)
  ! Returns index of right interpolating node (from grid x) for array xp
  USE global1
  IMPLICIT NONE

  REAL (long), DIMENSION (:), INTENT(in):: x
  REAL (long), DIMENSION (:), INTENT(in):: xp
  INTEGER, DIMENSION (SIZE(xp)), INTENT(out):: nodes

  INTEGER, DIMENSION(1) :: z, i_min, i_max
  INTEGER :: j, length_x, length_xp, step, x_min, x_max
  INTEGER, DIMENSION (size(xp)) :: x_index
 
  x_min = 1
  nodes = 1
  i_max = 1
  length_x = SIZE(x)
  length_xp = SIZE(xp)

  ! 1. Right bracketing nodes for extrapolation regions
  !-----------------------------------------------------
  IF (xp(length_xp).LE.x(1)) THEN
    ! Only extrapolation to the left: nodes(:) = 1, 2. is not executed
    i_min = length_xp + 1
  ELSEIF (xp(1).GE.x(length_x-1)) THEN
    ! Only extrapolation to the right: nodes(:) = length_x 
    ! 2. is not executed
    nodes = length_x 
    i_min = length_xp + 1
  ELSE
    ! Index of first xp for which interpolation applies
    i_min = COUNT(xp.le.x(1),1) + 1 
    ! Index of last xp for which interpolation applies
    i_max = COUNT(xp<x(length_x-1),1)
    IF (i_max(1)<length_xp) nodes(i_max(1)+1:) = length_x 
  END IF


  ! 2. Right bracketing nodes for remaining points
  !--------------------------------------------------------
  DO j = i_min(1),i_max(1)

    ! Increment x_max until x(x_max) brackets xp(j)
    DO step = 10,length_x+10,10
       x_max = MIN(x_min+step, length_x-1)
	  IF (x(x_max)>xp(j)) exit
    END DO

    ! Locate position of  closest node to xp(j)
    z = MINLOC(ABS(x(x_min:x_max)-xp(j))) + x_min - 1
    x_min = z(1)
    IF (xp(j)>x(x_min)) x_min = x_min + 1
    nodes(j) = x_min

  ENDDO

END SUBROUTINE int_node


end module int_nodes