FUNCTION betinv_fn(X)
! Called by a non-linear solver to the inverse of the beta distribution
! with parameters alpha, beta
USE global1
USE global2
USE BETDF_INT
INCLUDE 'link_fnl_shared.h' 

IMPLICIT NONE
REAL(long), INTENT(in):: X
REAL(long) :: betinv_fn

betinv_fn = LOG(bet_prob) - LOG(D_BETDF(X,s_alpha,s_beta))

END FUNCTION betinv_fn



