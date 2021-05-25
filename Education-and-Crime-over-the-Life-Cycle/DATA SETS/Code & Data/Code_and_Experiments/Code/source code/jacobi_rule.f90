SUBROUTINE jacobi_rule(order,B_ALPHA,B_BETA,A,B,x)

!*****************************************************************************80
!
!! MAIN is the main program for JACOBI_RULE.
!
!  Discussion:
!
!    This program computes a standard Gauss-Jacobi quadrature rule
!    and writes it to a file.
!
!    The user specifies:
!    * the ORDER (number of points) in the rule;
!    * B_ALPHA, the exponent of (x-A) in the Beta distribution;
!    * B_BETA, the exponent of (B-X) in the Beta distribution;
!    * A, the left endpoint;
!    * B, the right endpoint;
!    
!    The Beta distribution has the form:
!               
!                    K (x-A)^(B_ALPHA-1) * (B-X)^(B_BETA-1),  B_ALPHA,B_BETA>0
!
!    The Gauss-Jacobi weighting function has the form:
!                       
!                    (x-A)^beta * (B-x)^alpha,        alpha,beta>-1      
!   
!    It follows that 
!                    beta=B_ALPHA-1,   alpha=B_BETA-1
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 November 2010
!
!  Author:
!
!    Author:
!
!    John Burkardt (modified Giulio Fella)
!

  IMPLICIT NONE
  
  REAL      ( kind = 8 ), intent(in)    		        :: a,b,b_alpha,b_beta
  INTEGER   ( kind = 4 ), intent(in)    		        :: order
  REAL      ( kind = 8 )	                            :: alpha,beta 
  REAL      ( kind = 8 ), intent(out), dimension(order) :: x
  REAL      ( kind = 8 ), DIMENSION (order) 		    :: w
  INTEGER   ( kind = 4 )  kind

  kind = 4
  alpha = b_beta - 1.d0
  beta = b_alpha - 1.d0

  CALL cgqf ( order, kind, alpha, beta, a, b, x, w )

END SUBROUTINE jacobi_rule

SUBROUTINE cdgqf ( nt, kind, alpha, beta, t, wts )

!*****************************************************************************80
!
!! CDGQF computes a Gauss quadrature formula with default A, B and simple knots.
!
!  Discussion:
!
!    This routine computes all the knots and weights of a Gauss quadrature
!    formula with a classical weight function with default values for A and B,
!    and only simple knots.
!
!    There are no moments checks and no printing is done.
!
!    Use routine EIQFS to evaluate a quadrature computed by CGQFS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 January 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, real ( kind = 8 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 8 ) BETA, the value of Beta, if needed.
!
!    Output, real ( kind = 8 ) T(NT), the knots.
!
!    Output, real ( kind = 8 ) WTS(NT), the weights.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) nt

  REAL    ( kind = 8 ) aj(nt)
  REAL    ( kind = 8 ) alpha
  REAL    ( kind = 8 ) beta
  REAL    ( kind = 8 ) bj(nt)
  INTEGER ( kind = 4 ) kind
  REAL    ( kind = 8 ) t(nt)
  REAL    ( kind = 8 ) wts(nt)
  REAL    ( kind = 8 ) zemu

  CALL parchk ( kind, 2 * nt, alpha, beta )
!
!  Get the Jacobi matrix and zero-th moment.
!
  CALL class_matrix ( kind, nt, alpha, beta, aj, bj, zemu )
!
!  Compute the knots and weights.
!
  CALL sgqf ( nt, aj, bj, zemu, t, wts )

  RETURN
END SUBROUTINE Cdgqf

SUBROUTINE cgqf ( nt, kind, alpha, beta, a, b, t, wts )

!*****************************************************************************80
!
!! CGQF computes knots and weights of a Gauss quadrature formula.
!
!  Discussion:
!
!    The user may specify the interval (A,B).
!
!    Only simple knots are produced.
!
!    Use routine EIQFS to evaluate this quadrature formula.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 February 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
!    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
!
!    Input, real ( kind = 8 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 8 ) BETA, the value of Beta, if needed.
!
!    Input, real ( kind = 8 ) A, B, the interval endpoints, or
!    other parameters.
!
!    Output, real ( kind = 8 ) T(NT), the knots.
!
!    Output, real ( kind = 8 ) WTS(NT), the weights.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) nt

  REAL    ( kind = 8 ) a
  REAL    ( kind = 8 ) alpha
  REAL    ( kind = 8 ) b
  REAL    ( kind = 8 ) beta
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) kind
  INTEGER ( kind = 4 ), ALLOCATABLE :: mlt(:)
  INTEGER ( kind = 4 ), ALLOCATABLE :: ndx(:)
  REAL    ( kind = 8 ) t(nt)
  REAL    ( kind = 8 ) wts(nt)
!
!  Compute the Gauss quadrature formula for default values of A and B.
!
  CALL cdgqf ( nt, kind, alpha, beta, t, wts )
!
!  Prepare to scale the quadrature formula to other weight function with 
!  valid A and B.
!
  ALLOCATE ( mlt(1:nt) )

  mlt(1:nt) = 1

  ALLOCATE ( ndx(1:nt) )

  DO i = 1, nt 
    ndx(i) = i
  END DO

  CALL scqf ( nt, t, mlt, wts, nt, ndx, wts, t, kind, alpha, beta, a, b )

  DEALLOCATE ( mlt )
  DEALLOCATE ( ndx )

  RETURN
END SUBROUTINE cgqf
SUBROUTINE ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  IMPLICIT NONE

  CHARACTER              c
  INTEGER   ( kind = 4 ) itemp

  itemp = ICHAR ( c )

  IF ( 97 <= itemp .AND. itemp <= 122 ) THEN
    c = CHAR ( itemp - 32 )
  END IF

  RETURN
END SUBROUTINE ch_cap
FUNCTION ch_eqi ( c1, c2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Example:
!
!    CH_EQI ( 'A', 'a' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  IMPLICIT NONE

  LOGICAL   ch_eqi
  CHARACTER c1
  CHARACTER c1_cap
  CHARACTER c2
  CHARACTER c2_cap

  c1_cap = c1
  c2_cap = c2

  CALL ch_cap ( c1_cap )
  CALL ch_cap ( c2_cap )

  IF ( c1_cap == c2_cap ) THEN
    ch_eqi = .TRUE.
  ELSE
    ch_eqi = .FALSE.
  END IF

  RETURN
END FUNCTION ch_eqi
SUBROUTINE ch_to_digit ( c, digit )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.  
!    If C was 'illegal', then DIGIT is -1.
!
  IMPLICIT NONE

  CHARACTER              c
  INTEGER   ( kind = 4 ) digit

  IF ( LGE ( c, '0' ) .AND. LLE ( c, '9' ) ) THEN

    digit = ICHAR ( c ) - 48

  ELSE IF ( c == ' ' ) THEN

    digit = 0

  ELSE

    digit = -1

  END IF

  RETURN
END SUBROUTINE ch_to_digit
SUBROUTINE class_matrix ( kind, m, alpha, beta, aj, bj, zemu )

!*****************************************************************************80
!
!! CLASS_MATRIX computes the Jacobi matrix for a quadrature rule.
!
!  Discussion:
!
!    This routine computes the diagonal AJ and sub-diagonal BJ
!    elements of the order M tridiagonal symmetric Jacobi matrix
!    associated with the polynomials orthogonal with respect to
!    the weight function specified by KIND.
!
!    For weight functions 1-7, M elements are defined in BJ even
!    though only M-1 are needed.  For weight function 8, BJ(M) is
!    set to zero.
!
!    The zero-th moment of the weight function is returned in ZEMU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, integer ( kind = 4 ) M, the order of the Jacobi matrix.
!
!    Input, real ( kind = 8 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 8 ) BETA, the value of Beta, if needed.
!
!    Output, real ( kind = 8 ) AJ(M), BJ(M), the diagonal and subdiagonal
!    of the Jacobi matrix.
!
!    Output, real ( kind = 8 ) ZEMU, the zero-th moment.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) m

  REAL    ( kind = 8 ) a2b2
  REAL    ( kind = 8 ) ab
  REAL    ( kind = 8 ) aba
  REAL    ( kind = 8 ) abi
  REAL    ( kind = 8 ) abj
  REAL    ( kind = 8 ) abti
  REAL    ( kind = 8 ) aj(m)
  REAL    ( kind = 8 ) alpha
  REAL    ( kind = 8 ) apone
  REAL    ( kind = 8 ) beta
  REAL    ( kind = 8 ) bj(m)
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) kind
  REAL    ( kind = 8 ), PARAMETER :: pi = 3.14159265358979323846264338327950D+00
  REAL    ( kind = 8 ) r8_gamma
  REAL    ( kind = 8 ) temp
  REAL    ( kind = 8 ) temp2
  REAL    ( kind = 8 ) zemu

  temp = EPSILON ( temp )

  CALL parchk ( kind, 2 * m - 1, alpha, beta )

  temp2 = 0.5D+00

  IF ( 500.0D+00 * temp < ABS ( ( r8_gamma ( temp2 ) )**2 - pi ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CLASS_MATRIX - Fatal error!'
    WRITE ( *, '(a)' ) '  Gamma function does not match machine parameters.'
    STOP
  END IF

  IF ( kind == 1 ) THEN

    ab = 0.0D+00

    zemu = 2.0D+00 / ( ab + 1.0D+00 )

    aj(1:m) = 0.0D+00

    DO i = 1, m
      abi = i + ab * MOD ( i, 2 )
      abj = 2 * i + ab
      bj(i) = abi * abi / ( abj * abj - 1.0D+00 )
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 2 ) THEN

    zemu = pi

    aj(1:m) = 0.0D+00

    bj(1) =  SQRT ( 0.5D+00 )
    bj(2:m) = 0.5D+00

  ELSE IF ( kind == 3 ) THEN

    ab = alpha * 2.0D+00
    zemu = 2.0D+00**( ab + 1.0D+00 ) * r8_gamma ( alpha + 1.0D+00 )**2 &
      / r8_gamma ( ab + 2.0D+00 )

    aj(1:m) = 0.0D+00
    bj(1) = 1.0D+00 / ( 2.0D+00 * alpha + 3.0D+00 )
    DO i = 2, m
      bj(i) = i * ( i + ab ) / ( 4.0D+00 * ( i + alpha )**2 - 1.0D+00 )
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 4 ) THEN

    ab = alpha + beta
    abi = 2.0D+00 + ab
    zemu = 2.0D+00**( ab + 1.0D+00 ) * r8_gamma ( alpha + 1.0D+00 ) &
      * r8_gamma ( beta + 1.0D+00 ) / r8_gamma ( abi )
    aj(1) = ( beta - alpha ) / abi
    bj(1) = 4.0D+00 * ( 1.0 + alpha ) * ( 1.0D+00 + beta ) &
      / ( ( abi + 1.0D+00 ) * abi * abi )
    a2b2 = beta * beta - alpha * alpha

    DO i = 2, m
      abi = 2.0D+00 * i + ab
      aj(i) = a2b2 / ( ( abi - 2.0D+00 ) * abi )
      abi = abi**2
      bj(i) = 4.0D+00 * i * ( i + alpha ) * ( i + beta ) * ( i + ab ) &
        / ( ( abi - 1.0D+00 ) * abi )
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 5 ) THEN

    zemu = r8_gamma ( alpha + 1.0D+00 )

    DO i = 1, m
      aj(i) = 2.0D+00 * i - 1.0D+00 + alpha
      bj(i) = i * ( i + alpha )
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 6 ) THEN

    zemu = r8_gamma ( ( alpha + 1.0D+00 ) / 2.0D+00 )

    aj(1:m) = 0.0D+00

    DO i = 1, m
      bj(i) = ( i + alpha * MOD ( i, 2 ) ) / 2.0D+00
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 7 ) THEN

    ab = alpha
    zemu = 2.0D+00 / ( ab + 1.0D+00 )

    aj(1:m) = 0.0D+00

    DO i = 1, m
      abi = i + ab * MOD ( i, 2 )
      abj = 2 * i + ab
      bj(i) = abi * abi / ( abj * abj - 1.0D+00 )
    END DO
    bj(1:m) =  SQRT ( bj(1:m) )

  ELSE IF ( kind == 8 ) THEN

    ab = alpha + beta
    zemu = r8_gamma ( alpha + 1.0D+00 ) * r8_gamma ( - ( ab + 1.0D+00 ) ) &
      / r8_gamma ( - beta )
    apone = alpha + 1.0D+00
    aba = ab * apone
    aj(1) = - apone / ( ab + 2.0D+00 )
    bj(1) = - aj(1) * ( beta + 1.0D+00 ) / ( ab + 2.0D+00 ) / ( ab + 3.0D+00 )
    DO i = 2, m
      abti = ab + 2.0D+00 * i
      aj(i) = aba + 2.0D+00 * ( ab + i ) * ( i - 1 )
      aj(i) = - aj(i) / abti / ( abti - 2.0D+00 )
    END DO

    DO i = 2, m - 1
      abti = ab + 2.0D+00 * i
      bj(i) = i * ( alpha + i ) / ( abti - 1.0D+00 ) * ( beta + i ) &
        / ( abti**2 ) * ( ab + i ) / ( abti + 1.0D+00 )
    END DO

    bj(m) = 0.0D+00
    bj(1:m) =  SQRT ( bj(1:m) )

  END IF

  RETURN
END SUBROUTINE class_matrix
SUBROUTINE get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) ios
  INTEGER ( kind = 4 ) iunit
  LOGICAL              lopen

  iunit = 0

  DO i = 1, 99

    IF ( i /= 5 .AND. i /= 6 .AND. i /= 9 ) THEN

      INQUIRE ( unit = i, opened = lopen, iostat = ios )

      IF ( ios == 0 ) THEN
        IF ( .NOT. lopen ) THEN
          iunit = i
          RETURN
        END IF
      END IF

    END IF

  END DO

  RETURN
END SUBROUTINE get_unit
SUBROUTINE imtqlx ( n, d, e, z )

!*****************************************************************************80
!
!! IMTQLX diagonalizes a symmetric tridiagonal matrix.
!
!  Discussion:
!
!    This routine is a slightly modified version of the EISPACK routine to 
!    perform the implicit QL algorithm on a symmetric tridiagonal matrix. 
!
!    The authors thank the authors of EISPACK for permission to use this
!    routine. 
!
!    It has been modified to produce the product Q' * Z, where Z is an input 
!    vector and Q is the orthogonal matrix diagonalizing the input matrix.  
!    The changes consist (essentially) of applying the orthogonal transformations
!    directly to Z as they are generated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!    Roger Martin, James Wilkinson,
!    The Implicit QL Algorithm,
!    Numerische Mathematik,
!    Volume 12, Number 5, December 1968, pages 377-383.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input/output, real ( kind = 8 ) D(N), the diagonal entries of the matrix.
!    On output, the information in D has been overwritten.
!
!    Input/output, real ( kind = 8 ) E(N), the subdiagonal entries of the 
!    matrix, in entries E(1) through E(N-1).  On output, the information in
!    E has been overwritten.
!
!    Input/output, real ( kind = 8 ) Z(N).  On input, a vector.  On output,
!    the value of Q' * Z, where Q is the matrix that diagonalizes the
!    input symmetric tridiagonal matrix.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) n

  REAL    ( kind = 8 ) b
  REAL    ( kind = 8 ) c
  REAL    ( kind = 8 ) d(n)
  REAL    ( kind = 8 ) e(n)
  REAL    ( kind = 8 ) f
  REAL    ( kind = 8 ) g
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) ii
  INTEGER ( kind = 4 ), PARAMETER :: itn = 30
  INTEGER ( kind = 4 ) j
  INTEGER ( kind = 4 ) k
  INTEGER ( kind = 4 ) l
  INTEGER ( kind = 4 ) m
  INTEGER ( kind = 4 ) mml
  REAL    ( kind = 8 ) p
  REAL    ( kind = 8 ) prec
  REAL    ( kind = 8 ) r
  REAL    ( kind = 8 ) s
  REAL    ( kind = 8 ) z(n)

  prec = EPSILON ( prec )

  IF ( n == 1 ) THEN
    RETURN
  END IF

  e(n) = 0.0D+00

  DO l = 1, n

    j = 0

    DO

      DO m = l, n

        IF ( m == n ) THEN
          EXIT
        END IF

        IF ( ABS ( e(m) ) <= prec * ( ABS ( d(m) ) + ABS ( d(m+1) ) ) ) THEN
          EXIT
        END IF

      END DO

      p = d(l)

      IF ( m == l ) THEN
        EXIT
      END IF

      IF ( itn <= j ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'IMTQLX - Fatal error!'
        WRITE ( *, '(a)' ) '  Iteration limit exceeded.'
        WRITE ( *, '(a,i8)' ) '  J = ', j
        WRITE ( *, '(a,i8)' ) '  L = ', l
        WRITE ( *, '(a,i8)' ) '  M = ', m
        WRITE ( *, '(a,i8)' ) '  N = ', n
        STOP
      END IF

      j = j + 1
      g = ( d(l+1) - p ) / ( 2.0D+00 * e(l) )
      r =  SQRT ( g * g + 1.0D+00 )
      g = d(m) - p + e(l) / ( g + SIGN ( r, g ) )
      s = 1.0D+00
      c = 1.0D+00
      p = 0.0D+00
      mml = m - l

      DO ii = 1, mml

        i = m - ii
        f = s * e(i)
        b = c * e(i)

        IF ( ABS ( g ) <= ABS ( f ) ) THEN
          c = g / f
          r =  SQRT ( c * c + 1.0D+00 )
          e(i+1) = f * r
          s = 1.0D+00 / r
          c = c * s
        ELSE
          s = f / g
          r =  SQRT ( s * s + 1.0D+00 )
          e(i+1) = g * r
          c = 1.0D+00 / r
          s = s * c
        END IF

        g = d(i+1) - p
        r = ( d(i) - g ) * s + 2.0D+00 * c * b
        p = s * r
        d(i+1) = g + p
        g = c * r - b
        f = z(i+1)
        z(i+1) = s * z(i) + c * f
        z(i) = c * z(i) - s * f

      END DO

      d(l) = d(l) - p
      e(l) = g
      e(m) = 0.0D+00

    END DO

  END DO
!
!  Sorting.
!
  DO ii = 2, n

    i = ii - 1
    k = i
    p = d(i)

    DO j = ii, n
      IF ( d(j) < p ) THEN
        k = j
        p = d(j)
      END IF
    END DO

    IF ( k /= i ) THEN
      d(k) = d(i)
      d(i) = p
      p = z(i)
      z(i) = z(k)
      z(k) = p
    END IF

  END DO

  RETURN
END SUBROUTINE imtqlx
SUBROUTINE parchk ( kind, m, alpha, beta )

!*****************************************************************************80
!
!! PARCHK checks parameters ALPHA and BETA for classical weight functions. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev,            (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,inf)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-inf,inf)  |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,inf)     (x-a)^alpha*(x+b)^beta
!
!    Input, integer ( kind = 4 ) M, the order of the highest moment to
!    be calculated.  This value is only needed when KIND = 8.
!
!    Input, real ( kind = 8 ) ALPHA, BETA, the parameters, if required
!    by the value of KIND.
!
  IMPLICIT NONE

  REAL    ( kind = 8 ) alpha
  REAL    ( kind = 8 ) beta
  INTEGER ( kind = 4 ) kind
  INTEGER ( kind = 4 ) m
  REAL    ( kind = 8 ) tmp

  IF ( kind <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'PARCHK - Fatal error!'
    WRITE ( *, '(a)' ) '  KIND <= 0.'
    STOP
  END IF
!
!  Check ALPHA for Gegenbauer, Jacobi, Laguerre, Hermite, Exponential.
!
  IF ( 3 <= kind .AND. alpha <= -1.0D+00 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'PARCHK - Fatal error!'
    WRITE ( *, '(a)' ) '  3 <= KIND and ALPHA <= -1.'
    STOP
  END IF
!
!  Check BETA for Jacobi.
!
  IF ( kind == 4 .AND. beta <= -1.0D+00 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'PARCHK - Fatal error!'
    WRITE ( *, '(a)' ) '  KIND == 4 and BETA <= -1.0.'
    STOP
  END IF
!
!  Check ALPHA and BETA for rational.
!
  IF ( kind == 8 ) THEN
    tmp = alpha + beta + m + 1.0D+00
    IF ( 0.0D+00 <= tmp .OR. tmp <= beta ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'PARCHK - Fatal error!'
      WRITE ( *, '(a)' ) '  KIND == 8 but condition on ALPHA and BETA fails.'
      STOP
    END IF
  END IF

  RETURN
END SUBROUTINE parchk
FUNCTION r8_epsilon ( )

!*****************************************************************************80
!
!! R8_EPSILON returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    15 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EPSILON, the R8 round-off unit.
!
  IMPLICIT NONE

  REAL    ( kind = 8 ) d
  REAL    ( kind = 8 ) d_test
  REAL    ( kind = 8 ) r8_epsilon

  d = 1.0D+00
  d_test = 1.0D+00 + d / 2.0D+00

  DO WHILE ( 1.0D+00 < d_test )
    d = d / 2.0D+00
    d_test = 1.0D+00 + d / 2.0D+00
  END DO

  r8_epsilon = d

  RETURN
END FUNCTION r8_epsilon
FUNCTION r8_gamma ( x )

!*****************************************************************************80
!
!! R8_GAMMA evaluates Gamma(X) for a real argument.
!
!  Discussion:
!
!    This routine calculates the gamma function for a real argument X.
!
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the gamma
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for 12 <= X are from reference 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 February 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    An Overview of Software Development for Special Functions,
!    in Numerical Analysis Dundee, 1975,
!    edited by GA Watson,
!    Lecture Notes in Mathematics 506,
!    Springer, 1976.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) R8_GAMMA, the value of the function.
!
  IMPLICIT NONE

  REAL    ( kind = 8 ), DIMENSION ( 7 ) :: c = (/ &
   -1.910444077728D-03, &
    8.4171387781295D-04, &
   -5.952379913043012D-04, &
    7.93650793500350248D-04, &
   -2.777777777777681622553D-03, &
    8.333333333333333331554247D-02, &
    5.7083835261D-03 /)
  REAL    ( kind = 8 ), PARAMETER :: eps = 2.22D-16
  REAL    ( kind = 8 ) fact
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) n
  REAL    ( kind = 8 ), DIMENSION ( 8 ) :: p = (/ &
    -1.71618513886549492533811D+00, &
     2.47656508055759199108314D+01, &
    -3.79804256470945635097577D+02, &
     6.29331155312818442661052D+02, &
     8.66966202790413211295064D+02, &
    -3.14512729688483675254357D+04, &
    -3.61444134186911729807069D+04, &
     6.64561438202405440627855D+04 /)
  LOGICAL PARITY
  REAL    ( kind = 8 ), PARAMETER :: pi = 3.1415926535897932384626434D+00
  REAL    ( kind = 8 ), DIMENSION ( 8 ) :: q = (/ &
    -3.08402300119738975254353D+01, &
     3.15350626979604161529144D+02, &
    -1.01515636749021914166146D+03, &
    -3.10777167157231109440444D+03, &
     2.25381184209801510330112D+04, &
     4.75584627752788110767815D+03, &
    -1.34659959864969306392456D+05, &
    -1.15132259675553483497211D+05 /)
  REAL    ( kind = 8 ) r8_gamma
  REAL    ( kind = 8 ) res
  REAL    ( kind = 8 ), PARAMETER :: sqrtpi = 0.9189385332046727417803297D+00
  REAL    ( kind = 8 ) sum
  REAL    ( kind = 8 ) x
  REAL    ( kind = 8 ), PARAMETER :: xbig = 171.624D+00
  REAL    ( kind = 8 ) xden
  REAL    ( kind = 8 ), PARAMETER :: xinf = 1.0D+30
  REAL    ( kind = 8 ), PARAMETER :: xminin = 2.23D-308
  REAL    ( kind = 8 ) xnum
  REAL    ( kind = 8 ) y
  REAL    ( kind = 8 ) y1
  REAL    ( kind = 8 ) ysq
  REAL    ( kind = 8 ) z

  PARITY = .FALSE.
  fact = 1.0D+00
  n = 0
  y = x
!
!  Argument is negative.
!
  IF ( y <= 0.0D+00 ) THEN

    y = - x
    y1 = AINT ( y )
    res = y - y1

    IF ( res /= 0.0D+00 ) THEN

      IF ( y1 /= AINT ( y1 * 0.5D+00 ) * 2.0D+00 ) THEN
        PARITY = .TRUE.
      END IF

      fact = - pi / SIN ( pi * res )
      y = y + 1.0D+00

    ELSE

      res = xinf
      r8_gamma = res
      RETURN

    END IF

  END IF
!
!  Argument is positive.
!
  IF ( y < eps ) THEN
!
!  Argument < EPS.
!
    IF ( xminin <= y ) THEN
      res = 1.0D+00 / y
    ELSE
      res = xinf
      r8_gamma = res
      RETURN
    END IF

  ELSE IF ( y < 12.0D+00 ) THEN

    y1 = y
!
!  0.0 < argument < 1.0.
!
    IF ( y < 1.0D+00 ) THEN

      z = y
      y = y + 1.0D+00
!
!  1.0 < argument < 12.0.
!  Reduce argument if necessary.
!
    ELSE

      n = INT ( y ) - 1
      y = y - REAL ( n, kind = 8 )
      z = y - 1.0D+00

    END IF
!
!  Evaluate approximation for 1.0 < argument < 2.0.
!
    xnum = 0.0D+00
    xden = 1.0D+00
    DO i = 1, 8
      xnum = ( xnum + p(i) ) * z
      xden = xden * z + q(i)
    END DO

    res = xnum / xden + 1.0D+00
!
!  Adjust result for case  0.0 < argument < 1.0.
!
    IF ( y1 < y ) THEN

      res = res / y1
!
!  Adjust result for case 2.0 < argument < 12.0.
!
    ELSE IF ( y < y1 ) THEN

      DO i = 1, n
        res = res * y
        y = y + 1.0D+00
      END DO

    END IF

  ELSE
!
!  Evaluate for 12.0 <= argument.
!
    IF ( y <= xbig ) THEN

      ysq = y * y
      sum = c(7)
      DO i = 1, 6
        sum = sum / ysq + c(i)
      END DO
      sum = sum / y - y + sqrtpi
      sum = sum + ( y - 0.5D+00 ) * LOG ( y )
      res = EXP ( sum )

    ELSE

      res = xinf
      r8_gamma = res
      RETURN

    END IF

  END IF
!
!  Final adjustments and return.
!
  IF ( PARITY ) THEN
    res = - res
  END IF

  IF ( fact /= 1.0D+00 ) THEN
    res = fact / res
  END IF

  r8_gamma = res

  RETURN
END FUNCTION r8_gamma
SUBROUTINE s_to_i4 ( s, ival, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an I4 from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters of S 
!    used to make IVAL.
!
  IMPLICIT NONE

  CHARACTER              c
  INTEGER   ( kind = 4 ) i
  INTEGER   ( kind = 4 ) ierror
  INTEGER   ( kind = 4 ) isgn
  INTEGER   ( kind = 4 ) istate
  INTEGER   ( kind = 4 ) ival
  INTEGER   ( kind = 4 ) length
  CHARACTER ( len = * )  s

  ierror = 0
  istate = 0
  isgn = 1
  ival = 0

  DO i = 1, LEN_TRIM ( s )

    c = s(i:i)
!
!  Haven't read anything.
!
    IF ( istate == 0 ) THEN

      IF ( c == ' ' ) THEN

      ELSE IF ( c == '-' ) THEN
        istate = 1
        isgn = -1
      ELSE IF ( c == '+' ) THEN
        istate = 1
        isgn = + 1
      ELSE IF ( LLE ( '0', c ) .AND. LLE ( c, '9' ) ) THEN
        istate = 2
        ival = ICHAR ( c ) - ICHAR ( '0' )
      ELSE
        ierror = 1
        RETURN
      END IF
!
!  Have read the sign, expecting digits.
!
    ELSE IF ( istate == 1 ) THEN

      IF ( c == ' ' ) THEN

      ELSE IF ( LLE ( '0', c ) .AND. LLE ( c, '9' ) ) THEN
        istate = 2
        ival = ICHAR ( c ) - ICHAR ( '0' )
      ELSE
        ierror = 1
        RETURN
      END IF
!
!  Have read at least one digit, expecting more.
!
    ELSE IF ( istate == 2 ) THEN

      IF ( LLE ( '0', c ) .AND. LLE ( c, '9' ) ) THEN
        ival = 10 * ival + ICHAR ( c ) - ICHAR ( '0' )
      ELSE
        ival = isgn * ival
        length = i - 1
        RETURN
      END IF

    END IF

  END DO
!
!  If we read all the characters in the string, see if we're OK.
!
  IF ( istate == 2 ) THEN
    ival = isgn * ival
    length = LEN_TRIM ( s )
  ELSE
    ierror = 1
    length = 0
  END IF

  RETURN
END SUBROUTINE s_to_i4
SUBROUTINE s_to_r8 ( s, dval, ierror, length )

!*****************************************************************************80
!
!! S_TO_R8 reads an R8 from a string.
!
!  Discussion:
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 8 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  IMPLICIT NONE

  CHARACTER              c
  LOGICAL                ch_eqi
  REAL      ( kind = 8 ) dval
  INTEGER   ( kind = 4 ) ierror
  INTEGER   ( kind = 4 ) ihave
  INTEGER   ( kind = 4 ) isgn
  INTEGER   ( kind = 4 ) iterm
  INTEGER   ( kind = 4 ) jbot
  INTEGER   ( kind = 4 ) jsgn
  INTEGER   ( kind = 4 ) jtop
  INTEGER   ( kind = 4 ) length
  INTEGER   ( kind = 4 ) nchar
  INTEGER   ( kind = 4 ) ndig
  REAL      ( kind = 8 ) rbot
  REAL      ( kind = 8 ) rexp
  REAL      ( kind = 8 ) rtop
  CHARACTER ( len = * )  s

  nchar = LEN_TRIM ( s )

  ierror = 0
  dval = 0.0D+00
  length = -1
  isgn = 1
  rtop = 0
  rbot = 1
  jsgn = 1
  jtop = 0
  jbot = 1
  ihave = 1
  iterm = 0

  DO

    length = length + 1

    IF ( nchar < length+1 ) THEN
      EXIT
    END IF

    c = s(length+1:length+1)
!
!  Blank character.
!
    IF ( c == ' ' ) THEN

      IF ( ihave == 2 ) THEN

      ELSE IF ( ihave == 6 .OR. ihave == 7 ) THEN
        iterm = 1
      ELSE IF ( 1 < ihave ) THEN
        ihave = 11
      END IF
!
!  Comma.
!
    ELSE IF ( c == ',' .OR. c == ';' ) THEN

      IF ( ihave /= 1 ) THEN
        iterm = 1
        ihave = 12
        length = length + 1
      END IF
!
!  Minus sign.
!
    ELSE IF ( c == '-' ) THEN

      IF ( ihave == 1 ) THEN
        ihave = 2
        isgn = -1
      ELSE IF ( ihave == 6 ) THEN
        ihave = 7
        jsgn = -1
      ELSE
        iterm = 1
      END IF
!
!  Plus sign.
!
    ELSE IF ( c == '+' ) THEN

      IF ( ihave == 1 ) THEN
        ihave = 2
      ELSE IF ( ihave == 6 ) THEN
        ihave = 7
      ELSE
        iterm = 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( c == '.' ) THEN

      IF ( ihave < 4 ) THEN
        ihave = 4
      ELSE IF ( 6 <= ihave .AND. ihave <= 8 ) THEN
        ihave = 9
      ELSE
        iterm = 1
      END IF
!
!  Scientific notation exponent marker.
!
    ELSE IF ( ch_eqi ( c, 'E' ) .OR. ch_eqi ( c, 'D' ) ) THEN

      IF ( ihave < 6 ) THEN
        ihave = 6
      ELSE
        iterm = 1
      END IF
!
!  Digit.
!
    ELSE IF (  ihave < 11 .AND. LLE ( '0', c ) .AND. LLE ( c, '9' ) ) THEN

      IF ( ihave <= 2 ) THEN
        ihave = 3
      ELSE IF ( ihave == 4 ) THEN
        ihave = 5
      ELSE IF ( ihave == 6 .OR. ihave == 7 ) THEN
        ihave = 8
      ELSE IF ( ihave == 9 ) THEN
        ihave = 10
      END IF

      CALL ch_to_digit ( c, ndig )

      IF ( ihave == 3 ) THEN
        rtop = 10.0D+00 * rtop + REAL ( ndig, kind = 8 )
      ELSE IF ( ihave == 5 ) THEN
        rtop = 10.0D+00 * rtop + REAL ( ndig, kind = 8 )
        rbot = 10.0D+00 * rbot
      ELSE IF ( ihave == 8 ) THEN
        jtop = 10 * jtop + ndig
      ELSE IF ( ihave == 10 ) THEN
        jtop = 10 * jtop + ndig
        jbot = 10 * jbot
      END IF
!
!  Anything else is regarded as a terminator.
!
    ELSE
      iterm = 1
    END IF
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    IF ( iterm == 1 ) THEN
      EXIT
    END IF

  END DO
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to NCHAR.
!
  IF ( iterm /= 1 .AND. length+1 == nchar ) THEN
    length = nchar
  END IF
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  IF ( ihave == 1 .OR. ihave == 2 .OR. ihave == 6 .OR. ihave == 7 ) THEN
    ierror = ihave
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_TO_R8 - Serious error!'
    WRITE ( *, '(a)' ) '  Illegal or nonnumeric input:'
    WRITE ( *, '(a)' ) '    ' // TRIM ( s )
    RETURN
  END IF
!
!  Number seems OK.  Form it.
!
  IF ( jtop == 0 ) THEN
    rexp = 1.0D+00
  ELSE
    IF ( jbot == 1 ) THEN
      rexp = 10.0D+00 ** ( jsgn * jtop )
    ELSE
      rexp = 10.0D+00 ** ( REAL ( jsgn * jtop, kind = 8 ) &
        / REAL ( jbot, kind = 8 ) )
    END IF
  END IF

  dval = REAL ( isgn, kind = 8 ) * rexp * rtop / rbot

  RETURN
END SUBROUTINE s_to_r8
SUBROUTINE scqf ( nt, t, mlt, wts, nwts, ndx, swts, st, kind, alpha, beta, a, &
  b )

!*****************************************************************************80
!
!! SCQF scales a quadrature formula to a nonstandard interval.
!
!  Discussion:
!
!    The arrays WTS and SWTS may coincide.
!
!    The arrays T and ST may coincide.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    27 December 2009
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, real ( kind = 8 ) T(NT), the original knots.
!
!    Input, integer ( kind = 4 ) MLT(NT), the multiplicity of the knots.
!
!    Input, real ( kind = 8 ) WTS(NWTS), the weights.
!
!    Input, integer ( kind = 4 ) NWTS, the number of weights.
!
!    Input, integer ( kind = 4 ) NDX(NT), used to index the array WTS.  
!    For more details see the comments in CAWIQ.
!
!    Output, real ( kind = 8 ) SWTS(NWTS), the scaled weights.
!
!    Output, real ( kind = 8 ) ST(NT), the scaled knots.
!
!    Input, integer ( kind = 4 ) KIND, the rule.
!    1, Legendre,             (a,b)       1.0
!    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
!    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
!    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
!    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
!    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
!    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
!    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
!    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
!
!    Input, real ( kind = 8 ) ALPHA, the value of Alpha, if needed.
!
!    Input, real ( kind = 8 ) BETA, the value of Beta, if needed.
!
!    Input, real ( kind = 8 ) A, B, the interval endpoints.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) nt
  INTEGER ( kind = 4 ) nwts

  REAL    ( kind = 8 ) a
  REAL    ( kind = 8 ) al
  REAL    ( kind = 8 ) alpha
  REAL    ( kind = 8 ) b
  REAL    ( kind = 8 ) be
  REAL    ( kind = 8 ) beta
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) k
  INTEGER ( kind = 4 ) kind
  INTEGER ( kind = 4 ) l
  INTEGER ( kind = 4 ) mlt(nt)
  INTEGER ( kind = 4 ) ndx(nt)
  REAL    ( kind = 8 ) p
  REAL    ( kind = 8 ) shft
  REAL    ( kind = 8 ) slp
  REAL    ( kind = 8 ) st(nt)
  REAL    ( kind = 8 ) swts(nwts)
  REAL    ( kind = 8 ) t(nt)
  REAL    ( kind = 8 ) temp
  REAL    ( kind = 8 ) tmp
  REAL    ( kind = 8 ) wts(nwts)

  temp = EPSILON ( temp )

  CALL parchk ( kind, 1, alpha, beta )

  IF ( kind == 1 ) THEN

    al = 0.0D+00
    be = 0.0D+00

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  ELSE IF ( kind == 2 ) THEN

    al = -0.5D+00
    be = -0.5D+00

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  ELSE IF ( kind == 3 ) THEN

    al = alpha
    be = alpha

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  ELSE IF ( kind == 4 ) THEN

    al = alpha
    be = beta

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  ELSE IF ( kind == 5 ) THEN

    IF ( b <= 0.0D+00 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  B <= 0'
      STOP
    END IF

    shft = a
    slp = 1.0D+00 / b
    al = alpha
    be = 0.0D+00

  ELSE IF ( kind == 6 ) THEN

    IF ( b <= 0.0D+00 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  B <= 0.'
      STOP
    END IF

    shft = a
    slp = 1.0D+00 / SQRT ( b )
    al = alpha
    be = 0.0D+00

  ELSE IF ( kind == 7 ) THEN

    al = alpha
    be = 0.0D+00

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  ELSE IF ( kind == 8 ) THEN

    IF ( a + b <= 0.0D+00 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  A + B <= 0.'
      STOP
    END IF

    shft = a
    slp = a + b
    al = alpha
    be = beta

  ELSE IF ( kind == 9 ) THEN

    al = 0.5D+00
    be = 0.5D+00

    IF ( ABS ( b - a ) <= temp ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'SCQF - Fatal error!'
      WRITE ( *, '(a)' ) '  |B - A| too small.'
      STOP
    END IF

    shft = ( a + b ) / 2.0D+00
    slp = ( b - a ) / 2.0D+00

  END IF

  p = slp**( al + be + 1.0D+00 )

  DO k = 1, nt

    st(k) = shft + slp * t(k)
    l = ABS ( ndx(k) )

    IF ( l /= 0 ) THEN
      tmp = p
      DO i = l, l + mlt(k) - 1
        swts(i) = wts(i) * tmp
        tmp = tmp * slp
      END DO
    END IF

  END DO

  RETURN
END SUBROUTINE scqf
SUBROUTINE sgqf ( nt, aj, bj, zemu, t, wts )

!*****************************************************************************80
!
!! SGQF computes knots and weights of a Gauss Quadrature formula.
!
!  Discussion:
!
!    This routine computes all the knots and weights of a Gauss quadrature
!    formula with simple knots from the Jacobi matrix and the zero-th
!    moment of the weight function, using the Golub-Welsch technique.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    04 January 2010
!
!  Author:
!
!    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Sylvan Elhay, Jaroslav Kautsky,
!    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
!    Interpolatory Quadrature,
!    ACM Transactions on Mathematical Software,
!    Volume 13, Number 4, December 1987, pages 399-415.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NT, the number of knots.
!
!    Input, real ( kind = 8 ) AJ(NT), the diagonal of the Jacobi matrix.
!
!    Input/output, real ( kind = 8 ) BJ(NT), the subdiagonal of the Jacobi 
!    matrix, in entries 1 through NT-1.  On output, BJ has been overwritten.
!
!    Input, real ( kind = 8 ) ZEMU, the zero-th moment of the weight function.
!
!    Output, real ( kind = 8 ) T(NT), the knots.
!
!    Output, real ( kind = 8 ) WTS(NT), the weights.
!
  IMPLICIT NONE

  INTEGER ( kind = 4 ) nt

  REAL    ( kind = 8 ) aj(nt)
  REAL    ( kind = 8 ) bj(nt)
  INTEGER ( kind = 4 ) i
  REAL    ( kind = 8 ) t(nt)
  REAL    ( kind = 8 ) wts(nt)
  REAL    ( kind = 8 ) zemu
!
!  Exit if the zero-th moment is not positive.
!
  IF ( zemu <= 0.0D+00 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'SGQF - Fatal error!'
    WRITE ( *, '(a)' ) '  ZEMU <= 0.'
    STOP
  END IF
!
!  Set up vectors for IMTQLX.
!
  t(1:nt) = aj(1:nt)

  wts(1) = SQRT ( zemu )
  wts(2:nt) = 0.0D+00
!
!  Diagonalize the Jacobi matrix.
!
  CALL imtqlx ( nt, t, bj, wts )

  wts(1:nt) = wts(1:nt)**2

  RETURN
END SUBROUTINE sgqf
