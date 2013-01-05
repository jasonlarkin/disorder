  function dasum(n,dx,incx)
!
!     takes the sum of the absolute values.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dtemp,dasum
  integer(i4) i,incx,m,mp1,n,nincx
!
  dasum = 0.0_dp
  dtemp = 0.0_dp
  if (n.le.0) return
  if (incx.eq.1) go to 20
!
!        code for increment not equal to 1
!
  nincx = n*incx
  do 10 i = 1,nincx,incx
    dtemp = dtemp + dabs(dx(i))
10 continue
  dasum = dtemp
  return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
20 m = mod(n,6_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dtemp = dtemp + dabs(dx(i))
30 continue
  if( n .lt. 6 ) go to 60
40 mp1 = m + 1
  do 50 i = mp1,n,6
    dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2)) &
    + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
50 continue
60 dasum = dtemp
  return
  end
  subroutine daxpy(n,da,dx,incx,dy,incy)
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dy(*),da
  integer(i4) i,incx,incy,ix,iy,m,mp1,n
!
  if (n.le.0) return
  if (da .eq. 0.0d0) return
  if (incx.eq.1.and.incy.eq.1) go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    dy(iy) = dy(iy) + da*dx(ix)
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
20 m = mod(n,4_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dy(i) = dy(i) + da*dx(i)
30 continue
  if( n .lt. 4 ) return
40 mp1 = m + 1
  do 50 i = mp1,n,4
    dy(i) = dy(i) + da*dx(i)
    dy(i + 1) = dy(i + 1) + da*dx(i + 1)
    dy(i + 2) = dy(i + 2) + da*dx(i + 2)
    dy(i + 3) = dy(i + 3) + da*dx(i + 3)
50 continue
  return
  end
  subroutine  dcopy(n,dx,incx,dy,incy)
!
!     copies a vector, x, to a vector, y.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dy(*)
  integer(i4) i,incx,incy,ix,iy,m,mp1,n
!
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    dy(iy) = dx(ix)
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
20 m = mod(n,7_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dy(i) = dx(i)
30 continue
  if( n .lt. 7 ) return
40 mp1 = m + 1
  do 50 i = mp1,n,7
    dy(i) = dx(i)
    dy(i + 1) = dx(i + 1)
    dy(i + 2) = dx(i + 2)
    dy(i + 3) = dx(i + 3)
    dy(i + 4) = dx(i + 4)
    dy(i + 5) = dx(i + 5)
    dy(i + 6) = dx(i + 6)
50 continue
  return
  end
  function ddot(n,dx,incx,dy,incy)
!
!     forms the dot product of two vectors.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dy(*),dtemp,ddot
  integer(i4) i,incx,incy,ix,iy,m,mp1,n
!
  ddot = 0.0_dp
  dtemp = 0.0_dp
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    dtemp = dtemp + dx(ix)*dy(iy)
    ix = ix + incx
    iy = iy + incy
10 continue
  ddot = dtemp
  return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
20 m = mod(n,5_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dtemp = dtemp + dx(i)*dy(i)
30 continue
  if( n .lt. 5 ) go to 60
40 mp1 = m + 1
  do 50 i = mp1,n,5
    dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) + &
     dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
50 continue
60 ddot = dtemp
  return
  end
  subroutine  dscal(n,da,dx,incx)
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) da,dx(*)
  integer(i4) i,incx,m,mp1,n,nincx
!
  if(n.le.0)return
  if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
  nincx = n*incx
  do 10 i = 1,nincx,incx
    dx(i) = da*dx(i)
10 continue
  return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
20 m = mod(n,5_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dx(i) = da*dx(i)
30 continue
  if( n .lt. 5 ) return
40 mp1 = m + 1
  do 50 i = mp1,n,5
    dx(i) = da*dx(i)
    dx(i + 1) = da*dx(i + 1)
    dx(i + 2) = da*dx(i + 2)
    dx(i + 3) = da*dx(i + 3)
    dx(i + 4) = da*dx(i + 4)
50 continue
  return
  end
  subroutine  dswap (n,dx,incx,dy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal one.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dy(*),dtemp
  integer(i4) i,incx,incy,ix,iy,m,mp1,n
!
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    dtemp = dx(ix)
    dx(ix) = dy(iy)
    dy(iy) = dtemp
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
20 m = mod(n,3_i4)
  if( m .eq. 0 ) go to 40
  do 30 i = 1,m
    dtemp = dx(i)
    dx(i) = dy(i)
    dy(i) = dtemp
30 continue
  if( n .lt. 3 ) return
40 mp1 = m + 1
  do 50 i = mp1,n,3
    dtemp = dx(i)
    dx(i) = dy(i)
    dy(i) = dtemp
    dtemp = dx(i + 1)
    dx(i + 1) = dy(i + 1)
    dy(i + 1) = dtemp
    dtemp = dx(i + 2)
    dx(i + 2) = dy(i + 2)
    dy(i + 2) = dtemp
50 continue
  return
  end
  subroutine zaxpy(n,za,zx,incx,zy,incy)
!
!     constant times a vector plus a vector.
!     jack dongarra, 3/11/78.
!
  use datatypes
  implicit none
  complex(dpc) :: zx(*),zy(*),za
  real(dp)     :: dcabs1
  integer(i4)  :: n,incx,incy,ix,iy,i
!
  if (n.le.0)return
  if (dcabs1(za) .eq. 0.0d0) return
  if (incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    zy(iy) = zy(iy) + za*zx(ix)
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!        code for both increments equal to 1
!
20 do 30 i = 1,n
    zy(i) = zy(i) + za*zx(i)
30 continue
  return
  end
  subroutine  zcopy(n,zx,incx,zy,incy)
!
!     copies a vector, x, to a vector, y.
!     jack dongarra, linpack, 4/11/78.
!
  use datatypes
  implicit none
  integer(i4) incx,incy,ix,iy,n,i
  complex(dpc) zx(n),zy(n)
!
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    zy(iy) = zx(ix)
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!        code for both increments equal to 1
!
20 do 30 i = 1,n
    zy(i) = zx(i)
30 continue
  return
  end
  function zdotc(n,zx,incx,zy,incy)
!
!     forms the dot product of a vector.
!     jack dongarra, 3/11/78.
!
  use datatypes
  implicit none
  complex(dpc) zx(*),zy(*),ztemp,zdotc
  real(dp)     conjg
  integer(i4)  n,incx,incy,ix,iy,i
!
  ztemp = (0.0_dp,0.0_dp)
  zdotc = (0.0_dp,0.0_dp)
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    ztemp = ztemp + conjg(zx(ix))*zy(iy)
    ix = ix + incx
    iy = iy + incy
10 continue
  zdotc = ztemp
  return
!
!        code for both increments equal to 1
!
20 do 30 i = 1,n
    ztemp = ztemp + conjg(zx(i))*zy(i)
30 continue
  zdotc = ztemp
  return
  end
  subroutine  zswap(n,zx,incx,zy,incy)
!
!     interchanges two vectors.
!     jack dongarra, 3/11/78.
!
  use datatypes
  implicit none
  complex(dpc) zx(*),zy(*),ztemp
  integer(i4)  incx,incy,n,ix,iy,i
!
  if(n.le.0)return
  if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
  ix = 1
  iy = 1
  if(incx.lt.0)ix = (-n+1)*incx + 1
  if(incy.lt.0)iy = (-n+1)*incy + 1
  do 10 i = 1,n
    ztemp = zx(ix)
    zx(ix) = zy(iy)
    zy(iy) = ztemp
    ix = ix + incx
    iy = iy + incy
10 continue
  return
!
!       code for both increments equal to 1
20 do 30 i = 1,n
    ztemp = zx(i)
    zx(i) = zy(i)
    zy(i) = ztemp
30 continue
  return
  end
  function idamax(n,dx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!
  use datatypes
  implicit none
  real(dp) dx(*),dmax
  integer(i4) i,incx,ix,n,idamax
!
  idamax = 0
  if( n .lt. 1 ) return
  idamax = 1
  if(n.eq.1)return
  if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
  ix = 1
  dmax = dabs(dx(1))
  ix = ix + incx
  do 10 i = 2,n
     if(dabs(dx(ix)).le.dmax) go to 5
     idamax = i
     dmax = dabs(dx(ix))
5    ix = ix + incx
10 continue
  return
!
!        code for increment equal to 1
!
20 dmax = dabs(dx(1))
  do 30 i = 2,n
     if(dabs(dx(i)).le.dmax) go to 30
     idamax = i
     dmax = dabs(dx(i))
30 continue
  return
  end
  function dcabs1(z)
  use datatypes
  implicit none
  complex(dpc) z,zz
  real(dp) t(2),dcabs1
  equivalence (zz,t(1))
  zz = z
  dcabs1 = dabs(t(1)) + dabs(t(2))
  return
  end
  function izamax(n,zx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, 1/15/85.
!
  use datatypes
  implicit none
  complex(dpc) zx(*)
  real(dp) smax
  integer(i4) i,incx,ix,n,izamax
  real(dp) dcabs1
!
  izamax = 0
  if(n.lt.1)return
  izamax = 1
  if(n.eq.1)return
  if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
  ix = 1
  smax = dcabs1(zx(1))
  ix = ix + incx
  do 10 i = 2,n
     if(dcabs1(zx(ix)).le.smax) go to 5
     izamax = i
     smax = dcabs1(zx(ix))
5    ix = ix + incx
10 continue
  return
!
!        code for increment equal to 1
!
20 smax = dcabs1(zx(1))
  do 30 i = 2,n
     if(dcabs1(zx(i)).le.smax) go to 30
     izamax = i
     smax = dcabs1(zx(i))
30 continue
  return
  end
  subroutine DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, &
                     BETA, C, LDC )
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER(len=1)   TRANSA, TRANSB
  INTEGER(i4)        M, N, K, LDA, LDB, LDC
  real(dp)           ALPHA, BETA
!     .. Array Arguments ..
  real(dp)           A( LDA, * ), B( LDB, * ), C( LDC, * )
!     ..
!
!  Purpose
!  =======
!
!  DGEMM  performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A'.
!
!              TRANSA = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B'.
!
!              TRANSB = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - real(dp) array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - real(dp) array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     .. Local Scalars ..
  LOGICAL            NOTA, NOTB
  integer(i4)        I, INFO, J, L, NCOLA, NROWA, NROWB
  real(dp)           TEMP
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!
  NOTA  = LSAME( TRANSA, 'N' )
  NOTB  = LSAME( TRANSB, 'N' )
  IF( NOTA )THEN
     NROWA = M
     NCOLA = K
  else
     NROWA = K
     NCOLA = M
  endif
  IF( NOTB )THEN
     NROWB = K
  else
     NROWB = N
  endif
!
!     Test the input parameters.
!
  INFO = 0
  IF(      ( .NOT.NOTA                 ).AND. &
           ( .NOT.LSAME( TRANSA, 'C' ) ).AND. &
           ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
     INFO = 1
  else IF( ( .NOT.NOTB                 ).AND. &
           ( .NOT.LSAME( TRANSB, 'C' ) ).AND. &
           ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
     INFO = 2
  else IF( M  .LT.0               )THEN
     INFO = 3
  else IF( N  .LT.0               )THEN
     INFO = 4
  else IF( K  .LT.0               )THEN
     INFO = 5
  else IF( LDA.LT.MAX( 1, NROWA ) )THEN
     INFO = 8
  else IF( LDB.LT.MAX( 1, NROWB ) )THEN
     INFO = 10
  else IF( LDC.LT.MAX( 1, M     ) )THEN
     INFO = 13
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DGEMM ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR. &
      ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     And if  alpha.eq.zero.
!
  IF( ALPHA.EQ.ZERO )THEN
     IF( BETA.EQ.ZERO )THEN
        DO 20, J = 1, N
           DO 10, I = 1, M
              C( I, J ) = ZERO
10          CONTINUE
20       CONTINUE
     else
        DO 40, J = 1, N
           DO 30, I = 1, M
              C( I, J ) = BETA*C( I, J )
30          CONTINUE
40       CONTINUE
     endif
     return
  endif
!
!     Start the operations.
!
  IF( NOTB )THEN
     IF( NOTA )THEN
!
!           Form  C := alpha*A*B + beta*C.
!
        DO 90, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 50, I = 1, M
                 C( I, J ) = ZERO
50             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 60, I = 1, M
                 C( I, J ) = BETA*C( I, J )
60             CONTINUE
           endif
           DO 80, L = 1, K
              IF( B( L, J ).NE.ZERO )THEN
                 TEMP = ALPHA*B( L, J )
                 DO 70, I = 1, M
                    C( I, J ) = C( I, J ) + TEMP*A( I, L )
70                CONTINUE
              endif
80          CONTINUE
90       CONTINUE
     else
!
!           Form  C := alpha*A'*B + beta*C
!
        DO 120, J = 1, N
           DO 110, I = 1, M
              TEMP = ZERO
              DO 100, L = 1, K
                 TEMP = TEMP + A( L, I )*B( L, J )
100             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP
              else
                 C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
              endif
110          CONTINUE
120       CONTINUE
     endif
  else
     IF( NOTA )THEN
!
!           Form  C := alpha*A*B' + beta*C
!
        DO 170, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 130, I = 1, M
                 C( I, J ) = ZERO
130             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 140, I = 1, M
                 C( I, J ) = BETA*C( I, J )
140             CONTINUE
           endif
           DO 160, L = 1, K
              IF( B( J, L ).NE.ZERO )THEN
                 TEMP = ALPHA*B( J, L )
                 DO 150, I = 1, M
                    C( I, J ) = C( I, J ) + TEMP*A( I, L )
150                CONTINUE
              endif
160          CONTINUE
170       CONTINUE
     else
!
!           Form  C := alpha*A'*B' + beta*C
!
        DO 200, J = 1, N
           DO 190, I = 1, M
              TEMP = ZERO
              DO 180, L = 1, K
                 TEMP = TEMP + A( L, I )*B( J, L )
180             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP
              else
                 C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
              endif
190          CONTINUE
200       CONTINUE
     endif
  endif
!
  return
!
!     End of DGEMM .
!
  END
  subroutine DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX, &
                     BETA, Y, INCY )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA, BETA
  INTEGER(i4)        INCX, INCY, LDA, M, N
  CHARACTER(len=1)   TRANS
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DGEMV  performs one of the matrix-vector operations
!
!     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters
!  ==========
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - real(dp) array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - real(dp) array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP
  INTEGER(i4)        I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( TRANS, 'N' ).AND. &
           .NOT.LSAME( TRANS, 'T' ).AND. &
           .NOT.LSAME( TRANS, 'C' )      )THEN
     INFO = 1
  else IF( M.LT.0 )THEN
     INFO = 2
  else IF( N.LT.0 )THEN
     INFO = 3
  else IF( LDA.LT.MAX( 1, M ) )THEN
     INFO = 6
  else IF( INCX.EQ.0 )THEN
     INFO = 8
  else IF( INCY.EQ.0 )THEN
     INFO = 11
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DGEMV ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR. &
      ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
  IF( LSAME( TRANS, 'N' ) )THEN
     LENX = N
     LENY = M
  else
     LENX = M
     LENY = N
  endif
  IF( INCX.GT.0 )THEN
     KX = 1
  else
     KX = 1 - ( LENX - 1 )*INCX
  endif
  IF( INCY.GT.0 )THEN
     KY = 1
  else
     KY = 1 - ( LENY - 1 )*INCY
  endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
  IF( BETA.NE.ONE )THEN
     IF( INCY.EQ.1 )THEN
        IF( BETA.EQ.ZERO )THEN
           DO 10, I = 1, LENY
              Y( I ) = ZERO
10          CONTINUE
        else
           DO 20, I = 1, LENY
              Y( I ) = BETA*Y( I )
20          CONTINUE
        endif
     else
        IY = KY
        IF( BETA.EQ.ZERO )THEN
           DO 30, I = 1, LENY
              Y( IY ) = ZERO
              IY      = IY   + INCY
30          CONTINUE
        else
           DO 40, I = 1, LENY
              Y( IY ) = BETA*Y( IY )
              IY      = IY           + INCY
40          CONTINUE
        endif
     endif
  endif
  IF( ALPHA.EQ.ZERO ) &
     return
  IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  y := alpha*A*x + y.
!
     JX = KX
     IF( INCY.EQ.1 )THEN
        DO 60, J = 1, N
           IF( X( JX ).NE.ZERO )THEN
              TEMP = ALPHA*X( JX )
              DO 50, I = 1, M
                 Y( I ) = Y( I ) + TEMP*A( I, J )
50             CONTINUE
           endif
           JX = JX + INCX
60       CONTINUE
     else
        DO 80, J = 1, N
           IF( X( JX ).NE.ZERO )THEN
              TEMP = ALPHA*X( JX )
              IY   = KY
              DO 70, I = 1, M
                 Y( IY ) = Y( IY ) + TEMP*A( I, J )
                 IY      = IY      + INCY
70             CONTINUE
           endif
           JX = JX + INCX
80       CONTINUE
     endif
  else
!
!        Form  y := alpha*A'*x + y.
!
     JY = KY
     IF( INCX.EQ.1 )THEN
        DO 100, J = 1, N
           TEMP = ZERO
           DO 90, I = 1, M
              TEMP = TEMP + A( I, J )*X( I )
90          CONTINUE
           Y( JY ) = Y( JY ) + ALPHA*TEMP
           JY      = JY      + INCY
100       CONTINUE
     else
        DO 120, J = 1, N
           TEMP = ZERO
           IX   = KX
           DO 110, I = 1, M
              TEMP = TEMP + A( I, J )*X( IX )
              IX   = IX   + INCX
110          CONTINUE
           Y( JY ) = Y( JY ) + ALPHA*TEMP
           JY      = JY      + INCY
120       CONTINUE
     endif
  endif
!
  return
!
!     End of DGEMV .
!
  END
  subroutine DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA
  INTEGER(i4)        INCX, INCY, LDA, M, N
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DGER   performs the rank 1 operation
!
!     A := alpha*x*y' + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Parameters
!  ==========
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ZERO
  PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP
  INTEGER(i4)        I, INFO, IX, J, JY, KX
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( M.LT.0 )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( INCX.EQ.0 )THEN
     INFO = 5
  else IF( INCY.EQ.0 )THEN
     INFO = 7
  else IF( LDA.LT.MAX( 1, M ) )THEN
     INFO = 9
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DGER  ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) ) &
     return
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
  IF( INCY.GT.0 )THEN
     JY = 1
  else
     JY = 1 - ( N - 1 )*INCY
  endif
  IF( INCX.EQ.1 )THEN
     DO 20, J = 1, N
        IF( Y( JY ).NE.ZERO )THEN
           TEMP = ALPHA*Y( JY )
           DO 10, I = 1, M
              A( I, J ) = A( I, J ) + X( I )*TEMP
10          CONTINUE
        endif
        JY = JY + INCY
20    CONTINUE
  else
     IF( INCX.GT.0 )THEN
        KX = 1
     else
        KX = 1 - ( M - 1 )*INCX
     endif
     DO 40, J = 1, N
        IF( Y( JY ).NE.ZERO )THEN
           TEMP = ALPHA*Y( JY )
           IX   = KX
           DO 30, I = 1, M
              A( I, J ) = A( I, J ) + X( IX )*TEMP
              IX        = IX        + INCX
30          CONTINUE
        endif
        JY = JY + INCY
40    CONTINUE
  endif
!
  return
!
!     End of DGER  .
!
  END
  subroutine DSPMV ( UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA, BETA
  INTEGER(i4)        INCX, INCY, N
  CHARACTER(len=1)   UPLO
!     .. Array Arguments ..
  real(dp)   AP( * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DSPMV  performs the matrix-vector operation
!
!     y := alpha*A*x + beta*y,
!
!  where alpha and beta are scalars, x and y are n element vectors and
!  A is an n by n symmetric matrix, supplied in packed form.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the matrix A is supplied in the packed
!           array AP as follows:
!
!              UPLO = 'U' or 'u'   The upper triangular part of A is
!                                  supplied in AP.
!
!              UPLO = 'L' or 'l'   The lower triangular part of A is
!                                  supplied in AP.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  AP     - real(dp) array of DIMENSION at least
!           ( ( n*( n + 1 ) )/2 ).
!           Before entry with UPLO = 'U' or 'u', the array AP must
!           contain the upper triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
!           and a( 2, 2 ) respectively, and so on.
!           Before entry with UPLO = 'L' or 'l', the array AP must
!           contain the lower triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
!           and a( 3, 1 ) respectively, and so on.
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y. On exit, Y is overwritten by the updated
!           vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP1, TEMP2
  INTEGER(i4)        I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO, 'U' ).AND. &
           .NOT.LSAME( UPLO, 'L' )      )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( INCX.EQ.0 )THEN
     INFO = 6
  else IF( INCY.EQ.0 )THEN
     INFO = 9
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSPMV ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     Set up the start points in  X  and  Y.
!
  IF( INCX.GT.0 )THEN
     KX = 1
  else
     KX = 1 - ( N - 1 )*INCX
  endif
  IF( INCY.GT.0 )THEN
     KY = 1
  else
     KY = 1 - ( N - 1 )*INCY
  endif
!
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.
!
!     First form  y := beta*y.
!
  IF( BETA.NE.ONE )THEN
     IF( INCY.EQ.1 )THEN
        IF( BETA.EQ.ZERO )THEN
           DO 10, I = 1, N
              Y( I ) = ZERO
10          CONTINUE
        else
           DO 20, I = 1, N
              Y( I ) = BETA*Y( I )
20          CONTINUE
        endif
     else
        IY = KY
        IF( BETA.EQ.ZERO )THEN
           DO 30, I = 1, N
              Y( IY ) = ZERO
              IY      = IY   + INCY
30          CONTINUE
        else
           DO 40, I = 1, N
              Y( IY ) = BETA*Y( IY )
              IY      = IY           + INCY
40          CONTINUE
        endif
     endif
  endif
  IF( ALPHA.EQ.ZERO ) &
     return
  KK = 1
  IF( LSAME( UPLO, 'U' ) )THEN
!
!        Form  y  when AP contains the upper triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 60, J = 1, N
           TEMP1 = ALPHA*X( J )
           TEMP2 = ZERO
           K     = KK
           DO 50, I = 1, J - 1
              Y( I ) = Y( I ) + TEMP1*AP( K )
              TEMP2  = TEMP2  + AP( K )*X( I )
              K      = K      + 1
50          CONTINUE
           Y( J ) = Y( J ) + TEMP1*AP( KK + J - 1 ) + ALPHA*TEMP2
           KK     = KK     + J
60       CONTINUE
     else
        JX = KX
        JY = KY
        DO 80, J = 1, N
           TEMP1 = ALPHA*X( JX )
           TEMP2 = ZERO
           IX    = KX
           IY    = KY
           DO 70, K = KK, KK + J - 2
              Y( IY ) = Y( IY ) + TEMP1*AP( K )
              TEMP2   = TEMP2   + AP( K )*X( IX )
              IX      = IX      + INCX
              IY      = IY      + INCY
70          CONTINUE
           Y( JY ) = Y( JY ) + TEMP1*AP( KK + J - 1 ) + ALPHA*TEMP2
           JX      = JX      + INCX
           JY      = JY      + INCY
           KK      = KK      + J
80       CONTINUE
     endif
  else
!
!        Form  y  when AP contains the lower triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 100, J = 1, N
           TEMP1  = ALPHA*X( J )
           TEMP2  = ZERO
           Y( J ) = Y( J )       + TEMP1*AP( KK )
           K      = KK           + 1
           DO 90, I = J + 1, N
              Y( I ) = Y( I ) + TEMP1*AP( K )
              TEMP2  = TEMP2  + AP( K )*X( I )
              K      = K      + 1
90          CONTINUE
           Y( J ) = Y( J ) + ALPHA*TEMP2
           KK     = KK     + ( N - J + 1 )
100       CONTINUE
     else
        JX = KX
        JY = KY
        DO 120, J = 1, N
           TEMP1   = ALPHA*X( JX )
           TEMP2   = ZERO
           Y( JY ) = Y( JY )       + TEMP1*AP( KK )
           IX      = JX
           IY      = JY
           DO 110, K = KK + 1, KK + N - J
              IX      = IX      + INCX
              IY      = IY      + INCY
              Y( IY ) = Y( IY ) + TEMP1*AP( K )
              TEMP2   = TEMP2   + AP( K )*X( IX )
110          CONTINUE
           Y( JY ) = Y( JY ) + ALPHA*TEMP2
           JX      = JX      + INCX
           JY      = JY      + INCY
           KK      = KK      + ( N - J + 1 )
120       CONTINUE
     endif
  endif
!
  return
!
!     End of DSPMV .
!
  END
  subroutine DSPR  ( UPLO, N, ALPHA, X, INCX, AP )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA
  INTEGER(i4)        INCX, N
  CHARACTER(len=1)   UPLO
!     .. Array Arguments ..
  real(dp)   AP( * ), X( * )
!     ..
!
!  Purpose
!  =======
!
!  DSPR    performs the symmetric rank 1 operation
!
!     A := alpha*x*x' + A,
!
!  where alpha is a real scalar, x is an n element vector and A is an
!  n by n symmetric matrix, supplied in packed form.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the matrix A is supplied in the packed
!           array AP as follows:
!
!              UPLO = 'U' or 'u'   The upper triangular part of A is
!                                  supplied in AP.
!
!              UPLO = 'L' or 'l'   The lower triangular part of A is
!                                  supplied in AP.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  AP     - real(dp) array of DIMENSION at least
!           ( ( n*( n + 1 ) )/2 ).
!           Before entry with  UPLO = 'U' or 'u', the array AP must
!           contain the upper triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
!           and a( 2, 2 ) respectively, and so on. On exit, the array
!           AP is overwritten by the upper triangular part of the
!           updated matrix.
!           Before entry with UPLO = 'L' or 'l', the array AP must
!           contain the lower triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
!           and a( 3, 1 ) respectively, and so on. On exit, the array
!           AP is overwritten by the lower triangular part of the
!           updated matrix.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ZERO
  PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP
  INTEGER(i4)        I, INFO, IX, J, JX, K, KK, KX
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO, 'U' ).AND. &
           .NOT.LSAME( UPLO, 'L' )      )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( INCX.EQ.0 )THEN
     INFO = 5
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSPR  ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) ) &
     return
!
!     Set the start point in X if the increment is not unity.
!
  IF( INCX.LE.0 )THEN
     KX = 1 - ( N - 1 )*INCX
  else IF( INCX.NE.1 )THEN
     KX = 1
  endif
!
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.
!
  KK = 1
  IF( LSAME( UPLO, 'U' ) )THEN
!
!        Form  A  when upper triangle is stored in AP.
!
     IF( INCX.EQ.1 )THEN
        DO 20, J = 1, N
           IF( X( J ).NE.ZERO )THEN
              TEMP = ALPHA*X( J )
              K    = KK
              DO 10, I = 1, J
                 AP( K ) = AP( K ) + X( I )*TEMP
                 K       = K       + 1
10             CONTINUE
           endif
           KK = KK + J
20       CONTINUE
     else
        JX = KX
        DO 40, J = 1, N
           IF( X( JX ).NE.ZERO )THEN
              TEMP = ALPHA*X( JX )
              IX   = KX
              DO 30, K = KK, KK + J - 1
                 AP( K ) = AP( K ) + X( IX )*TEMP
                 IX      = IX      + INCX
30             CONTINUE
           endif
           JX = JX + INCX
           KK = KK + J
40       CONTINUE
     endif
  else
!
!        Form  A  when lower triangle is stored in AP.
!
     IF( INCX.EQ.1 )THEN
        DO 60, J = 1, N
           IF( X( J ).NE.ZERO )THEN
              TEMP = ALPHA*X( J )
              K    = KK
              DO 50, I = J, N
                 AP( K ) = AP( K ) + X( I )*TEMP
                 K       = K       + 1
50             CONTINUE
           endif
           KK = KK + N - J + 1
60       CONTINUE
     else
        JX = KX
        DO 80, J = 1, N
           IF( X( JX ).NE.ZERO )THEN
              TEMP = ALPHA*X( JX )
              IX   = JX
              DO 70, K = KK, KK + N - J
                 AP( K ) = AP( K ) + X( IX )*TEMP
                 IX      = IX      + INCX
70             CONTINUE
           endif
           JX = JX + INCX
           KK = KK + N - J + 1
80       CONTINUE
     endif
  endif
!
  return
!
!     End of DSPR  .
!
  END
  subroutine DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, &
                     B, LDB )
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER(len=1)   SIDE, UPLO, TRANSA, DIAG
  INTEGER(i4)        M, N, LDA, LDB
  real(dp)   ALPHA
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRMM  performs one of the matrix-matrix operations
!
!     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
!
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:
!
!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!
!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - real(dp) array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     .. Local Scalars ..
  LOGICAL            LSIDE, NOUNIT, UPPER
  INTEGER(i4)        I, INFO, J, K, NROWA
  real(dp)   TEMP
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  LSIDE  = LSAME( SIDE  , 'L' )
  IF( LSIDE )THEN
     NROWA = M
  else
     NROWA = N
  endif
  NOUNIT = LSAME( DIAG  , 'N' )
  UPPER  = LSAME( UPLO  , 'U' )
!
  INFO   = 0
  IF(      ( .NOT.LSIDE                ).AND. &
           ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
     INFO = 1
  else IF( ( .NOT.UPPER                ).AND. &
           ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
     INFO = 2
  else IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND. &
           ( .NOT.LSAME( TRANSA, 'T' ) ).AND. &
           ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
     INFO = 3
  else IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND. &
           ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
     INFO = 4
  else IF( M  .LT.0               )THEN
     INFO = 5
  else IF( N  .LT.0               )THEN
     INFO = 6
  else IF( LDA.LT.MAX( 1, NROWA ) )THEN
     INFO = 9
  else IF( LDB.LT.MAX( 1, M     ) )THEN
     INFO = 11
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DTRMM ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( N.EQ.0 ) &
     return
!
!     And when  alpha.eq.zero.
!
  IF( ALPHA.EQ.ZERO )THEN
     DO 20, J = 1, N
        DO 10, I = 1, M
           B( I, J ) = ZERO
10       CONTINUE
20    CONTINUE
     return
  endif
!
!     Start the operations.
!
  IF( LSIDE )THEN
     IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*A*B.
!
        IF( UPPER )THEN
           DO 50, J = 1, N
              DO 40, K = 1, M
                 IF( B( K, J ).NE.ZERO )THEN
                    TEMP = ALPHA*B( K, J )
                    DO 30, I = 1, K - 1
                       B( I, J ) = B( I, J ) + TEMP*A( I, K )
30                   CONTINUE
                    IF( NOUNIT ) &
                       TEMP = TEMP*A( K, K )
                    B( K, J ) = TEMP
                 endif
40             CONTINUE
50          CONTINUE
        else
           DO 80, J = 1, N
              DO 70 K = M, 1, -1
                 IF( B( K, J ).NE.ZERO )THEN
                    TEMP      = ALPHA*B( K, J )
                    B( K, J ) = TEMP
                    IF( NOUNIT ) &
                       B( K, J ) = B( K, J )*A( K, K )
                    DO 60, I = K + 1, M
                       B( I, J ) = B( I, J ) + TEMP*A( I, K )
60                   CONTINUE
                 endif
70             CONTINUE
80          CONTINUE
        endif
     else
!
!           Form  B := alpha*A'*B.
!
        IF( UPPER )THEN
           DO 110, J = 1, N
              DO 100, I = M, 1, -1
                 TEMP = B( I, J )
                 IF( NOUNIT ) &
                    TEMP = TEMP*A( I, I )
                 DO 90, K = 1, I - 1
                    TEMP = TEMP + A( K, I )*B( K, J )
90                CONTINUE
                 B( I, J ) = ALPHA*TEMP
100             CONTINUE
110          CONTINUE
        else
           DO 140, J = 1, N
              DO 130, I = 1, M
                 TEMP = B( I, J )
                 IF( NOUNIT ) &
                    TEMP = TEMP*A( I, I )
                 DO 120, K = I + 1, M
                    TEMP = TEMP + A( K, I )*B( K, J )
120                CONTINUE
                 B( I, J ) = ALPHA*TEMP
130             CONTINUE
140          CONTINUE
        endif
     endif
  else
     IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*B*A.
!
        IF( UPPER )THEN
           DO 180, J = N, 1, -1
              TEMP = ALPHA
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 150, I = 1, M
                 B( I, J ) = TEMP*B( I, J )
150             CONTINUE
              DO 170, K = 1, J - 1
                 IF( A( K, J ).NE.ZERO )THEN
                    TEMP = ALPHA*A( K, J )
                    DO 160, I = 1, M
                       B( I, J ) = B( I, J ) + TEMP*B( I, K )
160                   CONTINUE
                 endif
170             CONTINUE
180          CONTINUE
        else
           DO 220, J = 1, N
              TEMP = ALPHA
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 190, I = 1, M
                 B( I, J ) = TEMP*B( I, J )
190             CONTINUE
              DO 210, K = J + 1, N
                 IF( A( K, J ).NE.ZERO )THEN
                    TEMP = ALPHA*A( K, J )
                    DO 200, I = 1, M
                       B( I, J ) = B( I, J ) + TEMP*B( I, K )
200                   CONTINUE
                 endif
210             CONTINUE
220          CONTINUE
        endif
     else
!
!           Form  B := alpha*B*A'.
!
        IF( UPPER )THEN
           DO 260, K = 1, N
              DO 240, J = 1, K - 1
                 IF( A( J, K ).NE.ZERO )THEN
                    TEMP = ALPHA*A( J, K )
                    DO 230, I = 1, M
                       B( I, J ) = B( I, J ) + TEMP*B( I, K )
230                   CONTINUE
                 endif
240             CONTINUE
              TEMP = ALPHA
              IF( NOUNIT ) &
                 TEMP = TEMP*A( K, K )
              IF( TEMP.NE.ONE )THEN
                 DO 250, I = 1, M
                    B( I, K ) = TEMP*B( I, K )
250                CONTINUE
              endif
260          CONTINUE
        else
           DO 300, K = N, 1, -1
              DO 280, J = K + 1, N
                 IF( A( J, K ).NE.ZERO )THEN
                    TEMP = ALPHA*A( J, K )
                    DO 270, I = 1, M
                       B( I, J ) = B( I, J ) + TEMP*B( I, K )
270                   CONTINUE
                 endif
280             CONTINUE
              TEMP = ALPHA
              IF( NOUNIT ) &
                 TEMP = TEMP*A( K, K )
              IF( TEMP.NE.ONE )THEN
                 DO 290, I = 1, M
                    B( I, K ) = TEMP*B( I, K )
290                CONTINUE
              endif
300          CONTINUE
        endif
     endif
  endif
!
  return
!
!     End of DTRMM .
!
  END
  subroutine DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
!     .. Scalar Arguments ..
  use datatypes
  INTEGER(i4)        INCX, LDA, N
  CHARACTER(len=1)   DIAG, TRANS, UPLO
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), X( * )
!     ..
!
!  Purpose
!  =======
!
!  DTRMV  performs one of the matrix-vector operations
!
!     x := A*x,   or   x := A'*x,
!
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   x := A*x.
!
!              TRANS = 'T' or 't'   x := A'*x.
!
!              TRANS = 'C' or 'c'   x := A'*x.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x. On exit, X is overwritten with the
!           tranformed vector x.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ZERO
  PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP
  INTEGER(i4)        I, INFO, IX, J, JX, KX
  LOGICAL            NOUNIT
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO , 'U' ).AND. &
           .NOT.LSAME( UPLO , 'L' )      )THEN
     INFO = 1
  else IF( .NOT.LSAME( TRANS, 'N' ).AND. &
           .NOT.LSAME( TRANS, 'T' ).AND. &
           .NOT.LSAME( TRANS, 'C' )      )THEN
     INFO = 2
  else IF( .NOT.LSAME( DIAG , 'U' ).AND. &
           .NOT.LSAME( DIAG , 'N' )      )THEN
     INFO = 3
  else IF( N.LT.0 )THEN
     INFO = 4
  else IF( LDA.LT.MAX( 1, N ) )THEN
     INFO = 6
  else IF( INCX.EQ.0 )THEN
     INFO = 8
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DTRMV ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( N.EQ.0 ) &
     return
!
  NOUNIT = LSAME( DIAG, 'N' )
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
  IF( INCX.LE.0 )THEN
     KX = 1 - ( N - 1 )*INCX
  else IF( INCX.NE.1 )THEN
     KX = 1
  endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
  IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  x := A*x.
!
     IF( LSAME( UPLO, 'U' ) )THEN
        IF( INCX.EQ.1 )THEN
           DO 20, J = 1, N
              IF( X( J ).NE.ZERO )THEN
                 TEMP = X( J )
                 DO 10, I = 1, J - 1
                    X( I ) = X( I ) + TEMP*A( I, J )
10                CONTINUE
                 IF( NOUNIT ) &
                    X( J ) = X( J )*A( J, J )
              endif
20          CONTINUE
        else
           JX = KX
           DO 40, J = 1, N
              IF( X( JX ).NE.ZERO )THEN
                 TEMP = X( JX )
                 IX   = KX
                 DO 30, I = 1, J - 1
                    X( IX ) = X( IX ) + TEMP*A( I, J )
                    IX      = IX      + INCX
30                CONTINUE
                 IF( NOUNIT ) &
                    X( JX ) = X( JX )*A( J, J )
              endif
              JX = JX + INCX
40          CONTINUE
        endif
     else
        IF( INCX.EQ.1 )THEN
           DO 60, J = N, 1, -1
              IF( X( J ).NE.ZERO )THEN
                 TEMP = X( J )
                 DO 50, I = N, J + 1, -1
                    X( I ) = X( I ) + TEMP*A( I, J )
50                CONTINUE
                 IF( NOUNIT ) &
                    X( J ) = X( J )*A( J, J )
              endif
60          CONTINUE
        else
           KX = KX + ( N - 1 )*INCX
           JX = KX
           DO 80, J = N, 1, -1
              IF( X( JX ).NE.ZERO )THEN
                 TEMP = X( JX )
                 IX   = KX
                 DO 70, I = N, J + 1, -1
                    X( IX ) = X( IX ) + TEMP*A( I, J )
                    IX      = IX      - INCX
70                CONTINUE
                 IF( NOUNIT ) &
                    X( JX ) = X( JX )*A( J, J )
              endif
              JX = JX - INCX
80          CONTINUE
        endif
     endif
  else
!
!        Form  x := A'*x.
!
     IF( LSAME( UPLO, 'U' ) )THEN
        IF( INCX.EQ.1 )THEN
           DO 100, J = N, 1, -1
              TEMP = X( J )
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 90, I = J - 1, 1, -1
                 TEMP = TEMP + A( I, J )*X( I )
90             CONTINUE
              X( J ) = TEMP
100          CONTINUE
        else
           JX = KX + ( N - 1 )*INCX
           DO 120, J = N, 1, -1
              TEMP = X( JX )
              IX   = JX
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 110, I = J - 1, 1, -1
                 IX   = IX   - INCX
                 TEMP = TEMP + A( I, J )*X( IX )
110             CONTINUE
              X( JX ) = TEMP
              JX      = JX   - INCX
120          CONTINUE
        endif
     else
        IF( INCX.EQ.1 )THEN
           DO 140, J = 1, N
              TEMP = X( J )
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 130, I = J + 1, N
                 TEMP = TEMP + A( I, J )*X( I )
130             CONTINUE
              X( J ) = TEMP
140          CONTINUE
        else
           JX = KX
           DO 160, J = 1, N
              TEMP = X( JX )
              IX   = JX
              IF( NOUNIT ) &
                 TEMP = TEMP*A( J, J )
              DO 150, I = J + 1, N
                 IX   = IX   + INCX
                 TEMP = TEMP + A( I, J )*X( IX )
150             CONTINUE
              X( JX ) = TEMP
              JX      = JX   + INCX
160          CONTINUE
        endif
     endif
  endif
!
  return
!
!     End of DTRMV .
!
  END
  subroutine DTRSM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA, &
                     B, LDB )
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER(len=1)   SIDE, UPLO, TRANSA, DIAG
  INTEGER(i4)        M, N, LDA, LDB
  real(dp)   ALPHA
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), B( LDB, * )
!     ..
!
!  Purpose
!  =======
!
!  DTRSM  solves one of the matrix equations
!
!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
!
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A'.
!
!  The matrix X is overwritten on B.
!
!  Parameters
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:
!
!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
!
!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A'.
!
!              TRANSA = 'C' or 'c'   op( A ) = A'.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - INTEGER.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - real(dp) array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     .. Local Scalars ..
  LOGICAL            LSIDE, NOUNIT, UPPER
  INTEGER(i4)        I, INFO, J, K, NROWA
  real(dp)   TEMP
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  LSIDE  = LSAME( SIDE  , 'L' )
  IF( LSIDE )THEN
     NROWA = M
  else
     NROWA = N
  endif
  NOUNIT = LSAME( DIAG  , 'N' )
  UPPER  = LSAME( UPLO  , 'U' )
!
  INFO   = 0
  IF(      ( .NOT.LSIDE                ).AND. &
           ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
     INFO = 1
  else IF( ( .NOT.UPPER                ).AND. &
           ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
     INFO = 2
  else IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND. &
           ( .NOT.LSAME( TRANSA, 'T' ) ).AND. &
           ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
     INFO = 3
  else IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND. &
           ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
     INFO = 4
  else IF( M  .LT.0               )THEN
     INFO = 5
  else IF( N  .LT.0               )THEN
     INFO = 6
  else IF( LDA.LT.MAX( 1, NROWA ) )THEN
     INFO = 9
  else IF( LDB.LT.MAX( 1, M     ) )THEN
     INFO = 11
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DTRSM ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( N.EQ.0 ) &
     return
!
!     And when  alpha.eq.zero.
!
  IF( ALPHA.EQ.ZERO )THEN
     DO 20, J = 1, N
        DO 10, I = 1, M
           B( I, J ) = ZERO
10       CONTINUE
20    CONTINUE
     return
  endif
!
!     Start the operations.
!
  IF( LSIDE )THEN
     IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*inv( A )*B.
!
        IF( UPPER )THEN
           DO 60, J = 1, N
              IF( ALPHA.NE.ONE )THEN
                 DO 30, I = 1, M
                    B( I, J ) = ALPHA*B( I, J )
30                CONTINUE
              endif
              DO 50, K = M, 1, -1
                 IF( B( K, J ).NE.ZERO )THEN
                    IF( NOUNIT ) &
                       B( K, J ) = B( K, J )/A( K, K )
                    DO 40, I = 1, K - 1
                       B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
40                   CONTINUE
                 endif
50             CONTINUE
60          CONTINUE
        else
           DO 100, J = 1, N
              IF( ALPHA.NE.ONE )THEN
                 DO 70, I = 1, M
                    B( I, J ) = ALPHA*B( I, J )
70                CONTINUE
              endif
              DO 90 K = 1, M
                 IF( B( K, J ).NE.ZERO )THEN
                    IF( NOUNIT ) &
                       B( K, J ) = B( K, J )/A( K, K )
                    DO 80, I = K + 1, M
                       B( I, J ) = B( I, J ) - B( K, J )*A( I, K )
80                   CONTINUE
                 endif
90             CONTINUE
100          CONTINUE
        endif
     else
!
!           Form  B := alpha*inv( A' )*B.
!
        IF( UPPER )THEN
           DO 130, J = 1, N
              DO 120, I = 1, M
                 TEMP = ALPHA*B( I, J )
                 DO 110, K = 1, I - 1
                    TEMP = TEMP - A( K, I )*B( K, J )
110                CONTINUE
                 IF( NOUNIT ) &
                    TEMP = TEMP/A( I, I )
                 B( I, J ) = TEMP
120             CONTINUE
130          CONTINUE
        else
           DO 160, J = 1, N
              DO 150, I = M, 1, -1
                 TEMP = ALPHA*B( I, J )
                 DO 140, K = I + 1, M
                    TEMP = TEMP - A( K, I )*B( K, J )
140                CONTINUE
                 IF( NOUNIT ) &
                    TEMP = TEMP/A( I, I )
                 B( I, J ) = TEMP
150             CONTINUE
160          CONTINUE
        endif
     endif
  else
     IF( LSAME( TRANSA, 'N' ) )THEN
!
!           Form  B := alpha*B*inv( A ).
!
        IF( UPPER )THEN
           DO 210, J = 1, N
              IF( ALPHA.NE.ONE )THEN
                 DO 170, I = 1, M
                    B( I, J ) = ALPHA*B( I, J )
170                CONTINUE
              endif
              DO 190, K = 1, J - 1
                 IF( A( K, J ).NE.ZERO )THEN
                    DO 180, I = 1, M
                       B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
180                   CONTINUE
                 endif
190             CONTINUE
              IF( NOUNIT )THEN
                 TEMP = ONE/A( J, J )
                 DO 200, I = 1, M
                    B( I, J ) = TEMP*B( I, J )
200                CONTINUE
              endif
210          CONTINUE
        else
           DO 260, J = N, 1, -1
              IF( ALPHA.NE.ONE )THEN
                 DO 220, I = 1, M
                    B( I, J ) = ALPHA*B( I, J )
220                CONTINUE
              endif
              DO 240, K = J + 1, N
                 IF( A( K, J ).NE.ZERO )THEN
                    DO 230, I = 1, M
                       B( I, J ) = B( I, J ) - A( K, J )*B( I, K )
230                   CONTINUE
                 endif
240             CONTINUE
              IF( NOUNIT )THEN
                 TEMP = ONE/A( J, J )
                 DO 250, I = 1, M
                   B( I, J ) = TEMP*B( I, J )
250                CONTINUE
              endif
260          CONTINUE
        endif
     else
!
!           Form  B := alpha*B*inv( A' ).
!
        IF( UPPER )THEN
           DO 310, K = N, 1, -1
              IF( NOUNIT )THEN
                 TEMP = ONE/A( K, K )
                 DO 270, I = 1, M
                    B( I, K ) = TEMP*B( I, K )
270                CONTINUE
              endif
              DO 290, J = 1, K - 1
                 IF( A( J, K ).NE.ZERO )THEN
                    TEMP = A( J, K )
                    DO 280, I = 1, M
                       B( I, J ) = B( I, J ) - TEMP*B( I, K )
280                   CONTINUE
                 endif
290             CONTINUE
              IF( ALPHA.NE.ONE )THEN
                 DO 300, I = 1, M
                    B( I, K ) = ALPHA*B( I, K )
300                CONTINUE
              endif
310          CONTINUE
        else
           DO 360, K = 1, N
              IF( NOUNIT )THEN
                 TEMP = ONE/A( K, K )
                 DO 320, I = 1, M
                    B( I, K ) = TEMP*B( I, K )
320                CONTINUE
              endif
              DO 340, J = K + 1, N
                 IF( A( J, K ).NE.ZERO )THEN
                    TEMP = A( J, K )
                    DO 330, I = 1, M
                       B( I, J ) = B( I, J ) - TEMP*B( I, K )
330                   CONTINUE
                 endif
340             CONTINUE
              IF( ALPHA.NE.ONE )THEN
                 DO 350, I = 1, M
                    B( I, K ) = ALPHA*B( I, K )
350                CONTINUE
              endif
360          CONTINUE
        endif
     endif
  endif
!
  return
!
!     End of DTRSM .
!
  END
  subroutine DSYMV ( UPLO, N, ALPHA, A, LDA, X, INCX, &
                     BETA, Y, INCY )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)           ALPHA, BETA
  integer(i4)        INCX, INCY, LDA, N
  character(len=1)   UPLO
!     .. Array Arguments ..
  real(dp)           A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DSYMV  performs the matrix-vector  operation
!
!     y := alpha*A*x + beta*y,
!
!  where alpha and beta are scalars, x and y are n element vectors and
!  A is an n by n symmetric matrix.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the symmetric matrix and the strictly
!           lower triangular part of A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the symmetric matrix and the strictly
!           upper triangular part of A is not referenced.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y. On exit, Y is overwritten by the updated
!           vector y.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)           ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)           TEMP1, TEMP2
  integer(i4)        I, INFO, IX, IY, J, JX, JY, KX, KY
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO, 'U' ).AND. &
           .NOT.LSAME( UPLO, 'L' )      )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( LDA.LT.MAX( 1, N ) )THEN
     INFO = 5
  else IF( INCX.EQ.0 )THEN
     INFO = 7
  else IF( INCY.EQ.0 )THEN
     INFO = 10
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSYMV ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR.( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     Set up the start points in  X  and  Y.
!
  IF( INCX.GT.0 )THEN
     KX = 1
  else
     KX = 1 - ( N - 1 )*INCX
  endif
  IF( INCY.GT.0 )THEN
     KY = 1
  else
     KY = 1 - ( N - 1 )*INCY
  endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.
!
!     First form  y := beta*y.
!
  IF( BETA.NE.ONE )THEN
     IF( INCY.EQ.1 )THEN
        IF( BETA.EQ.ZERO )THEN
           DO 10, I = 1, N
              Y( I ) = ZERO
10          CONTINUE
        else
           DO 20, I = 1, N
              Y( I ) = BETA*Y( I )
20          CONTINUE
        endif
     else
        IY = KY
        IF( BETA.EQ.ZERO )THEN
           DO 30, I = 1, N
              Y( IY ) = ZERO
              IY      = IY   + INCY
30          CONTINUE
        else
           DO 40, I = 1, N
              Y( IY ) = BETA*Y( IY )
              IY      = IY           + INCY
40          CONTINUE
        endif
     endif
  endif
  IF( ALPHA.EQ.ZERO ) &
     return
  IF( LSAME( UPLO, 'U' ) )THEN
!
!        Form  y  when A is stored in upper triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 60, J = 1, N
           TEMP1 = ALPHA*X( J )
           TEMP2 = ZERO
           DO 50, I = 1, J - 1
              Y( I ) = Y( I ) + TEMP1*A( I, J )
              TEMP2  = TEMP2  + A( I, J )*X( I )
50          CONTINUE
           Y( J ) = Y( J ) + TEMP1*A( J, J ) + ALPHA*TEMP2
60       CONTINUE
     else
        JX = KX
        JY = KY
        DO 80, J = 1, N
           TEMP1 = ALPHA*X( JX )
           TEMP2 = ZERO
           IX    = KX
           IY    = KY
           DO 70, I = 1, J - 1
              Y( IY ) = Y( IY ) + TEMP1*A( I, J )
              TEMP2   = TEMP2   + A( I, J )*X( IX )
              IX      = IX      + INCX
              IY      = IY      + INCY
70          CONTINUE
           Y( JY ) = Y( JY ) + TEMP1*A( J, J ) + ALPHA*TEMP2
           JX      = JX      + INCX
           JY      = JY      + INCY
80       CONTINUE
     endif
  else
!
!        Form  y  when A is stored in lower triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 100, J = 1, N
           TEMP1  = ALPHA*X( J )
           TEMP2  = ZERO
           Y( J ) = Y( J )       + TEMP1*A( J, J )
           DO 90, I = J + 1, N
              Y( I ) = Y( I ) + TEMP1*A( I, J )
              TEMP2  = TEMP2  + A( I, J )*X( I )
90          CONTINUE
           Y( J ) = Y( J ) + ALPHA*TEMP2
100       CONTINUE
     else
        JX = KX
        JY = KY
        DO 120, J = 1, N
           TEMP1   = ALPHA*X( JX )
           TEMP2   = ZERO
           Y( JY ) = Y( JY )       + TEMP1*A( J, J )
           IX      = JX
           IY      = JY
           DO 110, I = J + 1, N
              IX      = IX      + INCX
              IY      = IY      + INCY
              Y( IY ) = Y( IY ) + TEMP1*A( I, J )
              TEMP2   = TEMP2   + A( I, J )*X( IX )
110          CONTINUE
           Y( JY ) = Y( JY ) + ALPHA*TEMP2
           JX      = JX      + INCX
           JY      = JY      + INCY
120       CONTINUE
     endif
  endif
!
  return
!
!     End of DSYMV .
!
  END
  FUNCTION DNRM2 ( N, X, INCX )
!     .. Scalar Arguments ..
  use datatypes
  INTEGER(i4)                       INCX, N
  real(dp)                  DNRM2
!     .. Array Arguments ..
  real(dp)                  X( * )
!     ..
!
!  DNRM2 returns the euclidean norm of a vector via the function
!  name, so that
!
!     DNRM2 := sqrt( x'*x )
!
!
!
!  -- This version written on 25-October-1982.
!     Modified on 14-October-1993 to inline the call to DLASSQ.
!     Sven Hammarling, Nag Ltd.
!
!
!     .. Parameters ..
  real(dp)      ONE         , ZERO
  PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     .. Local Scalars ..
  INTEGER(i4)           IX
  real(dp)      ABSXI, NORM, SCALE, SSQ
!     .. Intrinsic Functions ..
  INTRINSIC             ABS, SQRT
!     ..
!     .. Executable Statements ..
  IF( N.LT.1 .OR. INCX.LT.1 )THEN
     NORM  = ZERO
  else IF( N.EQ.1 )THEN
     NORM  = ABS( X( 1 ) )
  else
     SCALE = ZERO
     SSQ   = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
!
     DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
        IF( X( IX ).NE.ZERO )THEN
           ABSXI = ABS( X( IX ) )
           IF( SCALE.LT.ABSXI )THEN
              SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
              SCALE = ABSXI
           else
              SSQ   = SSQ   +     ( ABSXI/SCALE )**2
           endif
        endif
10    CONTINUE
     NORM  = SCALE * SQRT( SSQ )
  endif
!
  DNRM2 = NORM
  return
!
!     End of DNRM2.
!
  END
  subroutine DSPR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, AP )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA
  INTEGER(i4)        INCX, INCY, N
  CHARACTER*1        UPLO
!     .. Array Arguments ..
  real(dp)   AP( * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DSPR2  performs the symmetric rank 2 operation
!
!     A := alpha*x*y' + alpha*y*x' + A,
!
!  where alpha is a scalar, x and y are n element vectors and A is an
!  n by n symmetric matrix, supplied in packed form.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the matrix A is supplied in the packed
!           array AP as follows:
!
!              UPLO = 'U' or 'u'   The upper triangular part of A is
!                                  supplied in AP.
!
!              UPLO = 'L' or 'l'   The lower triangular part of A is
!                                  supplied in AP.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  AP     - real(dp) array of DIMENSION at least
!           ( ( n*( n + 1 ) )/2 ).
!           Before entry with  UPLO = 'U' or 'u', the array AP must
!           contain the upper triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )
!           and a( 2, 2 ) respectively, and so on. On exit, the array
!           AP is overwritten by the upper triangular part of the
!           updated matrix.
!           Before entry with UPLO = 'L' or 'l', the array AP must
!           contain the lower triangular part of the symmetric matrix
!           packed sequentially, column by column, so that AP( 1 )
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )
!           and a( 3, 1 ) respectively, and so on. On exit, the array
!           AP is overwritten by the lower triangular part of the
!           updated matrix.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ZERO
  PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP1, TEMP2
  INTEGER(i4)        I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO, 'U' ).AND. &
           .NOT.LSAME( UPLO, 'L' )      )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( INCX.EQ.0 )THEN
     INFO = 5
  else IF( INCY.EQ.0 )THEN
     INFO = 7
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSPR2 ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) ) &
     return
!
!     Set up the start points in X and Y if the increments are not both
!     unity.
!
  IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
     IF( INCX.GT.0 )THEN
        KX = 1
     else
        KX = 1 - ( N - 1 )*INCX
     endif
     IF( INCY.GT.0 )THEN
        KY = 1
     else
        KY = 1 - ( N - 1 )*INCY
     endif
     JX = KX
     JY = KY
  endif
!
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.
!
  KK = 1
  IF( LSAME( UPLO, 'U' ) )THEN
!
!        Form  A  when upper triangle is stored in AP.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 20, J = 1, N
           IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( J )
              TEMP2 = ALPHA*X( J )
              K     = KK
              DO 10, I = 1, J
                 AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                 K       = K       + 1
10             CONTINUE
           endif
           KK = KK + J
20       CONTINUE
     else
        DO 40, J = 1, N
           IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( JY )
              TEMP2 = ALPHA*X( JX )
              IX    = KX
              IY    = KY
              DO 30, K = KK, KK + J - 1
                 AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                 IX      = IX      + INCX
                 IY      = IY      + INCY
30             CONTINUE
           endif
           JX = JX + INCX
           JY = JY + INCY
           KK = KK + J
40       CONTINUE
     endif
  else
!
!        Form  A  when lower triangle is stored in AP.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 60, J = 1, N
           IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( J )
              TEMP2 = ALPHA*X( J )
              K     = KK
              DO 50, I = J, N
                 AP( K ) = AP( K ) + X( I )*TEMP1 + Y( I )*TEMP2
                 K       = K       + 1
50             CONTINUE
           endif
           KK = KK + N - J + 1
60       CONTINUE
     else
        DO 80, J = 1, N
           IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( JY )
              TEMP2 = ALPHA*X( JX )
              IX    = JX
              IY    = JY
              DO 70, K = KK, KK + N - J
                 AP( K ) = AP( K ) + X( IX )*TEMP1 + Y( IY )*TEMP2
                 IX      = IX      + INCX
                 IY      = IY      + INCY
70             CONTINUE
           endif
           JX = JX + INCX
           JY = JY + INCY
           KK = KK + N - J + 1
80       CONTINUE
     endif
  endif
!
  return
!
!     End of DSPR2 .
!
  END
  subroutine DSYR2 ( UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA )
!     .. Scalar Arguments ..
  use datatypes
  real(dp)   ALPHA
  INTEGER(i4)        INCX, INCY, LDA, N
  CHARACTER*1        UPLO
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), X( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DSYR2  performs the symmetric rank 2 operation
!
!     A := alpha*x*y' + alpha*y*x' + A,
!
!  where alpha is a scalar, x and y are n element vectors and A is an n
!  by n symmetric matrix.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - INTEGER.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - real(dp) array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - INTEGER.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the symmetric matrix and the strictly
!           lower triangular part of A is not referenced. On exit, the
!           upper triangular part of the array A is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the symmetric matrix and the strictly
!           upper triangular part of A is not referenced. On exit, the
!           lower triangular part of the array A is overwritten by the
!           lower triangular part of the updated matrix.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!
!     .. Parameters ..
  real(dp)   ZERO
  PARAMETER        ( ZERO = 0.0D+0 )
!     .. Local Scalars ..
  real(dp)   TEMP1, TEMP2
  INTEGER(i4)        I, INFO, IX, IY, J, JX, JY, KX, KY
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  INFO = 0
  IF     ( .NOT.LSAME( UPLO, 'U' ).AND. &
           .NOT.LSAME( UPLO, 'L' )      )THEN
     INFO = 1
  else IF( N.LT.0 )THEN
     INFO = 2
  else IF( INCX.EQ.0 )THEN
     INFO = 5
  else IF( INCY.EQ.0 )THEN
     INFO = 7
  else IF( LDA.LT.MAX( 1, N ) )THEN
     INFO = 9
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSYR2 ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) ) &
     return
!
!     Set up the start points in X and Y if the increments are not both
!     unity.
!
  IF( ( INCX.NE.1 ).OR.( INCY.NE.1 ) )THEN
     IF( INCX.GT.0 )THEN
        KX = 1
     else
        KX = 1 - ( N - 1 )*INCX
     endif
     IF( INCY.GT.0 )THEN
        KY = 1
     else
        KY = 1 - ( N - 1 )*INCY
     endif
     JX = KX
     JY = KY
  endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.
!
  IF( LSAME( UPLO, 'U' ) )THEN
!
!        Form  A  when A is stored in the upper triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 20, J = 1, N
           IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( J )
              TEMP2 = ALPHA*X( J )
              DO 10, I = 1, J
                 A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
10             CONTINUE
           endif
20       CONTINUE
     else
        DO 40, J = 1, N
           IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( JY )
              TEMP2 = ALPHA*X( JX )
              IX    = KX
              IY    = KY
              DO 30, I = 1, J
                 A( I, J ) = A( I, J ) + X( IX )*TEMP1 &
                                       + Y( IY )*TEMP2
                 IX        = IX        + INCX
                 IY        = IY        + INCY
30             CONTINUE
           endif
           JX = JX + INCX
           JY = JY + INCY
40       CONTINUE
     endif
  else
!
!        Form  A  when A is stored in the lower triangle.
!
     IF( ( INCX.EQ.1 ).AND.( INCY.EQ.1 ) )THEN
        DO 60, J = 1, N
           IF( ( X( J ).NE.ZERO ).OR.( Y( J ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( J )
              TEMP2 = ALPHA*X( J )
              DO 50, I = J, N
                 A( I, J ) = A( I, J ) + X( I )*TEMP1 + Y( I )*TEMP2
50             CONTINUE
           endif
60       CONTINUE
     else
        DO 80, J = 1, N
           IF( ( X( JX ).NE.ZERO ).OR.( Y( JY ).NE.ZERO ) )THEN
              TEMP1 = ALPHA*Y( JY )
              TEMP2 = ALPHA*X( JX )
              IX    = JX
              IY    = JY
              DO 70, I = J, N
                 A( I, J ) = A( I, J ) + X( IX )*TEMP1 &
                                       + Y( IY )*TEMP2
                 IX        = IX        + INCX
                 IY        = IY        + INCY
70             CONTINUE
           endif
           JX = JX + INCX
           JY = JY + INCY
80       CONTINUE
     endif
  endif
!
  return
!
!     End of DSYR2 .
!
  END
  subroutine DSYR2K( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB, &
                     BETA, C, LDC )
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER*1        UPLO, TRANS
  INTEGER(i4)        N, K, LDA, LDB, LDC
  real(dp)   ALPHA, BETA
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), B( LDB, * ), C( LDC, * )
!     ..
!
!  Purpose
!  =======
!
!  DSYR2K  performs one of the symmetric rank 2k operations
!
!     C := alpha*A*B' + alpha*B*A' + beta*C,
!
!  or
!
!     C := alpha*A'*B + alpha*B'*A + beta*C,
!
!  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
!  matrices in the second case.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
!                                        beta*C.
!
!              TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
!                                        beta*C.
!
!              TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
!                                        beta*C.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!           of rows of the matrices  A and B.  K must be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - real(dp) array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - INTEGER.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - real(dp) array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  symmetric matrix  and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  symmetric matrix  and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     .. Local Scalars ..
  LOGICAL            UPPER
  INTEGER(i4)        I, INFO, J, L, NROWA
  real(dp)   TEMP1, TEMP2
!     .. Parameters ..
  real(dp)   ONE         , ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  IF( LSAME( TRANS, 'N' ) )THEN
     NROWA = N
  else
     NROWA = K
  endif
  UPPER = LSAME( UPLO, 'U' )
!
  INFO = 0
  IF(      ( .NOT.UPPER               ).AND. &
           ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
     INFO = 1
  else IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND. &
           ( .NOT.LSAME( TRANS, 'T' ) ).AND. &
           ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
     INFO = 2
  else IF( N  .LT.0               )THEN
     INFO = 3
  else IF( K  .LT.0               )THEN
     INFO = 4
  else IF( LDA.LT.MAX( 1, NROWA ) )THEN
     INFO = 7
  else IF( LDB.LT.MAX( 1, NROWA ) )THEN
     INFO = 9
  else IF( LDC.LT.MAX( 1, N     ) )THEN
     INFO = 12
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSYR2K', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR. &
      ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     And when  alpha.eq.zero.
!
  IF( ALPHA.EQ.ZERO )THEN
     IF( UPPER )THEN
        IF( BETA.EQ.ZERO )THEN
           DO 20, J = 1, N
              DO 10, I = 1, J
                 C( I, J ) = ZERO
10             CONTINUE
20          CONTINUE
        else
           DO 40, J = 1, N
              DO 30, I = 1, J
                 C( I, J ) = BETA*C( I, J )
30             CONTINUE
40          CONTINUE
        endif
     else
        IF( BETA.EQ.ZERO )THEN
           DO 60, J = 1, N
              DO 50, I = J, N
                 C( I, J ) = ZERO
50             CONTINUE
60          CONTINUE
        else
           DO 80, J = 1, N
              DO 70, I = J, N
                 C( I, J ) = BETA*C( I, J )
70             CONTINUE
80          CONTINUE
        endif
     endif
     return
  endif
!
!     Start the operations.
!
  IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  C := alpha*A*B' + alpha*B*A' + C.
!
     IF( UPPER )THEN
        DO 130, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 90, I = 1, J
                 C( I, J ) = ZERO
90             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 100, I = 1, J
                 C( I, J ) = BETA*C( I, J )
100             CONTINUE
           endif
           DO 120, L = 1, K
              IF( ( A( J, L ).NE.ZERO ).OR. &
                  ( B( J, L ).NE.ZERO )     )THEN
                 TEMP1 = ALPHA*B( J, L )
                 TEMP2 = ALPHA*A( J, L )
                 DO 110, I = 1, J
                    C( I, J ) = C( I, J ) + &
                                A( I, L )*TEMP1 + B( I, L )*TEMP2
110                CONTINUE
              endif
120          CONTINUE
130       CONTINUE
     else
        DO 180, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 140, I = J, N
                 C( I, J ) = ZERO
140             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 150, I = J, N
                 C( I, J ) = BETA*C( I, J )
150             CONTINUE
           endif
           DO 170, L = 1, K
              IF( ( A( J, L ).NE.ZERO ).OR. &
                  ( B( J, L ).NE.ZERO )     )THEN
                 TEMP1 = ALPHA*B( J, L )
                 TEMP2 = ALPHA*A( J, L )
                 DO 160, I = J, N
                    C( I, J ) = C( I, J ) + &
                                A( I, L )*TEMP1 + B( I, L )*TEMP2
160                CONTINUE
              endif
170          CONTINUE
180       CONTINUE
     endif
  else
!
!        Form  C := alpha*A'*B + alpha*B'*A + C.
!
     IF( UPPER )THEN
        DO 210, J = 1, N
           DO 200, I = 1, J
              TEMP1 = ZERO
              TEMP2 = ZERO
              DO 190, L = 1, K
                 TEMP1 = TEMP1 + A( L, I )*B( L, J )
                 TEMP2 = TEMP2 + B( L, I )*A( L, J )
190             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP1 + ALPHA*TEMP2
              else
                 C( I, J ) = BETA *C( I, J ) + &
                             ALPHA*TEMP1 + ALPHA*TEMP2
              endif
200          CONTINUE
210       CONTINUE
     else
        DO 240, J = 1, N
           DO 230, I = J, N
              TEMP1 = ZERO
              TEMP2 = ZERO
              DO 220, L = 1, K
                 TEMP1 = TEMP1 + A( L, I )*B( L, J )
                 TEMP2 = TEMP2 + B( L, I )*A( L, J )
220             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP1 + ALPHA*TEMP2
              else
                 C( I, J ) = BETA *C( I, J ) + &
                             ALPHA*TEMP1 + ALPHA*TEMP2
              endif
230          CONTINUE
240       CONTINUE
     endif
  endif
!
  return
!
!     End of DSYR2K.
!
  END
  subroutine DSYRK ( UPLO, TRANS, N, K, ALPHA, A, LDA, &
                     BETA, C, LDC )
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER*1        UPLO, TRANS
  INTEGER(i4)        N, K, LDA, LDC
  real(dp)   ALPHA, BETA
!     .. Array Arguments ..
  real(dp)   A( LDA, * ), C( LDC, * )
!     ..
!
!  Purpose
!  =======
!
!  DSYRK  performs one of the symmetric rank k operations
!
!     C := alpha*A*A' + beta*C,
!
!  or
!
!     C := alpha*A'*A + beta*C,
!
!  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
!  in the second case.
!
!  Parameters
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
!
!              TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
!
!              TRANS = 'C' or 'c'   C := alpha*A'*A + beta*C.
!
!           Unchanged on exit.
!
!  N      - INTEGER.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - INTEGER.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns   of  the   matrix   A,   and  on   entry   with
!           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!           of rows of the matrix  A.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - real(dp).
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real(dp) array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - INTEGER.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - real(dp).
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - real(dp) array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  symmetric matrix  and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  symmetric matrix  and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!
!  LDC    - INTEGER.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!
!     .. External Functions ..
  LOGICAL            LSAME
  EXTERNAL           LSAME
!     .. External Subroutines ..
  EXTERNAL           XERBLA
!     .. Intrinsic Functions ..
  INTRINSIC          MAX
!     .. Local Scalars ..
  LOGICAL            UPPER
  INTEGER(i4)        I, INFO, J, L, NROWA
  real(dp)   TEMP
!     .. Parameters ..
  real(dp)   ONE ,         ZERO
  PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
  IF( LSAME( TRANS, 'N' ) )THEN
     NROWA = N
  else
     NROWA = K
  endif
  UPPER = LSAME( UPLO, 'U' )
!
  INFO = 0
  IF(      ( .NOT.UPPER               ).AND. &
           ( .NOT.LSAME( UPLO , 'L' ) )      )THEN
     INFO = 1
  else IF( ( .NOT.LSAME( TRANS, 'N' ) ).AND. &
           ( .NOT.LSAME( TRANS, 'T' ) ).AND. &
           ( .NOT.LSAME( TRANS, 'C' ) )      )THEN
     INFO = 2
  else IF( N  .LT.0               )THEN
     INFO = 3
  else IF( K  .LT.0               )THEN
     INFO = 4
  else IF( LDA.LT.MAX( 1, NROWA ) )THEN
     INFO = 7
  else IF( LDC.LT.MAX( 1, N     ) )THEN
     INFO = 10
  endif
  IF( INFO.NE.0 )THEN
     CALL XERBLA( 'DSYRK ', INFO )
     return
  endif
!
!     Quick return if possible.
!
  IF( ( N.EQ.0 ).OR. &
      ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) ) &
     return
!
!     And when  alpha.eq.zero.
!
  IF( ALPHA.EQ.ZERO )THEN
     IF( UPPER )THEN
        IF( BETA.EQ.ZERO )THEN
           DO 20, J = 1, N
              DO 10, I = 1, J
                 C( I, J ) = ZERO
10             CONTINUE
20          CONTINUE
        else
           DO 40, J = 1, N
              DO 30, I = 1, J
                 C( I, J ) = BETA*C( I, J )
30             CONTINUE
40          CONTINUE
        endif
     else
        IF( BETA.EQ.ZERO )THEN
           DO 60, J = 1, N
              DO 50, I = J, N
                 C( I, J ) = ZERO
50             CONTINUE
60          CONTINUE
        else
           DO 80, J = 1, N
              DO 70, I = J, N
                 C( I, J ) = BETA*C( I, J )
70             CONTINUE
80          CONTINUE
        endif
     endif
     return
  endif
!
!     Start the operations.
!
  IF( LSAME( TRANS, 'N' ) )THEN
!
!        Form  C := alpha*A*A' + beta*C.
!
     IF( UPPER )THEN
        DO 130, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 90, I = 1, J
                 C( I, J ) = ZERO
90             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 100, I = 1, J
                 C( I, J ) = BETA*C( I, J )
100             CONTINUE
           endif
           DO 120, L = 1, K
              IF( A( J, L ).NE.ZERO )THEN
                 TEMP = ALPHA*A( J, L )
                 DO 110, I = 1, J
                    C( I, J ) = C( I, J ) + TEMP*A( I, L )
110                CONTINUE
              endif
120          CONTINUE
130       CONTINUE
     else
        DO 180, J = 1, N
           IF( BETA.EQ.ZERO )THEN
              DO 140, I = J, N
                 C( I, J ) = ZERO
140             CONTINUE
           else IF( BETA.NE.ONE )THEN
              DO 150, I = J, N
                 C( I, J ) = BETA*C( I, J )
150             CONTINUE
           endif
           DO 170, L = 1, K
              IF( A( J, L ).NE.ZERO )THEN
                 TEMP      = ALPHA*A( J, L )
                 DO 160, I = J, N
                    C( I, J ) = C( I, J ) + TEMP*A( I, L )
160                CONTINUE
              endif
170          CONTINUE
180       CONTINUE
     endif
  else
!
!        Form  C := alpha*A'*A + beta*C.
!
     IF( UPPER )THEN
        DO 210, J = 1, N
           DO 200, I = 1, J
              TEMP = ZERO
              DO 190, L = 1, K
                 TEMP = TEMP + A( L, I )*A( L, J )
190             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP
              else
                 C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
              endif
200          CONTINUE
210       CONTINUE
     else
        DO 240, J = 1, N
           DO 230, I = J, N
              TEMP = ZERO
              DO 220, L = 1, K
                 TEMP = TEMP + A( L, I )*A( L, J )
220             CONTINUE
              IF( BETA.EQ.ZERO )THEN
                 C( I, J ) = ALPHA*TEMP
              else
                 C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
              endif
230          CONTINUE
240       CONTINUE
     endif
  endif
!
  return
!
!     End of DSYRK .
!
  END
  LOGICAL          FUNCTION LSAME( CA, CB )
  use datatypes
!
!  -- LAPACK auxiliary routine (version 2.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     January 31, 1994
!
!     .. Scalar Arguments ..
  CHARACTER          CA, CB
!     ..
!
!  Purpose
!  =======
!
!  LSAME returns .TRUE. if CA is the same letter as CB regardless of
!  case.
!
!  Arguments
!  =========
!
!  CA      (input) CHARACTER*1
!  CB      (input) CHARACTER*1
!          CA and CB specify the single characters to be compared.
!
! =====================================================================
!
!     .. Intrinsic Functions ..
  INTRINSIC          ICHAR
!     ..
!     .. Local Scalars ..
  INTEGER(i4)        INTA, INTB, ZCODE
!     ..
!     .. Executable Statements ..
!
!     Test if the characters are equal
!
  LSAME = CA.EQ.CB
  IF( LSAME ) &
     return
!
!     Now test for equivalence if both characters are alphabetic.
!
  ZCODE = ICHAR( 'Z' )
!
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
!     machines, on which ICHAR returns a value with bit 8 set.
!     ICHAR('A') on Prime machines returns 193 which is the same as
!     ICHAR('A') on an EBCDIC machine.
!
  INTA = ICHAR( CA )
  INTB = ICHAR( CB )
!
  IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
!
!        ASCII is assumed - ZCODE is the ASCII code of either lower or
!        upper case 'Z'.
!
     IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
     IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
!
  else IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
!
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.
!
     IF( INTA.GE.129 .AND. INTA.LE.137 .OR. &
         INTA.GE.145 .AND. INTA.LE.153 .OR. &
         INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
     IF( INTB.GE.129 .AND. INTB.LE.137 .OR. &
         INTB.GE.145 .AND. INTB.LE.153 .OR. &
         INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
!
  else IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
!
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
!        plus 128 of either lower or upper case 'Z'.
!
     IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
     IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
  endif
  LSAME = INTA.EQ.INTB
!
!     return
!
!     End of LSAME
!
  END
  subroutine XERBLA( SRNAME, INFO )
!
!  -- LAPACK auxiliary routine (preliminary version) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     February 29, 1992
!
!     .. Scalar Arguments ..
  use datatypes
  CHARACTER(len=6)   SRNAME
  INTEGER(i4)        INFO
!     ..
!
!  Purpose
!  =======
!
!  XERBLA  is an error handler for the LAPACK routines.
!  It is called by an LAPACK routine if an input parameter has an
!  invalid value.  A message is printed and execution stops.
!
!  Installers may consider modifying the STOP statement in order to
!  call system-specific exception-handling facilities.
!
!  Arguments
!  =========
!
!  SRNAME  (input) CHARACTER*6
!          The name of the routine which called XERBLA.
!
!  INFO    (input) INTEGER
!          The position of the invalid parameter in the parameter list
!          of the calling routine.
!
!
  WRITE( *, FMT = 9999 )SRNAME, INFO
!
  STOP
!
9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ', &
        'an illegal value' )
!
!     End of XERBLA
!
  END
      subroutine ZGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA,BETA
      integer(i4)  :: K,LDA,LDB,LDC,M,N
      CHARACTER TRANSA,TRANSB
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),B(LDB,*),C(LDC,*)
!     ..
!
!  Purpose
!  =======
!
!  ZGEMM  performs one of the matrix-matrix operations
!
!     C := alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
!
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Arguments
!  ==========
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n',  op( A ) = A.
!
!              TRANSA = 'T' or 't',  op( A ) = A**T.
!
!              TRANSA = 'C' or 'c',  op( A ) = A**H.
!
!           Unchanged on exit.
!
!  TRANSB - CHARACTER*1.
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSB = 'N' or 'n',  op( B ) = B.
!
!              TRANSB = 'T' or 't',  op( B ) = B**T.
!
!              TRANSB = 'C' or 'c',  op( B ) = B**H.
!
!           Unchanged on exit.
!
!  M      - integer(i4)  ::.
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer(i4)  ::.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - integer(i4)  ::.
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  BETA   - COMPLEX*16      .
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  C      - COMPLEX*16       array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - integer(i4)  ::.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!  =====================================================================
!
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP
      integer(i4)  :: I,INFO,J,L,NCOLA,NROWA,NROWB
      LOGICAL CONJA,CONJB,NOTA,NOTB
!     ..
!     .. Parameters ..
      complex(dpc) :: ONE
      PARAMETER (ONE= (1.0D+0,0.0D+0))
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     conjugated or transposed, set  CONJA and CONJB  as true if  A  and
!     B  respectively are to be  transposed but  not conjugated  and set
!     NROWA, NCOLA and  NROWB  as the number of rows and  columns  of  A
!     and the number of rows of  B  respectively.
!
      NOTA = LSAME(TRANSA,'N')
      NOTB = LSAME(TRANSB,'N')
      CONJA = LSAME(TRANSA,'C')
      CONJB = LSAME(TRANSB,'C')
      IF (NOTA) THEN
          NROWA = M
          NCOLA = K
      else
          NROWA = K
          NCOLA = M
      endif
      IF (NOTB) THEN
          NROWB = K
      else
          NROWB = N
      endif
!
!     Test the input parameters.
!
      INFO = 0
      IF ((.NOT.NOTA) .AND. (.NOT.CONJA) .AND. (.NOT.LSAME(TRANSA,'T'))) THEN
          INFO = 1
      else IF ((.NOT.NOTB) .AND. (.NOT.CONJB) .AND. (.NOT.LSAME(TRANSB,'T'))) THEN
          INFO = 2
      else IF (M.LT.0) THEN
          INFO = 3
      else IF (N.LT.0) THEN
          INFO = 4
      else IF (K.LT.0) THEN
          INFO = 5
      else IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 8
      else IF (LDB.LT.MAX(1,NROWB)) THEN
          INFO = 10
      else IF (LDC.LT.MAX(1,M)) THEN
          INFO = 13
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZGEMM ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (((ALPHA.EQ.ZERO).OR. (K.EQ.0)).AND. (BETA.EQ.ONE))) return
!
!     And when  alpha.eq.zero.
!
      IF (ALPHA.EQ.ZERO) THEN
          IF (BETA.EQ.ZERO) THEN
              DO 20 J = 1,N
                  DO 10 I = 1,M
                      C(I,J) = ZERO
   10             CONTINUE
   20         CONTINUE
          else
              DO 40 J = 1,N
                  DO 30 I = 1,M
                      C(I,J) = BETA*C(I,J)
   30             CONTINUE
   40         CONTINUE
          endif
          return
      endif
!
!     Start the operations.
!
      IF (NOTB) THEN
          IF (NOTA) THEN
!
!           Form  C := alpha*A*B + beta*C.
!
              DO 90 J = 1,N
                  IF (BETA.EQ.ZERO) THEN
                      DO 50 I = 1,M
                          C(I,J) = ZERO
   50                 CONTINUE
                  else IF (BETA.NE.ONE) THEN
                      DO 60 I = 1,M
                          C(I,J) = BETA*C(I,J)
   60                 CONTINUE
                  endif
                  DO 80 L = 1,K
                      IF (B(L,J).NE.ZERO) THEN
                          TEMP = ALPHA*B(L,J)
                          DO 70 I = 1,M
                              C(I,J) = C(I,J) + TEMP*A(I,L)
   70                     CONTINUE
                      endif
   80             CONTINUE
   90         CONTINUE
          else IF (CONJA) THEN
!
!           Form  C := alpha*A**H*B + beta*C.
!
              DO 120 J = 1,N
                  DO 110 I = 1,M
                      TEMP = ZERO
                      DO 100 L = 1,K
                          TEMP = TEMP + DCONJG(A(L,I))*B(L,J)
  100                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  110             CONTINUE
  120         CONTINUE
          else
!
!           Form  C := alpha*A**T*B + beta*C
!
              DO 150 J = 1,N
                  DO 140 I = 1,M
                      TEMP = ZERO
                      DO 130 L = 1,K
                          TEMP = TEMP + A(L,I)*B(L,J)
  130                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  140             CONTINUE
  150         CONTINUE
          endif
      else IF (NOTA) THEN
          IF (CONJB) THEN
!
!           Form  C := alpha*A*B**H + beta*C.
!
              DO 200 J = 1,N
                  IF (BETA.EQ.ZERO) THEN
                      DO 160 I = 1,M
                          C(I,J) = ZERO
  160                 CONTINUE
                  else IF (BETA.NE.ONE) THEN
                      DO 170 I = 1,M
                          C(I,J) = BETA*C(I,J)
  170                 CONTINUE
                  endif
                  DO 190 L = 1,K
                      IF (B(J,L).NE.ZERO) THEN
                          TEMP = ALPHA*DCONJG(B(J,L))
                          DO 180 I = 1,M
                              C(I,J) = C(I,J) + TEMP*A(I,L)
  180                     CONTINUE
                      endif
  190             CONTINUE
  200         CONTINUE
          else
!
!           Form  C := alpha*A*B**T          + beta*C
!
              DO 250 J = 1,N
                  IF (BETA.EQ.ZERO) THEN
                      DO 210 I = 1,M
                          C(I,J) = ZERO
  210                 CONTINUE
                  else IF (BETA.NE.ONE) THEN
                      DO 220 I = 1,M
                          C(I,J) = BETA*C(I,J)
  220                 CONTINUE
                  endif
                  DO 240 L = 1,K
                      IF (B(J,L).NE.ZERO) THEN
                          TEMP = ALPHA*B(J,L)
                          DO 230 I = 1,M
                              C(I,J) = C(I,J) + TEMP*A(I,L)
  230                     CONTINUE
                      endif
  240             CONTINUE
  250         CONTINUE
          endif
      else IF (CONJA) THEN
          IF (CONJB) THEN
!
!           Form  C := alpha*A**H*B**H + beta*C.
!
              DO 280 J = 1,N
                  DO 270 I = 1,M
                      TEMP = ZERO
                      DO 260 L = 1,K
                          TEMP = TEMP + DCONJG(A(L,I))*DCONJG(B(J,L))
  260                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  270             CONTINUE
  280         CONTINUE
          else
!
!           Form  C := alpha*A**H*B**T + beta*C
!
              DO 310 J = 1,N
                  DO 300 I = 1,M
                      TEMP = ZERO
                      DO 290 L = 1,K
                          TEMP = TEMP + DCONJG(A(L,I))*B(J,L)
  290                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  300             CONTINUE
  310         CONTINUE
          endif
      else
          IF (CONJB) THEN
!
!           Form  C := alpha*A**T*B**H + beta*C
!
              DO 340 J = 1,N
                  DO 330 I = 1,M
                      TEMP = ZERO
                      DO 320 L = 1,K
                          TEMP = TEMP + A(L,I)*DCONJG(B(J,L))
  320                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  330             CONTINUE
  340         CONTINUE
          else
!
!           Form  C := alpha*A**T*B**T + beta*C
!
              DO 370 J = 1,N
                  DO 360 I = 1,M
                      TEMP = ZERO
                      DO 350 L = 1,K
                          TEMP = TEMP + A(L,I)*B(J,L)
  350                 CONTINUE
                      IF (BETA.EQ.ZERO) THEN
                          C(I,J) = ALPHA*TEMP
                      else
                          C(I,J) = ALPHA*TEMP + BETA*C(I,J)
                      endif
  360             CONTINUE
  370         CONTINUE
          endif
      endif
!
      return
!
!     End of ZGEMM .
!
      END
      subroutine ZGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA,BETA
      integer(i4)  :: INCX,INCY,LDA,M,N
      CHARACTER TRANS
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),X(*),Y(*)
!     ..
!
!  Purpose
!  =======
!
!  ZGEMV  performs one of the matrix-vector operations
!
!     y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
!
!     y := alpha*A**H*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Arguments
!  ==========
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.
!
!              TRANS = 'C' or 'c'   y := alpha*A**H*x + beta*y.
!
!           Unchanged on exit.
!
!  M      - integer(i4)  ::.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - COMPLEX*16       array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  INCX   - integer(i4)  ::.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - COMPLEX*16      .
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - COMPLEX*16       array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
!           Before entry with BETA non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  INCY   - integer(i4)  ::.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 2 Blas routine.
!  The vector and matrix arguments are not referenced when N = 0, or M = 0
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!  =====================================================================
!
!     .. Parameters ..
      complex(dpc) :: ONE
      PARAMETER (ONE= (1.0D+0,0.0D+0))
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP
      integer(i4)  :: I,INFO,IX,IY,J,JX,JY,KX,KY,LENX,LENY
      LOGICAL NOCONJ
!     ..
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!     ..
!
!     Test the input parameters.
!
      INFO = 0
      IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND. .NOT.LSAME(TRANS,'C')) THEN
          INFO = 1
      else IF (M.LT.0) THEN
          INFO = 2
      else IF (N.LT.0) THEN
          INFO = 3
      else IF (LDA.LT.MAX(1,M)) THEN
          INFO = 6
      else IF (INCX.EQ.0) THEN
          INFO = 8
      else IF (INCY.EQ.0) THEN
          INFO = 11
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZGEMV ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) return
!
      NOCONJ = LSAME(TRANS,'T')
!
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
!     up the start points in  X  and  Y.
!
      IF (LSAME(TRANS,'N')) THEN
          LENX = N
          LENY = M
      else
          LENX = M
          LENY = N
      endif
      IF (INCX.GT.0) THEN
          KX = 1
      else
          KX = 1 - (LENX-1)*INCX
      endif
      IF (INCY.GT.0) THEN
          KY = 1
      else
          KY = 1 - (LENY-1)*INCY
      endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
!     First form  y := beta*y.
!
      IF (BETA.NE.ONE) THEN
          IF (INCY.EQ.1) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 10 I = 1,LENY
                      Y(I) = ZERO
   10             CONTINUE
              else
                  DO 20 I = 1,LENY
                      Y(I) = BETA*Y(I)
   20             CONTINUE
              endif
          else
              IY = KY
              IF (BETA.EQ.ZERO) THEN
                  DO 30 I = 1,LENY
                      Y(IY) = ZERO
                      IY = IY + INCY
   30             CONTINUE
              else
                  DO 40 I = 1,LENY
                      Y(IY) = BETA*Y(IY)
                      IY = IY + INCY
   40             CONTINUE
              endif
          endif
      endif
      IF (ALPHA.EQ.ZERO) return
      IF (LSAME(TRANS,'N')) THEN
!
!        Form  y := alpha*A*x + y.
!
          JX = KX
          IF (INCY.EQ.1) THEN
              DO 60 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      DO 50 I = 1,M
                          Y(I) = Y(I) + TEMP*A(I,J)
   50                 CONTINUE
                  endif
                  JX = JX + INCX
   60         CONTINUE
          else
              DO 80 J = 1,N
                  IF (X(JX).NE.ZERO) THEN
                      TEMP = ALPHA*X(JX)
                      IY = KY
                      DO 70 I = 1,M
                          Y(IY) = Y(IY) + TEMP*A(I,J)
                          IY = IY + INCY
   70                 CONTINUE
                  endif
                  JX = JX + INCX
   80         CONTINUE
          endif
      else
!
!        Form  y := alpha*A**T*x + y  or  y := alpha*A**H*x + y.
!
          JY = KY
          IF (INCX.EQ.1) THEN
              DO 110 J = 1,N
                  TEMP = ZERO
                  IF (NOCONJ) THEN
                      DO 90 I = 1,M
                          TEMP = TEMP + A(I,J)*X(I)
   90                 CONTINUE
                  else
                      DO 100 I = 1,M
                          TEMP = TEMP + DCONJG(A(I,J))*X(I)
  100                 CONTINUE
                  endif
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  110         CONTINUE
          else
              DO 140 J = 1,N
                  TEMP = ZERO
                  IX = KX
                  IF (NOCONJ) THEN
                      DO 120 I = 1,M
                          TEMP = TEMP + A(I,J)*X(IX)
                          IX = IX + INCX
  120                 CONTINUE
                  else
                      DO 130 I = 1,M
                          TEMP = TEMP + DCONJG(A(I,J))*X(IX)
                          IX = IX + INCX
  130                 CONTINUE
                  endif
                  Y(JY) = Y(JY) + ALPHA*TEMP
                  JY = JY + INCY
  140         CONTINUE
          endif
      endif
!
      return
!
!     End of ZGEMV .
!
      END
      subroutine ZGERC(M,N,ALPHA,X,INCX,Y,INCY,A,LDA)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA
      integer(i4)  :: INCX,INCY,LDA,M,N
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),X(*),Y(*)
!     ..
!
!  Purpose
!  =======
!
!  ZGERC  performs the rank 1 operation
!
!     A := alpha*x*y**H + A,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and A is an m by n matrix.
!
!  Arguments
!  ==========
!
!  M      - integer(i4)  ::.
!           On entry, M specifies the number of rows of the matrix A.
!           M must be at least zero.
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - COMPLEX*16       array of dimension at least
!           ( 1 + ( m - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the m
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - integer(i4)  ::.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - integer(i4)  ::.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients. On exit, A is
!           overwritten by the updated matrix.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!  =====================================================================
!
!     .. Parameters ..
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP
      integer(i4)  :: I,INFO,IX,J,JY,KX
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!     ..
!
!     Test the input parameters.
!
      INFO = 0
      IF (M.LT.0) THEN
          INFO = 1
      else IF (N.LT.0) THEN
          INFO = 2
      else IF (INCX.EQ.0) THEN
          INFO = 5
      else IF (INCY.EQ.0) THEN
          INFO = 7
      else IF (LDA.LT.MAX(1,M)) THEN
          INFO = 9
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZGERC ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO)) return
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF (INCY.GT.0) THEN
          JY = 1
      else
          JY = 1 - (N-1)*INCY
      endif
      IF (INCX.EQ.1) THEN
          DO 20 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*DCONJG(Y(JY))
                  DO 10 I = 1,M
                      A(I,J) = A(I,J) + X(I)*TEMP
   10             CONTINUE
              endif
              JY = JY + INCY
   20     CONTINUE
      else
          IF (INCX.GT.0) THEN
              KX = 1
          else
              KX = 1 - (M-1)*INCX
          endif
          DO 40 J = 1,N
              IF (Y(JY).NE.ZERO) THEN
                  TEMP = ALPHA*DCONJG(Y(JY))
                  IX = KX
                  DO 30 I = 1,M
                      A(I,J) = A(I,J) + X(IX)*TEMP
                      IX = IX + INCX
   30             CONTINUE
              endif
              JY = JY + INCY
   40     CONTINUE
      endif
!
      return
!
!     End of ZGERC .
!
      END
      subroutine ZHER2(UPLO,N,ALPHA,X,INCX,Y,INCY,A,LDA)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA
      integer(i4)  :: INCX,INCY,LDA,N
      CHARACTER UPLO
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),X(*),Y(*)
!     ..
!
!  Purpose
!  =======
!
!  ZHER2  performs the hermitian rank 2 operation
!
!     A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
!
!  where alpha is a scalar, x and y are n element vectors and A is an n
!  by n hermitian matrix.
!
!  Arguments
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  X      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - integer(i4)  ::.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Y      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y.
!           Unchanged on exit.
!
!  INCY   - integer(i4)  ::.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the hermitian matrix and the strictly
!           lower triangular part of A is not referenced. On exit, the
!           upper triangular part of the array A is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the hermitian matrix and the strictly
!           upper triangular part of A is not referenced. On exit, the
!           lower triangular part of the array A is overwritten by the
!           lower triangular part of the updated matrix.
!           Note that the imaginary parts of the diagonal elements need
!           not be set, they are assumed to be zero, and on exit they
!           are set to zero.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 2 Blas routine.
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!  =====================================================================
!
!     .. Parameters ..
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP1,TEMP2
      integer(i4)  :: I,INFO,IX,IY,J,JX,JY,KX,KY
!     ..
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DBLE,DCONJG,MAX
!     ..
!
!     Test the input parameters.
!
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      else IF (N.LT.0) THEN
          INFO = 2
      else IF (INCX.EQ.0) THEN
          INFO = 5
      else IF (INCY.EQ.0) THEN
          INFO = 7
      else IF (LDA.LT.MAX(1,N)) THEN
          INFO = 9
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZHER2 ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((N.EQ.0) .OR. (ALPHA.EQ.ZERO)) return
!
!     Set up the start points in X and Y if the increments are not both
!     unity.
!
      IF ((INCX.NE.1) .OR. (INCY.NE.1)) THEN
          IF (INCX.GT.0) THEN
              KX = 1
          else
              KX = 1 - (N-1)*INCX
          endif
          IF (INCY.GT.0) THEN
              KY = 1
          else
              KY = 1 - (N-1)*INCY
          endif
          JX = KX
          JY = KY
      endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.
!
      IF (LSAME(UPLO,'U')) THEN
!
!        Form  A  when A is stored in the upper triangle.
!
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 20 J = 1,N
                  IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                      TEMP1 = ALPHA*DCONJG(Y(J))
                      TEMP2 = DCONJG(ALPHA*X(J))
                      DO 10 I = 1,J - 1
                          A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
   10                 CONTINUE
                      A(J,J) = DBLE(A(J,J)) + DBLE(X(J)*TEMP1+Y(J)*TEMP2)
                  else
                      A(J,J) = DBLE(A(J,J))
                  endif
   20         CONTINUE
          else
              DO 40 J = 1,N
                  IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                      TEMP1 = ALPHA*DCONJG(Y(JY))
                      TEMP2 = DCONJG(ALPHA*X(JX))
                      IX = KX
                      IY = KY
                      DO 30 I = 1,J - 1
                          A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
                          IX = IX + INCX
                          IY = IY + INCY
   30                 CONTINUE
                      A(J,J) = DBLE(A(J,J)) + DBLE(X(JX)*TEMP1+Y(JY)*TEMP2)
                  else
                      A(J,J) = DBLE(A(J,J))
                  endif
                  JX = JX + INCX
                  JY = JY + INCY
   40         CONTINUE
          endif
      else
!
!        Form  A  when A is stored in the lower triangle.
!
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 60 J = 1,N
                  IF ((X(J).NE.ZERO) .OR. (Y(J).NE.ZERO)) THEN
                      TEMP1 = ALPHA*DCONJG(Y(J))
                      TEMP2 = DCONJG(ALPHA*X(J))
                      A(J,J) = DBLE(A(J,J)) + DBLE(X(J)*TEMP1+Y(J)*TEMP2)
                      DO 50 I = J + 1,N
                          A(I,J) = A(I,J) + X(I)*TEMP1 + Y(I)*TEMP2
   50                 CONTINUE
                  else
                      A(J,J) = DBLE(A(J,J))
                  endif
   60         CONTINUE
          else
              DO 80 J = 1,N
                  IF ((X(JX).NE.ZERO) .OR. (Y(JY).NE.ZERO)) THEN
                      TEMP1 = ALPHA*DCONJG(Y(JY))
                      TEMP2 = DCONJG(ALPHA*X(JX))
                      A(J,J) = DBLE(A(J,J)) + DBLE(X(JX)*TEMP1+Y(JY)*TEMP2)
                      IX = JX
                      IY = JY
                      DO 70 I = J + 1,N
                          IX = IX + INCX
                          IY = IY + INCY
                          A(I,J) = A(I,J) + X(IX)*TEMP1 + Y(IY)*TEMP2
   70                 CONTINUE
                  else
                      A(J,J) = DBLE(A(J,J))
                  endif
                  JX = JX + INCX
                  JY = JY + INCY
   80         CONTINUE
          endif
      endif
!
      return
!
!     End of ZHER2 .
!
      END
      subroutine ZHEMV(UPLO,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA,BETA
      integer(i4)  :: INCX,INCY,LDA,N
      CHARACTER UPLO
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),X(*),Y(*)
!     ..
!
!  Purpose
!  =======
!
!  ZHEMV  performs the matrix-vector  operation
!
!     y := alpha*A*x + beta*y,
!
!  where alpha and beta are scalars, x and y are n element vectors and
!  A is an n by n hermitian matrix.
!
!  Arguments
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the upper or lower
!           triangular part of the array A is to be referenced as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the upper triangular part of A
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the lower triangular part of A
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular part of the hermitian matrix and the strictly
!           lower triangular part of A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular part of the hermitian matrix and the strictly
!           upper triangular part of A is not referenced.
!           Note that the imaginary parts of the diagonal elements need
!           not be set and are assumed to be zero.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x.
!           Unchanged on exit.
!
!  INCX   - integer(i4)  ::.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  BETA   - COMPLEX*16      .
!           On entry, BETA specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCY ) ).
!           Before entry, the incremented array Y must contain the n
!           element vector y. On exit, Y is overwritten by the updated
!           vector y.
!
!  INCY   - integer(i4)  ::.
!           On entry, INCY specifies the increment for the elements of
!           Y. INCY must not be zero.
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 2 Blas routine.
!  The vector and matrix arguments are not referenced when N = 0, or M = 0
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!  =====================================================================
!
!     .. Parameters ..
      complex(dpc) :: ONE
      PARAMETER (ONE= (1.0D+0,0.0D+0))
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP1,TEMP2
      integer(i4)  :: I,INFO,IX,IY,J,JX,JY,KX,KY
!     ..
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DBLE,DCONJG,MAX
!     ..
!
!     Test the input parameters.
!
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      else IF (N.LT.0) THEN
          INFO = 2
      else IF (LDA.LT.MAX(1,N)) THEN
          INFO = 5
      else IF (INCX.EQ.0) THEN
          INFO = 7
      else IF (INCY.EQ.0) THEN
          INFO = 10
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZHEMV ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((N.EQ.0) .OR. ((ALPHA.EQ.ZERO).AND. (BETA.EQ.ONE))) return
!
!     Set up the start points in  X  and  Y.
!
      IF (INCX.GT.0) THEN
          KX = 1
      else
          KX = 1 - (N-1)*INCX
      endif
      IF (INCY.GT.0) THEN
          KY = 1
      else
          KY = 1 - (N-1)*INCY
      endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through the triangular part
!     of A.
!
!     First form  y := beta*y.
!
      IF (BETA.NE.ONE) THEN
          IF (INCY.EQ.1) THEN
              IF (BETA.EQ.ZERO) THEN
                  DO 10 I = 1,N
                      Y(I) = ZERO
   10             CONTINUE
              else
                  DO 20 I = 1,N
                      Y(I) = BETA*Y(I)
   20             CONTINUE
              endif
          else
              IY = KY
              IF (BETA.EQ.ZERO) THEN
                  DO 30 I = 1,N
                      Y(IY) = ZERO
                      IY = IY + INCY
   30             CONTINUE
              else
                  DO 40 I = 1,N
                      Y(IY) = BETA*Y(IY)
                      IY = IY + INCY
   40             CONTINUE
              endif
          endif
      endif
      IF (ALPHA.EQ.ZERO) return
      IF (LSAME(UPLO,'U')) THEN
!
!        Form  y  when A is stored in upper triangle.
!
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 60 J = 1,N
                  TEMP1 = ALPHA*X(J)
                  TEMP2 = ZERO
                  DO 50 I = 1,J - 1
                      Y(I) = Y(I) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + DCONJG(A(I,J))*X(I)
   50             CONTINUE
                  Y(J) = Y(J) + TEMP1*DBLE(A(J,J)) + ALPHA*TEMP2
   60         CONTINUE
          else
              JX = KX
              JY = KY
              DO 80 J = 1,N
                  TEMP1 = ALPHA*X(JX)
                  TEMP2 = ZERO
                  IX = KX
                  IY = KY
                  DO 70 I = 1,J - 1
                      Y(IY) = Y(IY) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + DCONJG(A(I,J))*X(IX)
                      IX = IX + INCX
                      IY = IY + INCY
   70             CONTINUE
                  Y(JY) = Y(JY) + TEMP1*DBLE(A(J,J)) + ALPHA*TEMP2
                  JX = JX + INCX
                  JY = JY + INCY
   80         CONTINUE
          endif
      else
!
!        Form  y  when A is stored in lower triangle.
!
          IF ((INCX.EQ.1) .AND. (INCY.EQ.1)) THEN
              DO 100 J = 1,N
                  TEMP1 = ALPHA*X(J)
                  TEMP2 = ZERO
                  Y(J) = Y(J) + TEMP1*DBLE(A(J,J))
                  DO 90 I = J + 1,N
                      Y(I) = Y(I) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + DCONJG(A(I,J))*X(I)
   90             CONTINUE
                  Y(J) = Y(J) + ALPHA*TEMP2
  100         CONTINUE
          else
              JX = KX
              JY = KY
              DO 120 J = 1,N
                  TEMP1 = ALPHA*X(JX)
                  TEMP2 = ZERO
                  Y(JY) = Y(JY) + TEMP1*DBLE(A(J,J))
                  IX = JX
                  IY = JY
                  DO 110 I = J + 1,N
                      IX = IX + INCX
                      IY = IY + INCY
                      Y(IY) = Y(IY) + TEMP1*A(I,J)
                      TEMP2 = TEMP2 + DCONJG(A(I,J))*X(IX)
  110             CONTINUE
                  Y(JY) = Y(JY) + ALPHA*TEMP2
                  JX = JX + INCX
                  JY = JY + INCY
  120         CONTINUE
          endif
      endif
!
      return
!
!     End of ZHEMV .
!
      END
      FUNCTION DZNRM2(N,X,INCX)
!     .. Scalar Arguments ..
      use datatypes
      integer(i4)  :: INCX,N
      real(dp)     :: DZNRM2
!     ..
!     .. Array Arguments ..
      complex(dpc) :: X(*)
!     ..
!
!  Purpose
!  =======
!
!  DZNRM2 returns the euclidean norm of a vector via the function
!  name, so that
!
!     DZNRM2 := sqrt( x**H*x )
!
!  Further Details
!  ===============
!
!  -- This version written on 25-October-1982.
!     Modified on 14-October-1993 to inline the call to ZLASSQ.
!     Sven Hammarling, Nag Ltd.
!
!  =====================================================================
!
!     .. Parameters ..
      real(dp)    :: ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
!     ..
!     .. Local Scalars ..
      real(dp)    :: NORM,SCALE,SSQ,TEMP
      integer(i4)  :: IX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC ABS,DBLE,DIMAG,SQRT
!     ..
      IF (N.LT.1 .OR. INCX.LT.1) THEN
          NORM = ZERO
      else
          SCALE = ZERO
          SSQ = ONE
!        The following loop is equivalent to this call to the LAPACK
!        auxiliary routine:
!        CALL ZLASSQ( N, X, INCX, SCALE, SSQ )
!
          DO 10 IX = 1,1 + (N-1)*INCX,INCX
              IF (DBLE(X(IX)).NE.ZERO) THEN
                  TEMP = ABS(DBLE(X(IX)))
                  IF (SCALE.LT.TEMP) THEN
                      SSQ = ONE + SSQ* (SCALE/TEMP)**2
                      SCALE = TEMP
                  else
                      SSQ = SSQ + (TEMP/SCALE)**2
                  endif
              endif
              IF (DIMAG(X(IX)).NE.ZERO) THEN
                  TEMP = ABS(DIMAG(X(IX)))
                  IF (SCALE.LT.TEMP) THEN
                      SSQ = ONE + SSQ* (SCALE/TEMP)**2
                      SCALE = TEMP
                  else
                      SSQ = SSQ + (TEMP/SCALE)**2
                  endif
              endif
   10     CONTINUE
          NORM = SCALE*SQRT(SSQ)
      endif
!
      DZNRM2 = NORM
      return
!
!     End of DZNRM2.
!
      END
      subroutine ZSCAL(N,ZA,ZX,INCX)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ZA
      integer(i4)  :: INCX,N
!     ..
!     .. Array Arguments ..
      complex(dpc) :: ZX(*)
!     ..
!
!  Purpose
!  =======
!
!     ZSCAL scales a vector by a constant.
!
!  Further Details
!  ===============
!
!     jack dongarra, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!  =====================================================================
!
!     .. Local Scalars ..
      integer(i4)  :: I,NINCX
!     ..
      IF (N.LE.0 .OR. INCX.LE.0) return
      IF (INCX.EQ.1) THEN
!
!        code for increment equal to 1
!
         DO I = 1,N
            ZX(I) = ZA*ZX(I)
         enddo
      else
!
!        code for increment not equal to 1
!
         NINCX = N*INCX
         DO I = 1,NINCX,INCX
            ZX(I) = ZA*ZX(I)
         enddo
      endif
      return
      END
      subroutine ZTRMM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA
      integer(i4)  :: LDA,LDB,M,N
      CHARACTER DIAG,SIDE,TRANSA,UPLO
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),B(LDB,*)
!     ..
!
!  Purpose
!  =======
!
!  ZTRMM  performs one of the matrix-matrix operations
!
!     B := alpha*op( A )*B,   or   B := alpha*B*op( A )
!
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!
!     op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
!
!  Arguments
!  ==========
!
!  SIDE   - CHARACTER*1.
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:
!
!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
!
!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
!
!           Unchanged on exit.
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANSA - CHARACTER*1.
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              TRANSA = 'N' or 'n'   op( A ) = A.
!
!              TRANSA = 'T' or 't'   op( A ) = A**T.
!
!              TRANSA = 'C' or 'c'   op( A ) = A**H.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  M      - integer(i4)  ::.
!           On entry, M specifies the number of rows of B. M must be at
!           least zero.
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16      .
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).
!           Unchanged on exit.
!
!  B      - COMPLEX*16       array of DIMENSION ( LDB, n ).
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  LDB    - integer(i4)  ::.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!  =====================================================================
!
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP
      integer(i4)  :: I,INFO,J,K,NROWA
      LOGICAL LSIDE,NOCONJ,NOUNIT,UPPER
!     ..
!     .. Parameters ..
      complex(dpc) :: ONE
      PARAMETER (ONE= (1.0D+0,0.0D+0))
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!
!     Test the input parameters.
!
      LSIDE = LSAME(SIDE,'L')
      IF (LSIDE) THEN
          NROWA = M
      else
          NROWA = N
      endif
      NOCONJ = LSAME(TRANSA,'T')
      NOUNIT = LSAME(DIAG,'N')
      UPPER = LSAME(UPLO,'U')
!
      INFO = 0
      IF ((.NOT.LSIDE) .AND. (.NOT.LSAME(SIDE,'R'))) THEN
          INFO = 1
      else IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
          INFO = 2
      else IF ((.NOT.LSAME(TRANSA,'N')) .AND. (.NOT.LSAME(TRANSA,'T')) .AND. &
               (.NOT.LSAME(TRANSA,'C'))) THEN
          INFO = 3
      else IF ((.NOT.LSAME(DIAG,'U')) .AND. (.NOT.LSAME(DIAG,'N'))) THEN
          INFO = 4
      else IF (M.LT.0) THEN
          INFO = 5
      else IF (N.LT.0) THEN
          INFO = 6
      else IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 9
      else IF (LDB.LT.MAX(1,M)) THEN
          INFO = 11
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZTRMM ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF (M.EQ.0 .OR. N.EQ.0) return
!
!     And when  alpha.eq.zero.
!
      IF (ALPHA.EQ.ZERO) THEN
          DO 20 J = 1,N
              DO 10 I = 1,M
                  B(I,J) = ZERO
   10         CONTINUE
   20     CONTINUE
          return
      endif
!
!     Start the operations.
!
      IF (LSIDE) THEN
          IF (LSAME(TRANSA,'N')) THEN
!
!           Form  B := alpha*A*B.
!
              IF (UPPER) THEN
                  DO 50 J = 1,N
                      DO 40 K = 1,M
                          IF (B(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*B(K,J)
                              DO 30 I = 1,K - 1
                                  B(I,J) = B(I,J) + TEMP*A(I,K)
   30                         CONTINUE
                              IF (NOUNIT) TEMP = TEMP*A(K,K)
                              B(K,J) = TEMP
                          endif
   40                 CONTINUE
   50             CONTINUE
              else
                  DO 80 J = 1,N
                      DO 70 K = M,1,-1
                          IF (B(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*B(K,J)
                              B(K,J) = TEMP
                              IF (NOUNIT) B(K,J) = B(K,J)*A(K,K)
                              DO 60 I = K + 1,M
                                  B(I,J) = B(I,J) + TEMP*A(I,K)
   60                         CONTINUE
                          endif
   70                 CONTINUE
   80             CONTINUE
              endif
          else
!
!           Form  B := alpha*A**T*B   or   B := alpha*A**H*B.
!
              IF (UPPER) THEN
                  DO 120 J = 1,N
                      DO 110 I = M,1,-1
                          TEMP = B(I,J)
                          IF (NOCONJ) THEN
                              IF (NOUNIT) TEMP = TEMP*A(I,I)
                              DO 90 K = 1,I - 1
                                  TEMP = TEMP + A(K,I)*B(K,J)
   90                         CONTINUE
                          else
                              IF (NOUNIT) TEMP = TEMP*DCONJG(A(I,I))
                              DO 100 K = 1,I - 1
                                  TEMP = TEMP + DCONJG(A(K,I))*B(K,J)
  100                         CONTINUE
                          endif
                          B(I,J) = ALPHA*TEMP
  110                 CONTINUE
  120             CONTINUE
              else
                  DO 160 J = 1,N
                      DO 150 I = 1,M
                          TEMP = B(I,J)
                          IF (NOCONJ) THEN
                              IF (NOUNIT) TEMP = TEMP*A(I,I)
                              DO 130 K = I + 1,M
                                  TEMP = TEMP + A(K,I)*B(K,J)
  130                         CONTINUE
                          else
                              IF (NOUNIT) TEMP = TEMP*DCONJG(A(I,I))
                              DO 140 K = I + 1,M
                                  TEMP = TEMP + DCONJG(A(K,I))*B(K,J)
  140                         CONTINUE
                          endif
                          B(I,J) = ALPHA*TEMP
  150                 CONTINUE
  160             CONTINUE
              endif
          endif
      else
          IF (LSAME(TRANSA,'N')) THEN
!
!           Form  B := alpha*B*A.
!
              IF (UPPER) THEN
                  DO 200 J = N,1,-1
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 170 I = 1,M
                          B(I,J) = TEMP*B(I,J)
  170                 CONTINUE
                      DO 190 K = 1,J - 1
                          IF (A(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*A(K,J)
                              DO 180 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  180                         CONTINUE
                          endif
  190                 CONTINUE
  200             CONTINUE
              else
                  DO 240 J = 1,N
                      TEMP = ALPHA
                      IF (NOUNIT) TEMP = TEMP*A(J,J)
                      DO 210 I = 1,M
                          B(I,J) = TEMP*B(I,J)
  210                 CONTINUE
                      DO 230 K = J + 1,N
                          IF (A(K,J).NE.ZERO) THEN
                              TEMP = ALPHA*A(K,J)
                              DO 220 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  220                         CONTINUE
                          endif
  230                 CONTINUE
  240             CONTINUE
              endif
          else
!
!           Form  B := alpha*B*A**T   or   B := alpha*B*A**H.
!
              IF (UPPER) THEN
                  DO 280 K = 1,N
                      DO 260 J = 1,K - 1
                          IF (A(J,K).NE.ZERO) THEN
                              IF (NOCONJ) THEN
                                  TEMP = ALPHA*A(J,K)
                              else
                                  TEMP = ALPHA*DCONJG(A(J,K))
                              endif
                              DO 250 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  250                         CONTINUE
                          endif
  260                 CONTINUE
                      TEMP = ALPHA
                      IF (NOUNIT) THEN
                          IF (NOCONJ) THEN
                              TEMP = TEMP*A(K,K)
                          else
                              TEMP = TEMP*DCONJG(A(K,K))
                          endif
                      endif
                      IF (TEMP.NE.ONE) THEN
                          DO 270 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  270                     CONTINUE
                      endif
  280             CONTINUE
              else
                  DO 320 K = N,1,-1
                      DO 300 J = K + 1,N
                          IF (A(J,K).NE.ZERO) THEN
                              IF (NOCONJ) THEN
                                  TEMP = ALPHA*A(J,K)
                              else
                                  TEMP = ALPHA*DCONJG(A(J,K))
                              endif
                              DO 290 I = 1,M
                                  B(I,J) = B(I,J) + TEMP*B(I,K)
  290                         CONTINUE
                          endif
  300                 CONTINUE
                      TEMP = ALPHA
                      IF (NOUNIT) THEN
                          IF (NOCONJ) THEN
                              TEMP = TEMP*A(K,K)
                          else
                              TEMP = TEMP*DCONJG(A(K,K))
                          endif
                      endif
                      IF (TEMP.NE.ONE) THEN
                          DO 310 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  310                     CONTINUE
                      endif
  320             CONTINUE
              endif
          endif
      endif
!
      return
!
!     End of ZTRMM .
!
      END
      subroutine ZTRMV(UPLO,TRANS,DIAG,N,A,LDA,X,INCX)
!     .. Scalar Arguments ..
      use datatypes
      integer(i4)  :: INCX,LDA,N
      CHARACTER DIAG,TRANS,UPLO
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),X(*)
!     ..
!
!  Purpose
!  =======
!
!  ZTRMV  performs one of the matrix-vector operations
!
!     x := A*x,   or   x := A**T*x,   or   x := A**H*x,
!
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.
!
!  Arguments
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On entry, UPLO specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.
!
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry, TRANS specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'   x := A*x.
!
!              TRANS = 'T' or 't'   x := A**T*x.
!
!              TRANS = 'C' or 'c'   x := A**H*x.
!
!           Unchanged on exit.
!
!  DIAG   - CHARACTER*1.
!           On entry, DIAG specifies whether or not A is unit
!           triangular as follows:
!
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
!
!              DIAG = 'N' or 'n'   A is not assumed to be unit
!                                  triangular.
!
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry, N specifies the order of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
!           Before entry with  UPLO = 'U' or 'u', the leading n by n
!           upper triangular part of the array A must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           A is not referenced.
!           Before entry with UPLO = 'L' or 'l', the leading n by n
!           lower triangular part of the array A must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           A is not referenced.
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of
!           A are not referenced either, but are assumed to be unity.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least
!           max( 1, n ).
!           Unchanged on exit.
!
!  X      - COMPLEX*16       array of dimension at least
!           ( 1 + ( n - 1 )*abs( INCX ) ).
!           Before entry, the incremented array X must contain the n
!           element vector x. On exit, X is overwritten with the
!           tranformed vector x.
!
!  INCX   - integer(i4)  ::.
!           On entry, INCX specifies the increment for the elements of
!           X. INCX must not be zero.
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 2 Blas routine.
!  The vector and matrix arguments are not referenced when N = 0, or M = 0
!
!  -- Written on 22-October-1986.
!     Jack Dongarra, Argonne National Lab.
!     Jeremy Du Croz, Nag Central Office.
!     Sven Hammarling, Nag Central Office.
!     Richard Hanson, Sandia National Labs.
!
!  =====================================================================
!
!     .. Parameters ..
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP
      integer(i4)  :: I,INFO,IX,J,JX,KX
      LOGICAL NOCONJ,NOUNIT
!     ..
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCONJG,MAX
!     ..
!
!     Test the input parameters.
!
      INFO = 0
      IF (.NOT.LSAME(UPLO,'U') .AND. .NOT.LSAME(UPLO,'L')) THEN
          INFO = 1
      else IF (.NOT.LSAME(TRANS,'N') .AND. .NOT.LSAME(TRANS,'T') .AND. .NOT.LSAME(TRANS,'C')) THEN
          INFO = 2
      else IF (.NOT.LSAME(DIAG,'U') .AND. .NOT.LSAME(DIAG,'N')) THEN
          INFO = 3
      else IF (N.LT.0) THEN
          INFO = 4
      else IF (LDA.LT.MAX(1,N)) THEN
          INFO = 6
      else IF (INCX.EQ.0) THEN
          INFO = 8
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZTRMV ',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF (N.EQ.0) return
!
      NOCONJ = LSAME(TRANS,'T')
      NOUNIT = LSAME(DIAG,'N')
!
!     Set up the start point in X if the increment is not unity. This
!     will be  ( N - 1 )*INCX  too small for descending loops.
!
      IF (INCX.LE.0) THEN
          KX = 1 - (N-1)*INCX
      else IF (INCX.NE.1) THEN
          KX = 1
      endif
!
!     Start the operations. In this version the elements of A are
!     accessed sequentially with one pass through A.
!
      IF (LSAME(TRANS,'N')) THEN
!
!        Form  x := A*x.
!
          IF (LSAME(UPLO,'U')) THEN
              IF (INCX.EQ.1) THEN
                  DO 20 J = 1,N
                      IF (X(J).NE.ZERO) THEN
                          TEMP = X(J)
                          DO 10 I = 1,J - 1
                              X(I) = X(I) + TEMP*A(I,J)
   10                     CONTINUE
                          IF (NOUNIT) X(J) = X(J)*A(J,J)
                      endif
   20             CONTINUE
              else
                  JX = KX
                  DO 40 J = 1,N
                      IF (X(JX).NE.ZERO) THEN
                          TEMP = X(JX)
                          IX = KX
                          DO 30 I = 1,J - 1
                              X(IX) = X(IX) + TEMP*A(I,J)
                              IX = IX + INCX
   30                     CONTINUE
                          IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                      endif
                      JX = JX + INCX
   40             CONTINUE
              endif
          else
              IF (INCX.EQ.1) THEN
                  DO 60 J = N,1,-1
                      IF (X(J).NE.ZERO) THEN
                          TEMP = X(J)
                          DO 50 I = N,J + 1,-1
                              X(I) = X(I) + TEMP*A(I,J)
   50                     CONTINUE
                          IF (NOUNIT) X(J) = X(J)*A(J,J)
                      endif
   60             CONTINUE
              else
                  KX = KX + (N-1)*INCX
                  JX = KX
                  DO 80 J = N,1,-1
                      IF (X(JX).NE.ZERO) THEN
                          TEMP = X(JX)
                          IX = KX
                          DO 70 I = N,J + 1,-1
                              X(IX) = X(IX) + TEMP*A(I,J)
                              IX = IX - INCX
   70                     CONTINUE
                          IF (NOUNIT) X(JX) = X(JX)*A(J,J)
                      endif
                      JX = JX - INCX
   80             CONTINUE
              endif
          endif
      else
!
!        Form  x := A**T*x  or  x := A**H*x.
!
          IF (LSAME(UPLO,'U')) THEN
              IF (INCX.EQ.1) THEN
                  DO 110 J = N,1,-1
                      TEMP = X(J)
                      IF (NOCONJ) THEN
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 90 I = J - 1,1,-1
                              TEMP = TEMP + A(I,J)*X(I)
   90                     CONTINUE
                      else
                          IF (NOUNIT) TEMP = TEMP*DCONJG(A(J,J))
                          DO 100 I = J - 1,1,-1
                              TEMP = TEMP + DCONJG(A(I,J))*X(I)
  100                     CONTINUE
                      endif
                      X(J) = TEMP
  110             CONTINUE
              else
                  JX = KX + (N-1)*INCX
                  DO 140 J = N,1,-1
                      TEMP = X(JX)
                      IX = JX
                      IF (NOCONJ) THEN
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 120 I = J - 1,1,-1
                              IX = IX - INCX
                              TEMP = TEMP + A(I,J)*X(IX)
  120                     CONTINUE
                      else
                          IF (NOUNIT) TEMP = TEMP*DCONJG(A(J,J))
                          DO 130 I = J - 1,1,-1
                              IX = IX - INCX
                              TEMP = TEMP + DCONJG(A(I,J))*X(IX)
  130                     CONTINUE
                      endif
                      X(JX) = TEMP
                      JX = JX - INCX
  140             CONTINUE
              endif
          else
              IF (INCX.EQ.1) THEN
                  DO 170 J = 1,N
                      TEMP = X(J)
                      IF (NOCONJ) THEN
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 150 I = J + 1,N
                              TEMP = TEMP + A(I,J)*X(I)
  150                     CONTINUE
                      else
                          IF (NOUNIT) TEMP = TEMP*DCONJG(A(J,J))
                          DO 160 I = J + 1,N
                              TEMP = TEMP + DCONJG(A(I,J))*X(I)
  160                     CONTINUE
                      endif
                      X(J) = TEMP
  170             CONTINUE
              else
                  JX = KX
                  DO 200 J = 1,N
                      TEMP = X(JX)
                      IX = JX
                      IF (NOCONJ) THEN
                          IF (NOUNIT) TEMP = TEMP*A(J,J)
                          DO 180 I = J + 1,N
                              IX = IX + INCX
                              TEMP = TEMP + A(I,J)*X(IX)
  180                     CONTINUE
                      else
                          IF (NOUNIT) TEMP = TEMP*DCONJG(A(J,J))
                          DO 190 I = J + 1,N
                              IX = IX + INCX
                              TEMP = TEMP + DCONJG(A(I,J))*X(IX)
  190                     CONTINUE
                      endif
                      X(JX) = TEMP
                      JX = JX + INCX
  200             CONTINUE
              endif
          endif
      endif
!
      return
!
!     End of ZTRMV .
!
      END
      subroutine ZDSCAL(N,DA,ZX,INCX)
!     .. Scalar Arguments ..
      use datatypes
      real(dp)    :: DA
      integer(i4)  :: INCX,N
!     ..
!     .. Array Arguments ..
      complex(dpc) :: ZX(*)
!     ..
!
!  Purpose
!  =======
!
!     ZDSCAL scales a vector by a constant.
!
!  Further Details
!  ===============
!
!     jack dongarra, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
!  =====================================================================
!
!     .. Local Scalars ..
      integer(i4)  :: I,NINCX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DCMPLX
!     ..
      IF (N.LE.0 .OR. INCX.LE.0) return
      IF (INCX.EQ.1) THEN
!
!        code for increment equal to 1
!
         DO I = 1,N
            ZX(I) = DCMPLX(DA,0.0d0)*ZX(I)
         enddo
      else
!
!        code for increment not equal to 1
!
         NINCX = N*INCX
         DO I = 1,NINCX,INCX
            ZX(I) = DCMPLX(DA,0.0d0)*ZX(I)
         enddo
      endif
      return
      END
      subroutine ZHER2K(UPLO,TRANS,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
!     .. Scalar Arguments ..
      use datatypes
      complex(dpc) :: ALPHA
      real(dp)    :: BETA
      integer(i4)  :: K,LDA,LDB,LDC,N
      CHARACTER TRANS,UPLO
!     ..
!     .. Array Arguments ..
      complex(dpc) :: A(LDA,*),B(LDB,*),C(LDC,*)
!     ..
!
!  Purpose
!  =======
!
!  ZHER2K  performs one of the hermitian rank 2k operations
!
!     C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
!
!  or
!
!     C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
!
!  where  alpha and beta  are scalars with  beta  real,  C is an  n by n
!  hermitian matrix and  A and B  are  n by k matrices in the first case
!  and  k by n  matrices in the second case.
!
!  Arguments
!  ==========
!
!  UPLO   - CHARACTER*1.
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:
!
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.
!
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.
!
!           Unchanged on exit.
!
!  TRANS  - CHARACTER*1.
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:
!
!              TRANS = 'N' or 'n'    C := alpha*A*B**H          +
!                                         conjg( alpha )*B*A**H +
!                                         beta*C.
!
!              TRANS = 'C' or 'c'    C := alpha*A**H*B          +
!                                         conjg( alpha )*B**H*A +
!                                         beta*C.
!
!           Unchanged on exit.
!
!  N      - integer(i4)  ::.
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer(i4)  ::.
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'C' or 'c',  K  specifies  the number of rows of the
!           matrices  A and B.  K must be at least zero.
!           Unchanged on exit.
!
!  ALPHA  - COMPLEX*16         .
!           On entry, ALPHA specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  LDA    - integer(i4)  ::.
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  LDB    - integer(i4)  ::.
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).
!           Unchanged on exit.
!
!  BETA   - real(dp)    ::            .
!           On entry, BETA specifies the scalar beta.
!           Unchanged on exit.
!
!  C      - COMPLEX*16          array of DIMENSION ( LDC, n ).
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
!           upper triangular part of the array C must contain the upper
!           triangular part  of the  hermitian matrix  and the strictly
!           lower triangular part of C is not referenced.  On exit, the
!           upper triangular part of the array  C is overwritten by the
!           upper triangular part of the updated matrix.
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
!           lower triangular part of the array C must contain the lower
!           triangular part  of the  hermitian matrix  and the strictly
!           upper triangular part of C is not referenced.  On exit, the
!           lower triangular part of the array  C is overwritten by the
!           lower triangular part of the updated matrix.
!           Note that the imaginary parts of the diagonal elements need
!           not be set,  they are assumed to be zero,  and on exit they
!           are set to zero.
!
!  LDC    - integer(i4)  ::.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).
!           Unchanged on exit.
!
!  Further Details
!  ===============
!
!  Level 3 Blas routine.
!
!  -- Written on 8-February-1989.
!     Jack Dongarra, Argonne National Laboratory.
!     Iain Duff, AERE Harwell.
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.
!     Sven Hammarling, Numerical Algorithms Group Ltd.
!
!  -- Modified 8-Nov-93 to set C(J,J) to DBLE( C(J,J) ) when BETA = 1.
!     Ed Anderson, Cray Research Inc.
!
!  =====================================================================
!
!     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC DBLE,DCONJG,MAX
!     ..
!     .. Local Scalars ..
      complex(dpc) :: TEMP1,TEMP2
      integer(i4)  :: I,INFO,J,L,NROWA
      LOGICAL UPPER
!     ..
!     .. Parameters ..
      real(dp)    :: ONE
      PARAMETER (ONE=1.0D+0)
      complex(dpc) :: ZERO
      PARAMETER (ZERO= (0.0D+0,0.0D+0))
!     ..
!
!     Test the input parameters.
!
      IF (LSAME(TRANS,'N')) THEN
          NROWA = N
      else
          NROWA = K
      endif
      UPPER = LSAME(UPLO,'U')
!
      INFO = 0
      IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
          INFO = 1
      else IF ((.NOT.LSAME(TRANS,'N')) .AND. (.NOT.LSAME(TRANS,'C'))) THEN
          INFO = 2
      else IF (N.LT.0) THEN
          INFO = 3
      else IF (K.LT.0) THEN
          INFO = 4
      else IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 7
      else IF (LDB.LT.MAX(1,NROWA)) THEN
          INFO = 9
      else IF (LDC.LT.MAX(1,N)) THEN
          INFO = 12
      endif
      IF (INFO.NE.0) THEN
          CALL XERBLA('ZHER2K',INFO)
          return
      endif
!
!     Quick return if possible.
!
      IF ((N.EQ.0) .OR. (((ALPHA.EQ.ZERO).OR. (K.EQ.0)).AND. (BETA.EQ.ONE))) return
!
!     And when  alpha.eq.zero.
!
      IF (ALPHA.EQ.ZERO) THEN
          IF (UPPER) THEN
              IF (BETA.EQ.DBLE(ZERO)) THEN
                  DO 20 J = 1,N
                      DO 10 I = 1,J
                          C(I,J) = ZERO
   10                 CONTINUE
   20             CONTINUE
              else
                  DO 40 J = 1,N
                      DO 30 I = 1,J - 1
                          C(I,J) = BETA*C(I,J)
   30                 CONTINUE
                      C(J,J) = BETA*DBLE(C(J,J))
   40             CONTINUE
              endif
          else
              IF (BETA.EQ.DBLE(ZERO)) THEN
                  DO 60 J = 1,N
                      DO 50 I = J,N
                          C(I,J) = ZERO
   50                 CONTINUE
   60             CONTINUE
              else
                  DO 80 J = 1,N
                      C(J,J) = BETA*DBLE(C(J,J))
                      DO 70 I = J + 1,N
                          C(I,J) = BETA*C(I,J)
   70                 CONTINUE
   80             CONTINUE
              endif
          endif
          return
      endif
!
!     Start the operations.
!
      IF (LSAME(TRANS,'N')) THEN
!
!        Form  C := alpha*A*B**H + conjg( alpha )*B*A**H +
!                   C.
!
          IF (UPPER) THEN
              DO 130 J = 1,N
                  IF (BETA.EQ.DBLE(ZERO)) THEN
                      DO 90 I = 1,J
                          C(I,J) = ZERO
   90                 CONTINUE
                  else IF (BETA.NE.ONE) THEN
                      DO 100 I = 1,J - 1
                          C(I,J) = BETA*C(I,J)
  100                 CONTINUE
                      C(J,J) = BETA*DBLE(C(J,J))
                  else
                      C(J,J) = DBLE(C(J,J))
                  endif
                  DO 120 L = 1,K
                      IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                          TEMP1 = ALPHA*DCONJG(B(J,L))
                          TEMP2 = DCONJG(ALPHA*A(J,L))
                          DO 110 I = 1,J - 1
                              C(I,J) = C(I,J) + A(I,L)*TEMP1 + B(I,L)*TEMP2
  110                     CONTINUE
                          C(J,J) = DBLE(C(J,J)) + DBLE(A(J,L)*TEMP1+B(J,L)*TEMP2)
                      endif
  120             CONTINUE
  130         CONTINUE
          else
              DO 180 J = 1,N
                  IF (BETA.EQ.DBLE(ZERO)) THEN
                      DO 140 I = J,N
                          C(I,J) = ZERO
  140                 CONTINUE
                  else IF (BETA.NE.ONE) THEN
                      DO 150 I = J + 1,N
                          C(I,J) = BETA*C(I,J)
  150                 CONTINUE
                      C(J,J) = BETA*DBLE(C(J,J))
                  else
                      C(J,J) = DBLE(C(J,J))
                  endif
                  DO 170 L = 1,K
                      IF ((A(J,L).NE.ZERO) .OR. (B(J,L).NE.ZERO)) THEN
                          TEMP1 = ALPHA*DCONJG(B(J,L))
                          TEMP2 = DCONJG(ALPHA*A(J,L))
                          DO 160 I = J + 1,N
                              C(I,J) = C(I,J) + A(I,L)*TEMP1 + B(I,L)*TEMP2
  160                     CONTINUE
                          C(J,J) = DBLE(C(J,J)) + DBLE(A(J,L)*TEMP1+B(J,L)*TEMP2)
                      endif
  170             CONTINUE
  180         CONTINUE
          endif
      else
!
!        Form  C := alpha*A**H*B + conjg( alpha )*B**H*A +
!                   C.
!
          IF (UPPER) THEN
              DO 210 J = 1,N
                  DO 200 I = 1,J
                      TEMP1 = ZERO
                      TEMP2 = ZERO
                      DO 190 L = 1,K
                          TEMP1 = TEMP1 + DCONJG(A(L,I))*B(L,J)
                          TEMP2 = TEMP2 + DCONJG(B(L,I))*A(L,J)
  190                 CONTINUE
                      IF (I.EQ.J) THEN
                          IF (BETA.EQ.DBLE(ZERO)) THEN
                              C(J,J) = DBLE(ALPHA*TEMP1+ DCONJG(ALPHA)*TEMP2)
                          else
                              C(J,J) = BETA*DBLE(C(J,J)) + DBLE(ALPHA*TEMP1+ DCONJG(ALPHA)*TEMP2)
                          endif
                      else
                          IF (BETA.EQ.DBLE(ZERO)) THEN
                              C(I,J) = ALPHA*TEMP1 + DCONJG(ALPHA)*TEMP2
                          else
                              C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + DCONJG(ALPHA)*TEMP2
                          endif
                      endif
  200             CONTINUE
  210         CONTINUE
          else
              DO 240 J = 1,N
                  DO 230 I = J,N
                      TEMP1 = ZERO
                      TEMP2 = ZERO
                      DO 220 L = 1,K
                          TEMP1 = TEMP1 + DCONJG(A(L,I))*B(L,J)
                          TEMP2 = TEMP2 + DCONJG(B(L,I))*A(L,J)
  220                 CONTINUE
                      IF (I.EQ.J) THEN
                          IF (BETA.EQ.DBLE(ZERO)) THEN
                              C(J,J) = DBLE(ALPHA*TEMP1+ DCONJG(ALPHA)*TEMP2)
                          else
                              C(J,J) = BETA*DBLE(C(J,J)) + DBLE(ALPHA*TEMP1+ DCONJG(ALPHA)*TEMP2)
                          endif
                      else
                          IF (BETA.EQ.DBLE(ZERO)) THEN
                              C(I,J) = ALPHA*TEMP1 + DCONJG(ALPHA)*TEMP2
                          else
                              C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 + DCONJG(ALPHA)*TEMP2
                          endif
                      endif
  230             CONTINUE
  240         CONTINUE
          endif
      endif
!
      return
!
!     End of ZHER2K.
!
      END

!> \brief \b DROT
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       subroutine DROT(N,DX,INCX,DY,INCY,C,S)
! 
!       .. Scalar Arguments ..
!       DOUBLE PRECISION C,S
!       INTEGER INCX,INCY,N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION DX(*),DY(*)
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!>    DROT applies a plane rotation.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      subroutine DROT(N,DX,INCX,DY,INCY,C,S)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      use datatypes
      real(dp)    :: C,S
      integer(i4) :: INCX,INCY,N
!     ..
!     .. Array Arguments ..
      real(dp)    :: DX(*),DY(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      real(dp)    :: DTEMP
      integer(i4) :: I,IX,IY
!     ..
      IF (N.LE.0) return
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
!
!       code for both increments equal to 1
!
         DO I = 1,N
            DTEMP = C*DX(I) + S*DY(I)
            DY(I) = C*DY(I) - S*DX(I)
            DX(I) = DTEMP
         enddo
      else
!
!       code for unequal increments or equal increments not equal
!         to 1
!
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
            DTEMP = C*DX(IX) + S*DY(IY)
            DY(IY) = C*DY(IY) - S*DX(IX)
            DX(IX) = DTEMP
            IX = IX + INCX
            IY = IY + INCY
         enddo
      endif
      return
      END
!> \brief \b ZDROT
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at 
!            http://www.netlib.org/lapack/explore-html/ 
!
!  Definition:
!  ===========
!
!       subroutine ZDROT( N, CX, INCX, CY, INCY, C, S )
! 
!       .. Scalar Arguments ..
!       INTEGER            INCX, INCY, N
!       DOUBLE PRECISION   C, S
!       ..
!       .. Array Arguments ..
!       COMPLEX*16         CX( * ), CY( * )
!       ..
!  
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> Applies a plane rotation, where the cos and sin (c and s) are real
!> and the vectors cx and cy are complex.
!> jack dongarra, linpack, 3/11/78.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>           On entry, N specifies the order of the vectors cx and cy.
!>           N must be at least zero.
!> \endverbatim
!>
!> \param[in,out] CX
!> \verbatim
!>          CX is COMPLEX*16 array, dimension at least
!>           ( 1 + ( N - 1 )*abs( INCX ) ).
!>           Before entry, the incremented array CX must contain the n
!>           element vector cx. On exit, CX is overwritten by the updated
!>           vector cx.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>           On entry, INCX specifies the increment for the elements of
!>           CX. INCX must not be zero.
!> \endverbatim
!>
!> \param[in,out] CY
!> \verbatim
!>          CY is COMPLEX*16 array, dimension at least
!>           ( 1 + ( N - 1 )*abs( INCY ) ).
!>           Before entry, the incremented array CY must contain the n
!>           element vector cy. On exit, CY is overwritten by the updated
!>           vector cy.
!> \endverbatim
!>
!> \param[in] INCY
!> \verbatim
!>          INCY is INTEGER
!>           On entry, INCY specifies the increment for the elements of
!>           CY. INCY must not be zero.
!> \endverbatim
!>
!> \param[in] C
!> \verbatim
!>          C is DOUBLE PRECISION
!>           On entry, C specifies the cosine, cos.
!> \endverbatim
!>
!> \param[in] S
!> \verbatim
!>          S is DOUBLE PRECISION
!>           On entry, S specifies the sine, sin.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup complex16_blas_level1
!
!  =====================================================================
      subroutine ZDROT( N, CX, INCX, CY, INCY, C, S )
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      use datatypes
      integer(i4)  :: INCX, INCY, N
      real(dp)     :: C, S
!     ..
!     .. Array Arguments ..
      complex(dpc) :: CX( * ), CY( * )
!     ..
!
! =====================================================================
!
!     .. Local Scalars ..
      integer(i4)  :: I, IX, IY
      complex(dpc) :: CTEMP
!     ..
!     .. Executable Statements ..
!
      IF( N.LE.0 ) return
      IF( INCX.EQ.1 .AND. INCY.EQ.1 ) THEN
!
!        code for both increments equal to 1
!
         DO I = 1, N
            CTEMP = C*CX( I ) + S*CY( I )
            CY( I ) = C*CY( I ) - S*CX( I )
            CX( I ) = CTEMP
         enddo
      else
!
!        code for unequal increments or equal increments not equal
!          to 1
!
         IX = 1
         IY = 1
         IF( INCX.LT.0 ) IX = ( -N+1 )*INCX + 1
         IF( INCY.LT.0 ) IY = ( -N+1 )*INCY + 1
         DO I = 1, N
            CTEMP = C*CX( IX ) + S*CY( IY )
            CY( IY ) = C*CY( IY ) - S*CX( IX )
            CX( IX ) = CTEMP
            IX = IX + INCX
            IY = IY + INCY
         enddo
      endif
      return
      END
