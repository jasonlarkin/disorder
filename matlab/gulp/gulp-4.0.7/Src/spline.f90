!
!  Spline common blocks and parameters:
!
!  nsplpt  = no. of splined points for a given potential
!  nsplty  = type of spline for a given potential
!            1 => rational function
!            3 => cubic spline
!            4 => quartic spline
!            5 => quintic spline
!  splr    = distance for spline point
!  splf    = function for spline point
!  d1f     = first derivatives of spline
!  d2f     = second derivatives of spline
!
  subroutine setspl(rr,ff,npoints,npot,rmax)
!
!  Set up routine for spline - estimate first and second derivatives
!  at each point rr() given the function ff(). rmax is the potential
!  cutoff.
!
!  Routine from David Cooper, Univ. of Liverpool.
!
  use splinedata
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)        :: npoints
  integer(i4)        :: npot
  real(dp)           :: ff(*)
  real(dp)           :: rmax
  real(dp)           :: rr(*)
!
!  Local variables
!
  real(dp)           :: cputime
  real(dp)           :: t1
  real(dp)           :: t2
!
  t1 = cputime()
  if (ff(npoints).ne.0.0_dp) then
    ff(npoints+1) = 0.0_dp
    if (rr(npoints).lt.rmax) then
      rr(npoints+1) = rmax
    else
      rr(npoints+1) = 1.1_dp*rr(npoints)
    endif
    npoints = npoints + 1
    if (npoints.gt.maxpts) then
      maxpts = npoints
      call changemaxpts
    endif
  endif
  call slopesl(rr,ff,d1f(1:1,npot),npoints)
  call slopesl(rr,d1f(1:1,npot),d2f(1:,npot),npoints)
  t2 = cputime()
  tspline = tspline + t2 - t1
!
  return
  end
!
  subroutine spline(npot,npoints,rr,ff,r,v,nspt,lgrad1,lgrad2)
!
!  Modules
!
  use splinedata
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)        :: npoints
  integer(i4)        :: npot
  integer(i4)        :: nspt
  logical            :: lgrad1
  logical            :: lgrad2
  real(dp)           :: ff(*)
  real(dp)           :: r
  real(dp)           :: rr(*)
  real(dp)           :: v(*)
!
!  Local variables
!
  integer(i4)        :: jfail
  real(dp)           :: cputime
  real(dp)           :: gremlin
  real(dp)           :: t1
  real(dp)           :: t2
!
  t1 = cputime()
  jfail = 0
  v(1) = gremlin(npoints,rr,ff,r,jfail,nspt)
  if (lgrad1) then
    v(2) = gremlin(npoints,rr,d1f(1:1,npot),r,jfail,nspt)
  endif
  if (lgrad2) then
    v(3) = gremlin(npoints,rr,d2f(1:1,npot),r,jfail,nspt)
  endif
  t2 = cputime()
  tspline = tspline + t2 - t1
!
  return
  end
!
  subroutine slopesl(x,y,z,n)
!
!  Evaluates the derivative of a pointwise function
!  using n-point lagrangian interpolation.
!
!  Routine by David Cooper, Univ. of Liverpool.
!
  use datatypes
  use iochannels
  implicit none
!
!  Passed variables
!
  integer(i4)        :: n
  real(dp)           :: x(*)
  real(dp)           :: y(*)
  real(dp)           :: z(*)
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: imin1
  integer(i4)        :: imin2
  integer(i4)        :: nhalf
  integer(i4)        :: npoint
  real(dp)           :: derivl
  real(dp)           :: sum
  real(dp)           :: sum2
  real(dp)           :: workz
!
  nhalf = 3
  npoint = 2*nhalf + 1
  if (npoint.gt.15) then
    write(ioout,6200) npoint
    call stopnow('spline')
  endif
  if (npoint.lt.3) then
    write(ioout,6400) npoint
    call stopnow('spline')
  endif
  if (n-2.lt.npoint) then
    write(ioout,6100) npoint,n
    call stopnow('spline')
  endif
  sum = 0.0_dp
  sum2 = 0.0_dp
  do ii = 1,n
    imin1 = ii - nhalf
    imin1 = max(1,imin1)
    imin1 = min(imin1,n+1-npoint)
    imin2 = ii - nhalf - 1
    imin2 = max(1,imin2)
    imin2 = min(imin2,n+1-npoint-2)
    z(ii) = derivl(x(imin1),y(imin1),npoint,  ii+1-imin1)
    workz = derivl(x(imin2),y(imin2),npoint+2,ii+1-imin2)
    if (dabs(z(ii)).gt.1.d-10) sum = sum + ((z(ii)-workz)/z(ii))**2
    if (dabs(z(ii)).gt.1.d-10) sum2 = sum2 + (z(ii)-workz)**2
  enddo
  return
6100  format('  **** Warning - you have asked for ',i2,'-point interpolation with only ',i2,' points ****')
6200  format('  **** Warning - ',i8,'-point interpolation is dangerous. ****')
6400  format('  **** Warning - ',i1,'-point interpolation ****')
  end
!
  function derivl(a,f,n,i)
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)        :: i
  integer(i4)        :: n
  real(dp)           :: a(*)
  real(dp)           :: derivl
  real(dp)           :: f(*)
!
!  Local variables
!
  integer(i4)        :: l
  integer(i4)        :: m
  real(dp)           :: ai
  real(dp)           :: bi
!
!  Function to evaluate the derivative of a pointwise function at
!  one of the supplied points using n-point lagrangian interpolation.
!  on input, a(n) are the points and f(n) the function values.
!  i is the index of the point where the derivative is required.
!
!  Routine written by David Cooper, Univ. of Liverpool.
!
  ai = a(i)
  bi = 0.0_dp
  do 100 m = 1,n
    if (m.eq.i) goto 100
    bi = bi + 1.0_dp/(ai - a(m))
100   continue
  derivl = bi*f(i)
  do 300 m = 1,n
    if (m.eq.i) goto 300
    bi = 1.0_dp
    do 200 l = 1,n
      if (l.eq.m) goto 200
      bi = bi/(a(m) - a(l))
      if (l.eq.i) goto 200
      bi = bi*(ai - a(l))
200     continue
    derivl = derivl + bi*f(m)
300   continue
  return
  end
!
  function gremlin(n,x,y,xwant,jfail,nspt)
!
!  Routine written by David Cooper, Univ. of Liverpool.
!  Modified to remove NAG calls by JDG.
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), parameter :: ntry = 10
  integer(i4)            :: jfail
  integer(i4)            :: n
  integer(i4)            :: nspt
  real(dp)               :: gremlin
  real(dp)               :: x(*)
  real(dp)               :: xwant
  real(dp)               :: y(*)
!
!  Local variables
!
  integer(i4)            :: i
  integer(i4)            :: ibeg
  integer(i4)            :: ntry2
  real(dp)               :: yerror
  real(dp)               :: ywant
  real(dp)               :: znew
  real(dp)               :: zold
!
  ntry2 = 2*ntry
  do i = 1,n
    if (x(i).eq.xwant) then
      gremlin = y(i)
      return
    endif
  enddo
  znew = x(1) - xwant
  ywant = 0.0_dp
  do i = 2,n
    zold = znew
    znew = x(i) - xwant
    if (zold*znew.lt.0.0_dp) then
      ibeg = i - ntry
      if (ibeg.lt.1 .or. ibeg.gt.n+1-ntry2) then
        if (nspt.eq.1) then
          call ratint(x,y,n,xwant,ywant,yerror)
        elseif (nspt.eq.3) then
          call cubspl(x,y,n,xwant,ywant)
        endif
      else
        if (nspt.eq.1) then
          call ratint(x(ibeg),y(ibeg),ntry2,xwant,ywant,yerror)
        elseif (nspt.eq.3) then
          call cubspl(x(ibeg),y(ibeg),ntry2,xwant,ywant)
        endif
      endif
      gremlin = ywant
      jfail = 0
      return
    endif
  enddo
  gremlin = ywant
!
  return
  end
!
  subroutine ratint(xa,ya,n,x,y,dy)
!
!  Numerical recipes routine for rational function interpolation
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)                         :: n
  real(dp)                            :: dy
  real(dp)                            :: x
  real(dp)                            :: xa(*)
  real(dp)                            :: y
  real(dp)                            :: ya(*)
!
!  Passed variables
!
  integer(i4)                         :: i
  integer(i4)                         :: m
  integer(i4)                         :: ns
  integer(i4)                         :: status
  real(dp), dimension(:), allocatable :: c
  real(dp), dimension(:), allocatable :: d
  real(dp)                            :: dd
  real(dp)                            :: h
  real(dp)                            :: hh
  real(dp)                            :: t
  real(dp)                            :: tiny
  real(dp)                            :: w
!
  allocate(c(n),stat=status)
  if (status/=0) call outofmemory('ratint','c')
  allocate(d(n),stat=status)
  if (status/=0) call outofmemory('ratint','d')
!
  tiny = 1.0d-25
  ns = 1
  hh = abs(x-xa(1))
  do i = 1,n
    h = abs(x-xa(i))
    if (h.eq.0.0_dp) then
      y = ya(i)
      dy = 0.0_dp
      return
    elseif (h.lt.hh) then
      ns = i
      hh = h
    endif
    c(i) = ya(i)
    d(i) = ya(i) + tiny
  enddo
  y = ya(ns)
  ns = ns - 1
  do m = 1,n-1
    do i = 1,n-m
      w = c(i+1) - d(i)
      h = xa(i+m) - x
      t = (xa(i) - x)*d(i)/h
      dd = t - c(i+1)
      if (dd.eq.0.0_dp) then
        call outerror('badly conditioned interpolating function',0_i4)
        call stopnow('spline')
      endif
      dd = w/dd
      d(i) = c(i+1)*dd
      c(i) = t*dd
    enddo
    if (2*ns.lt.n-m) then
      dy = c(ns+1)
    else
      dy = d(ns)
      ns = ns - 1
    endif
    y = y + dy
  enddo
!
  deallocate(d,stat=status)
  if (status/=0) call deallocate_error('ratint','d')
  deallocate(c,stat=status)
  if (status/=0) call deallocate_error('ratint','c')
!
  return
  end
!
  subroutine cubspl(xa,ya,n,x,y)
!
!  Numerical recipes routine for cubic spline (spline and splint merged)
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)                         :: n
  real(dp)                            :: x
  real(dp)                            :: xa(*)
  real(dp)                            :: y
  real(dp)                            :: ya(*)
!
!  Local variables
!
  integer(i4)                         :: i
  integer(i4)                         :: k
  integer(i4)                         :: khi
  integer(i4)                         :: klo
  integer(i4)                         :: status
  real(dp)                            :: a
  real(dp)                            :: atrm
  real(dp)                            :: b
  real(dp)                            :: btrm
  real(dp)                            :: h
  real(dp)                            :: p
  real(dp)                            :: qn
  real(dp)                            :: sig
  real(dp)                            :: trm1
  real(dp)                            :: trm2
  real(dp)                            :: trm3
  real(dp), dimension(:), allocatable :: u
  real(dp)                            :: un
  real(dp), dimension(:), allocatable :: y2
  real(dp)                            :: yp1
  real(dp)                            :: ypn
!
  allocate(y2(n),stat=status)
  if (status/=0) call outofmemory('cubspl','y2')
  allocate(u(n),stat=status)
  if (status/=0) call outofmemory('cubspl','y2')
!
  ypn = 0.0_dp
!
!  Estimate yp1 based on quadratic fit to first three points
!
  trm1 = (1.0_dp/(xa(2)+xa(3)) - 1.0_dp/(xa(1)+xa(2)))
  trm1 = 1.0_dp/trm1
  trm2 = (ya(2)-ya(3))/(xa(2)*xa(2)-xa(3)*xa(3))
  trm3 = trm2-(ya(1)-ya(2))/(xa(1)*xa(1)-xa(2)*xa(2))
  btrm = trm3*trm1
  atrm = trm2 - btrm/(xa(2)+xa(3))
  yp1 = 2.0_dp*atrm*xa(1) + btrm
  y2(1) = - 0.5_dp
  u(1) = (3.0_dp/(xa(2)-xa(1)))*((ya(2)-ya(1))/(xa(2)-xa(1))-yp1)
  do i = 2,n-1
    sig = (xa(i)-xa(i-1))/(xa(i+1)-xa(i-1))
    p = sig*y2(i-1) + 2.0_dp
    y2(i) = (sig - 1.0_dp)/p
    u(i) = (6.0_dp*((ya(i+1)-ya(i))/(xa(i+1)-xa(i))-(ya(i)-ya(i-1))/ &
      (xa(i)-xa(i-1)))/(xa(i+1)-xa(i-1))-sig*u(i-1))/p
  enddo
  qn = - 0.5_dp
  un = (3.0_dp/(xa(n)-xa(n-1)))*(ypn-(ya(n)-ya(n-1))/(xa(n)-xa(n-1)))
  y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.0_dp)
  do k = n-1,1,-1
    y2(k) = y2(k)*y2(k+1) + u(k)
  enddo
!
  klo = 1
  khi = n
1 if (khi-klo.gt.1) then
    k = (khi + klo)/2
    if (xa(k).gt.x) then
      khi = k
    else
      klo = k
    endif
    goto 1
  endif
  h = xa(khi) - xa(klo)
  a = (xa(khi) - x)/h
  b = (x - xa(klo))/h
  y = a*ya(klo) + b*ya(khi) + ((a**3-a)*y2(klo) + (b**3-b)*y2(khi))*(h*h)/6.0_dp
!
  deallocate(u,stat=status)
  if (status/=0) call deallocate_error('cubspl','u')
  deallocate(y2,stat=status)
  if (status/=0) call deallocate_error('cubspl','y2')
!
  return
  end
