!**********
!  Gamma  *
!**********
  subroutine gammas(na,nb,za,zb,r,gam,dgam,dza,dzb,d2zaa,d2zab,d2zbb,d2zra,d2zrb,d2gamr2)
!
!  Subroutine calculates the integral over two s orbtials for Slater functions
!
!  On input :
!
!  na  =  principal quantum number of A
!  nb  =  principal quantum number of B
!  za  =  orbital exponent of A
!  zb  =  orbital exponent of B
!  r   =  distance between A and B
!
!  On exit :
!
!  gam      =  integral
!  dgam     =  first derivative of gam with respect to r
!  dza      =  first derivative of gam with respect to za
!  dzb      =  first derivative of gam with respect to zb
!  d2zaa    =  second derivative of gam with respect to za/za
!  d2zab    =  second derivative of gam with respect to za/zb
!  d2zbb    =  second derivative of gam with respect to zb/zb
!  d2zra    =  second derivative of gam with respect to r/za
!  d2zrb    =  second derivative of gam with respect to r/zb
!  d2gamr2  =  second derivative of gam with respect to r
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)              :: na
  integer(i4)              :: nb
  real(dp)                 :: dgam
  real(dp)                 :: dza
  real(dp)                 :: dzb
  real(dp)                 :: d2gamr2
  real(dp)                 :: d2zaa
  real(dp)                 :: d2zab
  real(dp)                 :: d2zbb
  real(dp)                 :: d2zra
  real(dp)                 :: d2zrb
  real(dp)                 :: gam
  real(dp)                 :: r
  real(dp)                 :: za
  real(dp)                 :: zb
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: na2
  integer(i4)              :: nb2
  real(dp)                 :: ctrm
  real(dp)                 :: d2rtrm
  real(dp)                 :: d2zeta11
  real(dp)                 :: d2zeta12
  real(dp)                 :: d2zeta1r
  real(dp)                 :: d2zeta22
  real(dp)                 :: d2zeta2r
  real(dp)                 :: deriv
  real(dp)                 :: deriv2
  real(dp)                 :: drtrm
  real(dp)                 :: dzeta1
  real(dp)                 :: dzeta2
  real(dp)                 :: factorial
  real(dp)                 :: halfr
  real(dp)                 :: rdza1
  real(dp)                 :: rfct1
  real(dp)                 :: rgam1
  real(dp)                 :: rgam2
  real(dp)                 :: rtrm
  real(dp)                 :: ss
  real(dp)                 :: trm1
  real(dp)                 :: trm2
  real(dp)                 :: trm3
  real(dp)                 :: z2ra
  real(dp)                 :: z2rb
  real(dp)                 :: ztrm
!
  gam = 0.0_dp
  dgam = 0.0_dp
  dza = 0.0_dp
  dzb = 0.0_dp
  d2zaa = 0.0_dp
  d2zab = 0.0_dp
  d2zbb = 0.0_dp
  d2zra = 0.0_dp
  d2zrb = 0.0_dp
  d2gamr2 = 0.0_dp
!
!  This routine only handles two centre integrals
!
  if (r.lt.1.0d-10) return
!
!  Create local variables
!
  z2ra = 2.0_dp*za*r
  z2rb = 2.0_dp*zb*r
  na2 = 2*na
  nb2 = 2*nb
  halfr = 0.5_dp*r
  d2rtrm = halfr**(na2-2)
  drtrm = d2rtrm*halfr
  rtrm = drtrm*halfr
!
!  First term
!
  call css(ss,na2-1_i4,0_i4,z2ra,0.0_dp,r,deriv,dzeta1,dzeta2, &
    d2zeta11,d2zeta12,d2zeta22,d2zeta1r,d2zeta2r,deriv2)
!
  gam = rtrm*ss
  dgam = rtrm*deriv + dble(na)*drtrm*ss
  dza = rtrm*dzeta1
  d2zaa = rtrm*d2zeta11
  d2zra = rtrm*d2zeta1r + dble(na)*drtrm*dzeta1
  d2gamr2 = d2gamr2 + 0.5_dp*dble(na*(na2-1))*d2rtrm*ss + 2.0_dp*dble(na)*drtrm*deriv + rtrm*deriv2
!
!  Sum over 2*nb
!
  rtrm = drtrm
  drtrm = d2rtrm
  ztrm = 0.5_dp/(zb*dble(nb2))
  do i = nb2,1,-1
    rtrm = rtrm*halfr
    drtrm = drtrm*halfr
    ztrm = ztrm*2.0_dp*zb
    ctrm = ztrm/factorial(nb2-i)
    call css(ss,na2-1_i4,nb2-i,z2ra,z2rb,r,deriv,dzeta1,dzeta2, &
      d2zeta11,d2zeta12,d2zeta22,d2zeta1r,d2zeta2r,deriv2)
    trm1 = dble(i)*ctrm
    trm2 = trm1*rtrm
    gam = gam - trm2*ss
    trm3 = trm1*dble(na2 + nb2-i)*drtrm
    dgam = dgam - trm2*deriv - 0.5_dp*trm3*ss
    d2gamr2 = d2gamr2 - trm2*deriv2 - trm3*deriv - 0.5_dp*trm3*dble(na2+nb2-i-1)*ss/r
    dza = dza - trm2*dzeta1
    dzb = dzb - (trm2/zb)*((dble(nb2-i))*ss+zb*dzeta2)
    d2zaa = d2zaa - trm2*d2zeta11
    d2zab = d2zab - (trm2/zb)*((dble(nb2-i))*dzeta1+zb*d2zeta12)
    d2zbb = d2zbb - (trm2/zb)*(2.0_dp*(dble(nb2-i))*dzeta2+zb*d2zeta22 + (dble((nb2-i-1)*(nb2-i))*ss/zb))
    d2zra = d2zra - trm2*d2zeta1r - 0.5_dp*trm3*dzeta1
    d2zrb = d2zrb - (trm2/zb)*((dble(nb2-i))*deriv+zb*d2zeta2r) - 0.5_dp*(trm3/zb)*((dble(nb2-i))*ss+zb*dzeta2)
  enddo
!
!  Multiply by coefficients
!
  trm3 = ((2.0_dp*za)**(na2+1))/factorial(na2)
  gam = gam*trm3
  dgam = dgam*trm3
  rfct1 = ((dble(na2+1))/za)
  rgam1 = rfct1*gam
  dza = dza*trm3
  rdza1 = 2.0_dp*rfct1*dza
  dza = dza + rgam1
  dzb = dzb*trm3
  rgam2 = rgam1*dble(na2)/za
  d2zaa = d2zaa*trm3 + rgam2 + rdza1
  d2zab = d2zab*trm3 + rfct1*dzb
  d2zbb = d2zbb*trm3
  d2zra = d2zra*trm3 + rfct1*dgam
  d2zrb = d2zrb*trm3
  d2gamr2 = d2gamr2*trm3
!
  return
  end
!*********************
!  Overlap integral  *
!*********************
  subroutine overlap(na,nb,za,zb,r,s,dsdr,d2sdr2)
!
!  Subroutine calculates the overlap integral over two s orbtials for Slater functions
!
!  On input :
!
!  na  =  principal quantum number of A
!  nb  =  principal quantum number of B
!  za  =  orbital exponent of A (in Ang**-1)
!  zb  =  orbital exponent of B (in Ang**-1)
!  r   =  distance between A and B (in Ang)
!
!  On exit :
!
!  s        = overlap integral
!  dsdr     = first derivative of overlap integral
!  d2sdr2   = second derivative of overlap integral
!
!   4/12 dsqrt replaced by sqrt
!
!  Julian Gale, NRI, Curtin University, April 2012
!
  use datatypes
  use numbers,     only : fct
  implicit none
!
!  Passed variables
!
  integer(i4)              :: na
  integer(i4)              :: nb
  real(dp)                 :: dsdr
  real(dp)                 :: d2sdr2
  real(dp)                 :: s
  real(dp)                 :: r
  real(dp)                 :: za
  real(dp)                 :: zb
!
!  Local variables
!
  integer(i4)              :: n11
  integer(i4)              :: n22
  real(dp)                 :: dzeta1
  real(dp)                 :: dzeta2
  real(dp)                 :: d2zeta11
  real(dp)                 :: d2zeta12
  real(dp)                 :: d2zeta22
  real(dp)                 :: d2zeta1r
  real(dp)                 :: d2zeta2r
  real(dp)                 :: z1r
  real(dp)                 :: z2r
!
  s = 0.0_dp
  dsdr = 0.0_dp
  d2sdr2 = 0.0_dp
!
!  This routine only handles two centre integrals
!
  if (r.lt.1.0d-10) return
!
!  Create local variables
!
  z1r = za*r
  z2r = zb*r
!
!  First term
!
  call css(s,na,nb,z1r,z2r,r,dsdr,dzeta1,dzeta2, &
    d2zeta11,d2zeta12,d2zeta22,d2zeta1r,d2zeta2r,d2sdr2)
!
!  Apply normalisation factor - AU conversion?????
!
  n11 = 2*na + 1
  n22 = 2*nb + 1
  s = s*sqrt((z1r**n11*z2r**n22)/(fct(n11)*fct(n22)))
!
  return
  end
!********
!  Css  *
!********
  subroutine css(s,nn1,nn2,alpha,beta,r,deriv,dzeta1,dzeta2, &
    d2zeta11,d2zeta12,d2zeta22,d2zeta1r,d2zeta2r,deriv2)
!
!  Modified integral calculation routine for Slater orbitals
!  including derivatives. This version is for S orbitals only.
!
!  dzeta1 and dzeta2 are the first derivatives with respect to zetas
!  and d2zeta11/d2zeta12/d2zeta22 are the second.
!  d2zeta1r and d2zeta2r are the mixed zeta/r second derivatives
!  deriv2 is the second derivative with respect to r
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use numbers
  implicit none
!
!  Passed variables
!
  integer(i4) :: nn1
  integer(i4) :: nn2
  real(dp)    :: alpha
  real(dp)    :: beta
  real(dp)    :: d2zeta11
  real(dp)    :: d2zeta12
  real(dp)    :: d2zeta22
  real(dp)    :: d2zeta1r
  real(dp)    :: d2zeta2r
  real(dp)    :: deriv
  real(dp)    :: deriv2
  real(dp)    :: dzeta1
  real(dp)    :: dzeta2
  real(dp)    :: r
  real(dp)    :: s
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: i1
  integer(i4) :: k
  integer(i4) :: n1
  integer(i4) :: n2
  integer(i4) :: nni1
  integer(i4) :: ulim
  real(dp)    :: a(30)
  real(dp)    :: b(30)
  real(dp)    :: coeffs
  real(dp)    :: coff
  real(dp)    :: da1(30)
  real(dp)    :: da2(30)
  real(dp)    :: dar(30)
  real(dp)    :: db1(30)
  real(dp)    :: db2(30)
  real(dp)    :: dbr(30)
  real(dp)    :: d2a11(30)
  real(dp)    :: d2a12(30)
  real(dp)    :: d2a22(30)
  real(dp)    :: d2b11(30)
  real(dp)    :: d2b12(30)
  real(dp)    :: d2b22(30)
  real(dp)    :: d2a1r(30)
  real(dp)    :: d2a2r(30)
  real(dp)    :: d2b1r(30)
  real(dp)    :: d2b2r(30)
  real(dp)    :: d2ar2(30)
  real(dp)    :: d2br2(30)
  real(dp)    :: d2pdz1r
  real(dp)    :: d2pdz2r
  real(dp)    :: d2ptdz1r
  real(dp)    :: d2ptdz2r
  real(dp)    :: difzeta
  real(dp)    :: dpdr
  real(dp)    :: dptdr
  real(dp)    :: dpdz1
  real(dp)    :: dpdz2
  real(dp)    :: dptdz1
  real(dp)    :: dptdz2
  real(dp)    :: factorial
  real(dp)    :: p
  real(dp)    :: pt
  real(dp)    :: sumzeta
  real(dp)    :: x
  real(dp)    :: zeta1
  real(dp)    :: zeta2
!
!  Set up factorials - stored as factorial(n) in location(n+1)
!
  do i = 1,30
    fct(i) = factorial(i-1_i4)
  enddo
  dzeta1 = 0.0_dp
  dzeta2 = 0.0_dp
  d2zeta11 = 0.0_dp
  d2zeta12 = 0.0_dp
  d2zeta22 = 0.0_dp
  d2zeta1r = 0.0_dp
  d2zeta2r = 0.0_dp
  deriv = 0.0_dp
  deriv2 = 0.0_dp
  n1 = nn1
  n2 = nn2
  p  = (alpha + beta)*0.5_dp
  pt = (alpha - beta)*0.5_dp
  x  =  0.0_dp
  zeta1 = alpha/r
  zeta2 = beta/r
  sumzeta = zeta1+zeta2
  difzeta = zeta1-zeta2
!
!  Partial derivative terms for zeta derivatives
!
  dpdz1 = r
  dpdz2 = r
  dptdz1 = r
  dptdz2 = -r
  dpdr = 0.5_dp*sumzeta
  dptdr = 0.5_dp*difzeta
  d2pdz1r = 1.0_dp
  d2pdz2r = 1.0_dp
  d2ptdz1r = 1.0_dp
  d2ptdz2r = - 1.0_dp
!
!  Reverse quantum numbers if necessary -
!  also change the sign of difzeta to match
!  change in sign of pt
!
  if (n2.lt.n1) then
    k  =  n1
    n1 =  n2
    n2 =  k
    pt = - pt
    difzeta = - difzeta
    dptdr = - dptdr
    dptdz1 = - dptdz1
    dptdz2 = - dptdz2
    d2ptdz1r = - d2ptdz1r
    d2ptdz2r = - d2ptdz2r
  endif
!
!  Trap for enormously long distances which would cause
!  caintgs or cbintgs to crash with an overflow
!
  if (p.gt.86.0.or.pt.gt.86.0) then
    s = 0.0_dp
    return
  endif
!***************************
!  Find a and b integrals  *
!***************************
  call caintgs(p,n1+n2+3_i4,a)
  call cbintgs(pt,n1+n2+3_i4,b)
!
!  Convert derivatives with respect to p and pt
!  into derivatives with respect to zeta1 and
!  zeta2
!
  ulim = n1+n2+1
  do i = 1,ulim
    da1(i) = - a(i+1)*dpdz1
    da2(i) = - a(i+1)*dpdz2
    db1(i) = - b(i+1)*dptdz1
    db2(i) = - b(i+1)*dptdz2
    d2a11(i) = a(i+2)*dpdz1*dpdz1
    d2a12(i) = a(i+2)*dpdz1*dpdz2
    d2a22(i) = a(i+2)*dpdz2*dpdz2
    d2b11(i) = b(i+2)*dptdz1*dptdz1
    d2b12(i) = b(i+2)*dptdz1*dptdz2
    d2b22(i) = b(i+2)*dptdz2*dptdz2
    dar(i) = - a(i+1)*dpdr
    dbr(i) = - b(i+1)*dptdr
    d2a1r(i) = a(i+2)*dpdz1*dpdr - a(i+1)*d2pdz1r
    d2a2r(i) = a(i+2)*dpdz2*dpdr - a(i+1)*d2pdz2r
    d2b1r(i) = b(i+2)*dptdz1*dptdr - b(i+1)*d2ptdz1r
    d2b2r(i) = b(i+2)*dptdz2*dptdr - b(i+1)*d2ptdz2r
    d2ar2(i) = a(i+2)*dpdr*dpdr
    d2br2(i) = b(i+2)*dptdr*dptdr
  enddo
!
!  Begin section used for overlap integrals involving s functions
!
  do i1 = 1,ulim
    nni1 = n1 + n2 - i1 + 2
    coff = coeffs(n1,n2,i1-1_i4)
    deriv = deriv + coff*(dar(i1)*b(nni1)+a(i1)*dbr(nni1))
    x = x + coff*a(i1)*b(nni1)
    dzeta1 = dzeta1 + coff*(da1(i1)*b(nni1)+a(i1)*db1(nni1))
    dzeta2 = dzeta2 + coff*(da2(i1)*b(nni1)+a(i1)*db2(nni1))
    d2zeta11 = d2zeta11 + coff*(d2a11(i1)*b(nni1) + a(i1)*d2b11(nni1) + 2.0_dp*da1(i1)*db1(nni1))
    d2zeta12 = d2zeta12 + coff*(d2a12(i1)*b(nni1) + a(i1)*d2b12(nni1) + da1(i1)*db2(nni1) + da2(i1)*db1(nni1))
    d2zeta22 = d2zeta22 + coff*(d2a22(i1)*b(nni1) + a(i1)*d2b22(nni1) + 2.0d0*da2(i1)*db2(nni1))
    d2zeta1r = d2zeta1r + coff*(d2a1r(i1)*b(nni1) + dar(i1)*db1(nni1) + da1(i1)*dbr(nni1) + a(i1)*d2b1r(nni1))
    d2zeta2r = d2zeta2r + coff*(d2a2r(i1)*b(nni1) + dar(i1)*db2(nni1) + da2(i1)*dbr(nni1) + a(i1)*d2b2r(nni1))
    deriv2 = deriv2 + coff*(d2ar2(i1)*b(nni1) + a(i1)*d2br2(nni1) + 2.0_dp*dar(i1)*dbr(nni1))
  enddo
  s = x*0.5_dp
  deriv = 0.5_dp*deriv
  deriv2 = 0.5_dp*deriv2
  dzeta1 = 0.5_dp*dzeta1
  dzeta2 = 0.5_dp*dzeta2
  d2zeta11 = 0.5_dp*d2zeta11
  d2zeta12 = 0.5_dp*d2zeta12
  d2zeta22 = 0.5_dp*d2zeta22
  d2zeta1r = 0.5_dp*d2zeta1r
  d2zeta2r = 0.5_dp*d2zeta2r
!
  return
  end
!***********
!  Coeffs  *
!***********
  function coeffs(na,nb,k)
  use numbers
  implicit none
!
!  Passed variables
!
  integer(i4)            :: k
  integer(i4)            :: na
  integer(i4)            :: nb
  real(dp)               :: coeffs
!
!  Local variables
!
  integer(i4)            :: binm
  integer(i4)            :: i
  integer(i4)            :: ia
  integer(i4)            :: ie
  integer(i4)            :: il
  integer(i4)            :: j
  integer(i4)            :: je
  integer(i4)            :: l
! Do not remove this statement even though compiler says value is unused!!
  integer(i4)            :: n
!
!  Statement function
!
  binm(n,i) = fct(n+1)/(fct(n-i+1)*fct(i+1))
!
  coeffs = 0.0_dp
  l = na + nb - k
  ie = min(l,na) + 1
  je = min(l,nb)
  ia = l - je + 1
  do il = ia,ie
    i = il - 1
    j = l - i
    coeffs = coeffs + dble(binm(na,i)*binm(nb,j))*(-1.0_dp)**j
  enddo
!
  return
  end
!************
!  Caintgs  *
!************
  subroutine caintgs(x,k,a)
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)        :: k
  real(dp)           :: a(30)
  real(dp)           :: x
!
!  Local variables
!
  integer(i4)        :: i
  real(dp)           :: const
  real(dp)           :: rx
!
  const = dexp(-x)
  rx = 1.0_dp/x
  a(1) = const*rx
  do i = 1,k
    a(i+1) = (a(i)*dble(i) + const)*rx
  enddo
!
  return
  end
!************
!  Cbintgs  *
!************
  subroutine cbintgs(x,k,b)
!*******************************************************************
! Fills array of b-integrals. note that b(i) is b(i-1) in the
! usual notation
! for x.gt.3			  exponential formula is used
! for 2.lt.x.le.3 and k.le.10   exponential formula is used
! for 2.lt.x.le.3 and k.gt.10   15 term series is used
! for 1.lt.x .e.2 and k.le.7    exponential formula is used
! for 1.lt.x.le.2 and k.gt.7    12 term series is used
! for .5.lt.x.le.1 and k.le.5   exponential formula is used
! for .5.lt.x.le.1 and k.gt.5    7 term series is used
! for x.le..5			  6 term series is used
!*******************************************************************
  use numbers
  implicit none
!
!  Passed variables
!
  integer(i4)        :: k
  real(dp)           :: b(30)
  real(dp)           :: x
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: i0
  integer(i4)        :: last
  integer(i4)        :: m
  real(dp)           :: absx
  real(dp)           :: expmx
  real(dp)           :: expx
  real(dp)           :: rx
  real(dp)           :: y
  real(dp)           :: ytrm
!
  i0 = 0
  absx = dabs(x)
  if (absx.gt.3.0_dp) goto 120
  if (absx.gt.2.0_dp) goto 20
  if (absx.gt.1.0_dp) goto 50
  if (absx.gt.0.5_dp) goto 80
  if (absx.ge.1.0d-8) goto 110
  goto 170
110 last = 6
  goto 140
80 if (k.le.5) goto 120
  last = 7
  goto 140
50 if (k.le.7) goto 120
  last = 12
  goto 140
20 if (k.le.10) goto 120
  last = 15
  goto 140

120 expx = exp(x)
  expmx = 1.0_dp/expx
  rx = 1.0_dp/x
  b(1) = (expx - expmx)*rx
  do i = 1,k
    b(i+1) = (dble(i)*b(i)+(-1.0_dp)**i*expx - expmx)*rx
  enddo
  go to 190
!
!  Series to calculate b(i)
!
140 do i = i0,k
    y = 0.0_dp
    do m = i0,last
      ytrm = (-x)**(m-1)*(1.0_dp - (-1.0_dp)**(m+i+1))/(fct(m+1)*dble(m+i+1))
      y = y + ytrm*(-x)
    enddo
    b(i+1) = y
  enddo
  go to 190
!
!  x extremely small
!
170 do i = i0,k
    b(i+1) = (1.0_dp-(-1.0_dp)**(i+1))/dble(i+1)
  enddo
190 continue
  return
  end
!**************
!  Factorial  *
!**************
  function factorial(n)
!
!  Calculates the factorial of an integer n
!
!  12/07 Unused variables removed
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)        :: n
  real(dp)           :: factorial
!
!  Local variables
!
  integer(i4)        :: i
!
  if (n.le.1) then
    factorial = 1.0_dp
    return
  endif
  factorial = 1.0_dp
  do i = 2,n
    factorial = factorial*dble(i)
  enddo
!
  return
  end
