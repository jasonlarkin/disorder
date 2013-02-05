  subroutine real1d3(nkp,i,j,nati,natj,d3,d3r,d3i,d3s,d3rs,d3is,xji,yji,zji)
!
!  Subroutine for third derivatives of 1-D electrostatic energy
!  for i-j pair. Called from realrecip3d3. 
!
!   5/02 Created from real1Dp
!   5/02 hfunc call modified for third derivatives
!  12/07 Unused variables removed
!   3/09 Explicit 1.0d-15 replaced by global value smallself from general module
!
!  Conditions of use:
!
!  GULP is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, March 2009
!
  use constants,    only : angstoev
  use current
  use element,      only : maxele
  use general,      only : nemorder, smallself
  use ksample,      only : xkpt
  use qmedata,      only : maxloop
  use shell,        only : cuts
  use symmetry,     only : lstr
  use times
  implicit none
!
!  Passed arguments
!
  integer(i4), intent(in)    :: nkp
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: nati
  integer(i4), intent(in)    :: natj
  real(dp),    intent(inout) :: d3(3,3,3)
  real(dp),    intent(inout) :: d3i(3,3,3)
  real(dp),    intent(inout) :: d3r(3,3,3)
  real(dp),    intent(inout) :: d3s(3,3,6)
  real(dp),    intent(inout) :: d3is(3,3,6)
  real(dp),    intent(inout) :: d3rs(3,3,6)
  real(dp),    intent(in)    :: xji
  real(dp),    intent(in)    :: yji
  real(dp),    intent(in)    :: zji
!
!  Local variables
!
  integer   :: m
  logical   :: lcspair
  real(dp)  :: acell
  real(dp)  :: cosk
  real(dp)  :: cputime
  real(dp)  :: cut2s
  real(dp)  :: d0
  real(dp)  :: d1
  real(dp)  :: d2
  real(dp)  :: dh1(3)
  real(dp)  :: dh2(3)
  real(dp)  :: dh1s
  real(dp)  :: dh2s
  real(dp)  :: d2h1(6)
  real(dp)  :: d2h2(6)
  real(dp)  :: d2h1m(3)
  real(dp)  :: d2h2m(3)
  real(dp)  :: d2h1s
  real(dp)  :: d2h2s
  real(dp)  :: d3h1(10)
  real(dp)  :: d3h2(10)
  real(dp)  :: d3h1m(6)
  real(dp)  :: d3h2m(6)
  real(dp)  :: d3k(3,3,3)
  real(dp)  :: d3ks(3,3)
  real(dp)  :: d3l
  real(dp)  :: e1
  real(dp)  :: e2
  real(dp)  :: h1
  real(dp)  :: h2
  real(dp)  :: oci     
  real(dp)  :: ocj 
  real(dp)  :: qi  
  real(dp)  :: qj
  real(dp)  :: qij
  real(dp)  :: r
  real(dp)  :: rcut
  real(dp)  :: rr
  real(dp)  :: sink
  real(dp)  :: t1, t2
  real(dp)  :: u
  real(dp)  :: xkv
  real(dp)  :: x
  real(dp)  :: y
  real(dp)  :: z
!
  t1 = cputime()
!
!  Set up local variables
!
  cut2s = cuts*cuts
  oci = occuf(i)
  qi = qf(i)*oci
  ocj = occuf(j)
  qj = qf(j)*ocj
  qij = qi*qj
  lcspair = (abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001d0)
  if (lcspair) then
    rcut = cut2s
  else
    rcut = smallself
  endif
  y = yji
  z = zji
!  
!  Calculate cartesian K vector component
!
  xkv = xkpt(nkp)*kv(1,1)
!
!  Loop over number of cells in sum
!
  do m = -maxloop(1),maxloop(1)
!
!  Direct sum component over neutral cells
!
    acell = dble(m)*a
    x = acell + xji
    r = x*x + y*y + z*z
    if (r.gt.rcut) then
      cosk = xkv*x
      sink = sin(cosk)
      cosk = cos(cosk)
      r = sqrt(r)
      rr = 1.0_dp/r
      d0 = qij*angstoev*rr
      d1 = d0*rr*rr
      d2 = 3.0_dp*d1*rr*rr
      d3l = - 5.0_dp*d2*rr*rr
!
!  Calculate third derivative matrix - first term
!
      d3k(1,1,1) = x*x*x*d3l + 3.0_dp*x*d2
      d3k(2,1,1) = x*y*x*d3l + 2.0_dp*y*d2
      d3k(3,1,1) = x*z*x*d3l + 2.0_dp*z*d2
      d3k(2,2,1) = y*y*x*d3l + 2.0_dp*x*d2
      d3k(3,2,1) = y*z*x*d3l
      d3k(3,3,1) = z*z*x*d3l + 2.0_dp*x*d2
      d3k(2,2,2) = y*y*y*d3l + 3.0_dp*y*d2
      d3k(3,2,2) = y*z*y*d3l + 2.0_dp*z*d2
      d3k(3,3,2) = z*z*y*d3l + 2.0_dp*y*d2
      d3k(3,3,3) = z*z*z*d3l + 3.0_dp*z*d2
!
!  Strain derivatives
!
      if (lstr) then
        d3ks(1,1) = d3k(1,1,1)*x
        d3ks(2,1) = d3k(2,1,1)*x
        d3ks(3,1) = d3k(3,1,1)*x
        d3ks(2,2) = d3k(2,2,1)*x
        d3ks(3,2) = d3k(3,2,1)*x
        d3ks(3,3) = d3k(3,3,1)*x
      endif
!
!  Calculate real component of third derivative matrix
!  summed with unphased component for diagonal blocks
!
      d3(1,1,1) = d3(1,1,1) + d3k(1,1,1)
      d3(2,1,1) = d3(2,1,1) + d3k(2,1,1)
      d3(3,1,1) = d3(3,1,1) + d3k(3,1,1)
      d3(2,2,1) = d3(2,2,1) + d3k(2,2,1)
      d3(3,2,1) = d3(3,2,1) + d3k(3,2,1)
      d3(3,3,1) = d3(3,3,1) + d3k(3,3,1)
      d3(2,2,2) = d3(2,2,2) + d3k(2,2,2)
      d3(3,2,2) = d3(3,2,2) + d3k(3,2,2)
      d3(3,3,2) = d3(3,3,2) + d3k(3,3,2)
      d3(3,3,3) = d3(3,3,3) + d3k(3,3,3)
      d3r(1,1,1) = d3r(1,1,1) + d3k(1,1,1)*cosk
      d3r(2,1,1) = d3r(2,1,1) + d3k(2,1,1)*cosk
      d3r(3,1,1) = d3r(3,1,1) + d3k(3,1,1)*cosk
      d3r(2,2,1) = d3r(2,2,1) + d3k(2,2,1)*cosk
      d3r(3,2,1) = d3r(3,2,1) + d3k(3,2,1)*cosk
      d3r(3,3,1) = d3r(3,3,1) + d3k(3,3,1)*cosk
      d3r(2,2,2) = d3r(2,2,2) + d3k(2,2,2)*cosk
      d3r(3,2,2) = d3r(3,2,2) + d3k(3,2,2)*cosk
      d3r(3,3,2) = d3r(3,3,2) + d3k(3,3,2)*cosk
      d3r(3,3,3) = d3r(3,3,3) + d3k(3,3,3)*cosk
      d3i(1,1,1) = d3i(1,1,1) + d3k(1,1,1)*sink
      d3i(2,1,1) = d3i(2,1,1) + d3k(2,1,1)*sink
      d3i(3,1,1) = d3i(3,1,1) + d3k(3,1,1)*sink
      d3i(2,2,1) = d3i(2,2,1) + d3k(2,2,1)*sink
      d3i(3,2,1) = d3i(3,2,1) + d3k(3,2,1)*sink
      d3i(3,3,1) = d3i(3,3,1) + d3k(3,3,1)*sink
      d3i(2,2,2) = d3i(2,2,2) + d3k(2,2,2)*sink
      d3i(3,2,2) = d3i(3,2,2) + d3k(3,2,2)*sink
      d3i(3,3,2) = d3i(3,3,2) + d3k(3,3,2)*sink
      d3i(3,3,3) = d3i(3,3,3) + d3k(3,3,3)*sink
      if (lstr) then
        d3s(1,1,1) = d3s(1,1,1) + d3ks(1,1)
        d3s(2,1,1) = d3s(2,1,1) + d3ks(2,1)
        d3s(3,1,1) = d3s(3,1,1) + d3ks(3,1)
        d3s(2,2,1) = d3s(2,2,1) + d3ks(2,2)
        d3s(3,2,1) = d3s(3,2,1) + d3ks(3,2)
        d3s(3,3,1) = d3s(3,3,1) + d3ks(3,3)
        d3rs(1,1,1) = d3rs(1,1,1) + d3ks(1,1)*cosk
        d3rs(2,1,1) = d3rs(2,1,1) + d3ks(2,1)*cosk
        d3rs(3,1,1) = d3rs(3,1,1) + d3ks(3,1)*cosk
        d3rs(2,2,1) = d3rs(2,2,1) + d3ks(2,2)*cosk
        d3rs(3,2,1) = d3rs(3,2,1) + d3ks(3,2)*cosk
        d3rs(3,3,1) = d3rs(3,3,1) + d3ks(3,3)*cosk
        d3is(1,1,1) = d3is(1,1,1) + d3ks(1,1)*sink
        d3is(2,1,1) = d3is(2,1,1) + d3ks(2,1)*sink
        d3is(3,1,1) = d3is(3,1,1) + d3ks(3,1)*sink
        d3is(2,2,1) = d3is(2,2,1) + d3ks(2,2)*sink
        d3is(3,2,1) = d3is(3,2,1) + d3ks(3,2)*sink
        d3is(3,3,1) = d3is(3,3,1) + d3ks(3,3)*sink
      endif
    endif
  enddo
!
!  Neutralising terms
!
!  Background
!
!  and
!
!  Euler-MacLaurin component
!
  if (maxloop(1).gt.0) then
    u = (dble(maxloop(1))+0.5_dp)*a
    x = xji
!
!  Phase factor
!
    cosk = xkv*x
    sink = sin(cosk)
    cosk = cos(cosk)
!
!  H term
!
    call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,.true.,.true.,.true.)
    call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h2,.true.,.true.,.true.)
    d2 = qij*angstoev/a
!
    d3k(1,1,1) = - d2*(d3h1(1) + d3h2(1))
    d3k(2,1,1) = - d2*(d3h1(2) + d3h2(2))
    d3k(3,1,1) = - d2*(d3h1(3) + d3h2(3))
    d3k(2,2,1) = - d2*(d3h1(4) + d3h2(4))
    d3k(3,2,1) = - d2*(d3h1(5) + d3h2(5))
    d3k(3,3,1) = - d2*(d3h1(6) + d3h2(6))
    d3k(2,2,2) = - d2*(d3h1(7) + d3h2(7))
    d3k(3,2,2) = - d2*(d3h1(8) + d3h2(8))
    d3k(3,3,2) = - d2*(d3h1(9) + d3h2(9))
    d3k(3,3,3) = - d2*(d3h1(10)+ d3h2(10))
    if (lstr) then
!
!  Need to allow for +2*xdrv term
!
      d3ks(1,1) = - d2*(d3h1(1)*(u+x) - d3h2(1)*(u-x) - d2h1(1) - d2h2(1))
      d3ks(2,1) = - d2*(d3h1(2)*(u+x) - d3h2(2)*(u-x) - (d2h1(6) + d2h2(6)) - d2h1(6) - d2h2(6))
      d3ks(3,1) = - d2*(d3h1(3)*(u+x) - d3h2(3)*(u-x) - (d2h1(5) + d2h2(5)) - d2h1(5) - d2h2(5))
      d3ks(2,2) = - d2*(d3h1(4)*(u+x) - d3h2(4)*(u-x) - d2h1(2) - d2h2(2))
      d3ks(3,2) = - d2*(d3h1(5)*(u+x) - d3h2(5)*(u-x) - d2h1(4) - d2h2(4))
      d3ks(3,3) = - d2*(d3h1(6)*(u+x) - d3h2(6)*(u-x) - d2h1(3) - d2h2(3))
    endif
!
!  E-M term
!
    call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s,d2h1s,d2h1m,d3h1,d3h1m,.true.,.true.,.true.)
    call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s,d2h2s,d2h2m,d3h2,d3h2m,.true.,.true.,.true.)
    d2 = qij*angstoev
    d3k(1,1,1) = d3k(1,1,1) + d2*(d3h1(1) + d3h2(1))
    d3k(2,1,1) = d3k(2,1,1) + d2*(d3h1(2) + d3h2(2))
    d3k(3,1,1) = d3k(3,1,1) + d2*(d3h1(3) + d3h2(3))
    d3k(2,2,1) = d3k(2,2,1) + d2*(d3h1(4) + d3h2(4))
    d3k(3,2,1) = d3k(3,2,1) + d2*(d3h1(5) + d3h2(5))
    d3k(3,3,1) = d3k(3,3,1) + d2*(d3h1(6) + d3h2(6))
    d3k(2,2,2) = d3k(2,2,2) + d2*(d3h1(7) + d3h2(7))
    d3k(3,2,2) = d3k(3,2,2) + d2*(d3h1(8) + d3h2(8))
    d3k(3,3,2) = d3k(3,3,2) + d2*(d3h1(9) + d3h2(9))
    d3k(3,3,3) = d3k(3,3,3) + d2*(d3h1(10)+ d3h2(10))
    if (lstr) then
      d3ks(1,1) = d3ks(1,1) + d2*(d3h1m(1) + d3h2m(1))
      d3ks(2,1) = d3ks(2,1) + d2*(d3h1m(2) + d3h2m(2))
      d3ks(3,1) = d3ks(3,1) + d2*(d3h1m(3) + d3h2m(3))
      d3ks(2,2) = d3ks(2,2) + d2*(d3h1m(4) + d3h2m(4))
      d3ks(3,2) = d3ks(3,2) + d2*(d3h1m(5) + d3h2m(5))
      d3ks(3,3) = d3ks(3,3) + d2*(d3h1m(6) + d3h2m(6))
    endif
  endif
!
!  Calculate real component of third derivative matrix
!  summed with unphased component for diagonal blocks
!
  d3(1,1,1) = d3(1,1,1) + d3k(1,1,1)
  d3(2,1,1) = d3(2,1,1) + d3k(2,1,1)
  d3(3,1,1) = d3(3,1,1) + d3k(3,1,1)
  d3(2,2,1) = d3(2,2,1) + d3k(2,2,1)
  d3(3,2,1) = d3(3,2,1) + d3k(3,2,1)
  d3(3,3,1) = d3(3,3,1) + d3k(3,3,1)
  d3(2,2,2) = d3(2,2,2) + d3k(2,2,2)
  d3(3,2,2) = d3(3,2,2) + d3k(3,2,2)
  d3(3,3,2) = d3(3,3,2) + d3k(3,3,2)
  d3(3,3,3) = d3(3,3,3) + d3k(3,3,3)
  d3r(1,1,1) = d3r(1,1,1) + d3k(1,1,1)*cosk
  d3r(2,1,1) = d3r(2,1,1) + d3k(2,1,1)*cosk
  d3r(3,1,1) = d3r(3,1,1) + d3k(3,1,1)*cosk
  d3r(2,2,1) = d3r(2,2,1) + d3k(2,2,1)*cosk
  d3r(3,2,1) = d3r(3,2,1) + d3k(3,2,1)*cosk
  d3r(3,3,1) = d3r(3,3,1) + d3k(3,3,1)*cosk
  d3r(2,2,2) = d3r(2,2,2) + d3k(2,2,2)*cosk
  d3r(3,2,2) = d3r(3,2,2) + d3k(3,2,2)*cosk
  d3r(3,3,2) = d3r(3,3,2) + d3k(3,3,2)*cosk
  d3r(3,3,3) = d3r(3,3,3) + d3k(3,3,3)*cosk
  d3i(1,1,1) = d3i(1,1,1) + d3k(1,1,1)*sink
  d3i(2,1,1) = d3i(2,1,1) + d3k(2,1,1)*sink
  d3i(3,1,1) = d3i(3,1,1) + d3k(3,1,1)*sink
  d3i(2,2,1) = d3i(2,2,1) + d3k(2,2,1)*sink
  d3i(3,2,1) = d3i(3,2,1) + d3k(3,2,1)*sink
  d3i(3,3,1) = d3i(3,3,1) + d3k(3,3,1)*sink
  d3i(2,2,2) = d3i(2,2,2) + d3k(2,2,2)*sink
  d3i(3,2,2) = d3i(3,2,2) + d3k(3,2,2)*sink
  d3i(3,3,2) = d3i(3,3,2) + d3k(3,3,2)*sink
  d3i(3,3,3) = d3i(3,3,3) + d3k(3,3,3)*sink
  if (lstr) then
    d3s(1,1,1) = d3s(1,1,1) + d3ks(1,1)
    d3s(2,1,1) = d3s(2,1,1) + d3ks(2,1)
    d3s(3,1,1) = d3s(3,1,1) + d3ks(3,1)
    d3s(2,2,1) = d3s(2,2,1) + d3ks(2,2)
    d3s(3,2,1) = d3s(3,2,1) + d3ks(3,2)
    d3s(3,3,1) = d3s(3,3,1) + d3ks(3,3)
    d3rs(1,1,1) = d3rs(1,1,1) + d3ks(1,1)*cosk
    d3rs(2,1,1) = d3rs(2,1,1) + d3ks(2,1)*cosk
    d3rs(3,1,1) = d3rs(3,1,1) + d3ks(3,1)*cosk
    d3rs(2,2,1) = d3rs(2,2,1) + d3ks(2,2)*cosk
    d3rs(3,2,1) = d3rs(3,2,1) + d3ks(3,2)*cosk
    d3rs(3,3,1) = d3rs(3,3,1) + d3ks(3,3)*cosk
    d3is(1,1,1) = d3is(1,1,1) + d3ks(1,1)*sink
    d3is(2,1,1) = d3is(2,1,1) + d3ks(2,1)*sink
    d3is(3,1,1) = d3is(3,1,1) + d3ks(3,1)*sink
    d3is(2,2,1) = d3is(2,2,1) + d3ks(2,2)*sink
    d3is(3,2,1) = d3is(3,2,1) + d3ks(3,2)*sink
    d3is(3,3,1) = d3is(3,3,1) + d3ks(3,3)*sink
  endif
!
  t2 = cputime()
  tatom = tatom + t2 - t1
!
  return
  end
