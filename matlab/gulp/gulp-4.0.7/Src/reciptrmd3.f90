  subroutine reciptrmd3(i,j,nkp,c6tot,lstr,nkvec0,ktrm0,ktrm20, &
    ktrm6,ktrm60,ktrm62,ktrm620,d3,d3r,d3i,d3s,d3rs,d3is)
!
!  Calculates the reciprocal space contribution to the third
!  derivatives when called from realrecip3d3
!
!  nkp    = k point to be calculated
!
!  The use of right angled saving must be disabled in kindex when
!  calling this routine as the k point breaks the mirror plane
!  symmetry.
!
!   9/97 Created from recip3d3
!   3/01 Modified for 2-D case
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
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use constants
  use current
  use ksample
  use kspace
  implicit none
!
!  Passed variables
!
  integer(i4)        :: i
  integer(i4)        :: j
  integer(i4)        :: nkp
  integer(i4)        :: nkvec0
  logical            :: lstr
  real(dp)           :: c6tot
  real(dp)           :: ktrm6(*)
  real(dp)           :: ktrm62(*)
  real(dp)           :: ktrm0(*)
  real(dp)           :: ktrm20(*)
  real(dp)           :: ktrm60(*)
  real(dp)           :: ktrm620(*)
  real(dp)           :: d3(3,3,3)
  real(dp)           :: d3i(3,3,3)
  real(dp)           :: d3r(3,3,3)
  real(dp)           :: d3is(3,3,6)
  real(dp)           :: d3rs(3,3,6)
  real(dp)           :: d3s(3,3,6)
!
!  Local variables
!
  integer(i4)        :: kk
  integer(i4)        :: iv
  logical            :: ldoc6
  logical            :: lgamma
  real(dp)           :: arg
  real(dp)           :: cosa
  real(dp)           :: cosq
  real(dp)           :: cosr
  real(dp)           :: coss
  real(dp)           :: cossz
  real(dp)           :: cosszz
  real(dp)           :: d11m22
  real(dp)           :: d13m24
  real(dp)           :: d13p24
  real(dp)           :: d3t1dz3
  real(dp)           :: darg1
  real(dp)           :: darg2
  real(dp)           :: derfc
  real(dp)           :: derfc1
  real(dp)           :: derfc2
  real(dp)           :: dexp1
  real(dp)           :: dexp2
  real(dp)           :: dexp3
  real(dp)           :: dexp4
  real(dp)           :: dexpz
  real(dp)           :: dt1
  real(dp)           :: dtrm2
  real(dp)           :: etaz
  real(dp)           :: etaz2
  real(dp)           :: kexperfc
  real(dp)           :: kvec
  real(dp)           :: oci
  real(dp)           :: ocj
  real(dp)           :: qfct
  real(dp)           :: qli
  real(dp)           :: qlj
  real(dp)           :: rk1x
  real(dp)           :: rk1y
  real(dp)           :: rk1z
  real(dp)           :: rk2x
  real(dp)           :: rk2y
  real(dp)           :: rk2z
  real(dp)           :: rk3x
  real(dp)           :: rk3y
  real(dp)           :: rk3z
  real(dp)           :: rkvec
  real(dp)           :: sina
  real(dp)           :: sinq
  real(dp)           :: sinr
  real(dp)           :: sins
  real(dp)           :: sinsz
  real(dp)           :: sinszz
  real(dp)           :: sktrm1
  real(dp)           :: sktrm2
  real(dp)           :: strm1
  real(dp)           :: sztrm2
  real(dp)           :: sztrm3
  real(dp)           :: tmp1
  real(dp)           :: tmp2
  real(dp)           :: tmp3
  real(dp)           :: tmp4
  real(dp)           :: tmp5
  real(dp)           :: tmp6
  real(dp)           :: trmzz
  real(dp)           :: trmzzz
  real(dp)           :: xd
  real(dp)           :: yd
  real(dp)           :: zd
  real(dp)           :: xk
  real(dp)           :: yk
  real(dp)           :: zk
  real(dp)           :: xkv
  real(dp)           :: ykv
  real(dp)           :: zkv
  real(dp)           :: xrkk
  real(dp)           :: yrkk
  real(dp)           :: zrkk
  real(dp)           :: ztrm1
!
!  Assign local variables
!
  rk1x = kv(1,1)
  rk1y = kv(2,1)
  rk1z = kv(3,1)
  rk2x = kv(1,2)
  rk2y = kv(2,2)
  rk2z = kv(3,2)
  rk3x = kv(1,3)
  rk3y = kv(2,3)
  rk3z = kv(3,3)
!
!  Calculate basic k vector
!
  if (ndim.eq.3) then
    xk = xkpt(nkp)
    yk = ykpt(nkp)
    zk = zkpt(nkp)
    lgamma = ((abs(xk)+abs(yk)+abs(zk)).lt.1.0d-8)
    xkv = xk*rk1x + yk*rk2x + zk*rk3x
    ykv = xk*rk1y + yk*rk2y + zk*rk3y
    zkv = xk*rk1z + yk*rk2z + zk*rk3z
  elseif (ndim.eq.2) then
    xk = xkpt(nkp)
    yk = ykpt(nkp)
    zk = 0.0_dp
    lgamma = ((abs(xk)+abs(yk)).lt.1.0d-8)
    xkv = xk*rk1x + yk*rk2x
    ykv = xk*rk1y + yk*rk2y
    zkv = 0.0_dp
  elseif (ndim.eq.1) then
    xk = xkpt(nkp)
    yk = 0.0_dp
    zk = 0.0_dp
    lgamma = (abs(xk).lt.1.0d-8)
    xkv = xk*rk1x
    ykv = 0.0_dp
    zkv = 0.0_dp
  endif
!************************************************
!  Algorithm for cases where dispersion cannot  *
!  be factorised into one centre terms          *
!************************************************
  oci = occuf(i)
  qli = qf(i)*oci
  ocj = occuf(j)
  qlj = qf(j)*ocj
  qfct = qli*qlj
  c6tot = c6tot*oci*ocj
  ldoc6 = (abs(c6tot).gt.1.0d-4)
!
!  Find relative vector between atoms
!
  xd = xclat(j) - xclat(i)
  yd = yclat(j) - yclat(i)
  zd = zclat(j) - zclat(i)
!
!  Define K vector independent constants and T1 surface derivatives
!
  if (ndim.eq.2) then
!
!  Define constants
!
    rpieta = 1.0_dp / sqrt(pi * eta)
    rhseta = 0.5_dp / seta
!
!  Evaluate K -> 0 limit of 2-D sum
!
    etaz = seta*zd
    etaz2 = etaz * etaz
    dexpz = exp(-etaz2)*angstoev
    dtrm2 = - qfct*vol4pi*tweatpi*dexpz
    d3t1dz3 = 2.0_dp*qfct*vol4pi*tweatpi*dexpz*eta*zd
    d3s(3,3,1) = d3s(3,3,1) - dtrm2
    d3s(3,3,2) = d3s(3,3,2) - dtrm2
    d3(3,3,3)  = d3(3,3,3)  + d3t1dz3
    if (lgamma) then
      d3rs(3,3,1) = d3rs(3,3,1) - dtrm2
      d3rs(3,3,2) = d3rs(3,3,2) - dtrm2
      d3r(3,3,3)  = d3r(3,3,3)  + d3t1dz3
    else
      if (i.eq.j) then
        d3r(3,3,3) = d3r(3,3,3) - d3t1dz3
      endif
    endif
  endif
!********************************
!  Calculate third derivatives  *
!********************************
  do iv = 1,nkvec
    xrkk = xrk(iv)
    yrkk = yrk(iv)
    zrkk = zrk(iv)
    if (ndim.eq.3) then
!
!  3-D
!
      arg = (xrkk-xkv)*xd + (yrkk-ykv)*yd + (zrkk-zkv)*zd
      if (ldoc6) then
        cosa = cos(arg)
        sina = sin(arg)
        sktrm1 = (ktrm(iv)*qfct - ktrm6(iv)*c6tot)
        sktrm2 = (ktrms(iv)*qfct - ktrm62(iv)*c6tot)
        cosq = cosa*sktrm1
        sinq = sina*sktrm1
        coss = cosa*sktrm2
        sins = sina*sktrm2
      else
        cosa = cos(arg)*qfct
        sina = sin(arg)*qfct
        cosq = cosa*ktrm(iv)
        sinq = sina*ktrm(iv)
        coss = cosa*ktrms(iv)
        sins = sina*ktrms(iv)
      endif
    elseif (ndim.eq.2) then
!
!  2-D
!
      arg = (xrkk-xkv)*xd + (yrkk-ykv)*yd
      cosa = cos(arg)*qfct
      sina = sin(arg)*qfct
!           
      kvec = kmod(iv)
      dexp1 = exp(kvec*zd)
      dexp2 = 1.0_dp/dexp1
      darg1 = kvec*rhseta + etaz
      darg2 = kvec*rhseta - etaz
      dexp3 = exp(-(darg1)**2)
      dexp4 = exp(-(darg2)**2)
      derfc1 = derfc(darg1)
      derfc2 = derfc(darg2)
      d11m22 = dexp1*derfc1 - dexp2*derfc2
      d13m24 = dexp1*dexp3 - dexp2*dexp4
      d13p24 = dexp1*dexp3 + dexp2*dexp4
      kexperfc = dexp1*derfc1 + dexp2*derfc2
      ztrm1 = kvec*d11m22 - tweatpi*d13m24
      trmzz = (kvec*kvec*kexperfc - 2.0_dp*tweatpi*(kvec*d13p24 -  &
               seta*(darg1*dexp1*dexp3 + darg2*dexp2*dexp4)))
      trmzzz = kvec*kvec*kvec*d11m22 - tweatpi*(3.0_dp*kvec*kvec*d13m24  &
      - 6.0_dp*kvec*seta*(dexp1*darg1*dexp3-dexp2*darg2*dexp4) &
      + 4.0*eta*(dexp1*darg1*darg1*dexp3-dexp2*darg2*darg2*dexp4) &
      - 2.0_dp*eta*d13m24)
!            
      cosr = cosa*ktrm(iv)
      sinr = sina*ktrm(iv)
      cosq = cosr*kexperfc
      sinq = sinr*kexperfc
      if (lstr) then
        rkvec = 1.0_dp/kvec
        strm1 = rkvec*(-rkvec*kexperfc + zd*d11m22 - rpieta*d13p24)
        sztrm2 = rkvec*(zd*kvec*kexperfc + tweatpi*rkvec*d13m24)
        sztrm3 = rkvec*(kvec*kexperfc + kvec*kvec*zd*d11m22 - kvec*zd*tweatpi*d13m24 + &
          tweatpi*d13p24 - 2.0_dp*tweatpi*seta*rkvec*(darg1*dexp1*dexp3 + darg2*dexp2*dexp4))
        coss = cosr*strm1
        sins = sinr*strm1
        cossz = cosr*sztrm2
        sinsz = sinr*sztrm2
        cosszz = cosr*sztrm3
        sinszz = sinr*sztrm3
      endif
    endif
!
    tmp1 = xrkk*xrkk
    tmp2 = yrkk*yrkk
    tmp3 = zrkk*zrkk
    tmp4 = yrkk*zrkk
    tmp5 = xrkk*zrkk
    tmp6 = xrkk*yrkk
!
! Calculate real and imaginary third derivatives
!
! no z component
!
    d3r(1,1,1) = d3r(1,1,1) + sinq*tmp1*xrkk
    d3r(2,1,1) = d3r(2,1,1) + sinq*tmp6*xrkk
    d3r(2,2,1) = d3r(2,2,1) + sinq*tmp2*xrkk
    d3r(2,2,2) = d3r(2,2,2) + sinq*tmp2*yrkk
    d3i(1,1,1) = d3i(1,1,1) + cosq*tmp1*xrkk
    d3i(2,1,1) = d3i(2,1,1) + cosq*tmp6*xrkk
    d3i(2,2,1) = d3i(2,2,1) + cosq*tmp2*xrkk
    d3i(2,2,2) = d3i(2,2,2) + cosq*tmp2*yrkk
    if (ndim.eq.3) then
      d3r(3,1,1) = d3r(3,1,1) + sinq*tmp5*xrkk
      d3r(3,2,1) = d3r(3,2,1) + sinq*tmp4*xrkk
      d3r(3,3,1) = d3r(3,3,1) + sinq*tmp3*xrkk
      d3r(3,2,2) = d3r(3,2,2) + sinq*tmp4*yrkk
      d3r(3,3,2) = d3r(3,3,2) + sinq*tmp3*yrkk
      d3r(3,3,3) = d3r(3,3,3) + sinq*tmp3*zrkk
      d3i(3,1,1) = d3i(3,1,1) + cosq*tmp5*xrkk
      d3i(3,2,1) = d3i(3,2,1) + cosq*tmp4*xrkk
      d3i(3,3,1) = d3i(3,3,1) + cosq*tmp3*xrkk
      d3i(3,2,2) = d3i(3,2,2) + cosq*tmp4*yrkk
      d3i(3,3,2) = d3i(3,3,2) + cosq*tmp3*yrkk
      d3i(3,3,3) = d3i(3,3,3) + cosq*tmp3*zrkk
    elseif (ndim.eq.2) then
! 1 z component
      d3r(3,1,1) = d3r(3,1,1) - cosr*tmp1*ztrm1
      d3r(3,2,1) = d3r(3,2,1) - cosr*tmp6*ztrm1
      d3r(3,2,2) = d3r(3,2,2) - cosr*tmp2*ztrm1
      d3i(3,1,1) = d3i(3,1,1) + sinr*tmp1*ztrm1
      d3i(3,2,1) = d3i(3,2,1) + sinr*tmp6*ztrm1
      d3i(3,2,2) = d3i(3,2,2) + sinr*tmp2*ztrm1
! 2 z components
      d3r(3,3,1) = d3r(3,3,1) - sinr*trmzz*xrkk
      d3r(3,3,2) = d3r(3,3,2) - sinr*trmzz*yrkk
      d3i(3,3,1) = d3i(3,3,1) - cosr*trmzz*xrkk
      d3i(3,3,2) = d3i(3,3,2) - cosr*trmzz*yrkk
! 3 z components
      d3r(3,3,3) = d3r(3,3,3) + cosr*trmzzz
      d3i(3,3,3) = d3i(3,3,3) - sinr*trmzzz
    endif
    if (lstr) then
      d3rs(1,1,1) = d3rs(1,1,1) + coss*tmp1*tmp1 + 3.0d0*cosq*tmp1
      d3rs(2,1,1) = d3rs(2,1,1) + coss*tmp6*tmp1 + 2.0d0*cosq*tmp6
      d3rs(2,2,1) = d3rs(2,2,1) + coss*tmp2*tmp1 + cosq*tmp2
      d3rs(1,1,2) = d3rs(1,1,2) + coss*tmp1*tmp2 + cosq*tmp1
      d3rs(2,1,2) = d3rs(2,1,2) + coss*tmp6*tmp2 + 2.0d0*cosq*tmp6
      d3rs(2,2,2) = d3rs(2,2,2) + coss*tmp2*tmp2 + 3.0d0*cosq*tmp2
!
      d3is(1,1,1) = d3is(1,1,1) - sins*tmp1*tmp1 - 3.0d0*sinq*tmp1
      d3is(2,1,1) = d3is(2,1,1) - sins*tmp6*tmp1 - 2.0d0*sinq*tmp6
      d3is(2,2,1) = d3is(2,2,1) - sins*tmp2*tmp1 - sinq*tmp2
      d3is(1,1,2) = d3is(1,1,2) - sins*tmp1*tmp2 - sinq*tmp1
      d3is(2,1,2) = d3is(2,1,2) - sins*tmp6*tmp2 - 2.0d0*sinq*tmp6
      d3is(2,2,2) = d3is(2,2,2) - sins*tmp2*tmp2 - 3.0d0*sinq*tmp2
!
      if (ndim.eq.3) then
        d3rs(3,1,1) = d3rs(3,1,1) + coss*tmp5*tmp1 + 2.0d0*cosq*tmp5
        d3rs(3,2,1) = d3rs(3,2,1) + coss*tmp4*tmp1 + cosq*tmp4
        d3rs(3,3,1) = d3rs(3,3,1) + coss*tmp3*tmp1 + cosq*tmp3
        d3rs(3,1,2) = d3rs(3,1,2) + coss*tmp5*tmp2 + cosq*tmp5
        d3rs(3,2,2) = d3rs(3,2,2) + coss*tmp4*tmp2 + 2.0d0*cosq*tmp4
        d3rs(3,3,2) = d3rs(3,3,2) + coss*tmp3*tmp2 + cosq*tmp3
        d3rs(1,1,3) = d3rs(1,1,3) + coss*tmp1*tmp3 + cosq*tmp1
        d3rs(2,1,3) = d3rs(2,1,3) + coss*tmp6*tmp3 + cosq*tmp6
        d3rs(2,2,3) = d3rs(2,2,3) + coss*tmp2*tmp3 + cosq*tmp2
        d3rs(3,1,3) = d3rs(3,1,3) + coss*tmp5*tmp3 + 2.0d0*cosq*tmp5
        d3rs(3,2,3) = d3rs(3,2,3) + coss*tmp4*tmp3 + 2.0d0*cosq*tmp4
        d3rs(3,3,3) = d3rs(3,3,3) + coss*tmp3*tmp3 + 3.0d0*cosq*tmp3
        d3rs(1,1,4) = d3rs(1,1,4) + coss*tmp1*tmp4
        d3rs(2,1,4) = d3rs(2,1,4) + coss*tmp6*tmp4 + 0.5d0*cosq*tmp5
        d3rs(2,2,4) = d3rs(2,2,4) + coss*tmp2*tmp4 + cosq*tmp4
        d3rs(3,1,4) = d3rs(3,1,4) + coss*tmp5*tmp4 + 0.5d0*cosq*tmp6
        d3rs(3,2,4) = d3rs(3,2,4) + coss*tmp4*tmp4 + 0.5*cosq*(tmp2 + tmp3)
        d3rs(3,3,4) = d3rs(3,3,4) + coss*tmp3*tmp4 + cosq*tmp4
        d3rs(1,1,5) = d3rs(1,1,5) + coss*tmp1*tmp5 + cosq*tmp5
        d3rs(2,1,5) = d3rs(2,1,5) + coss*tmp6*tmp5 + 0.5d0*cosq*tmp4
        d3rs(2,2,5) = d3rs(2,2,5) + coss*tmp2*tmp5
        d3rs(3,1,5) = d3rs(3,1,5) + coss*tmp5*tmp5 + 0.5*cosq*(tmp1 + tmp3)
        d3rs(3,2,5) = d3rs(3,2,5) + coss*tmp4*tmp5 + 0.5d0*cosq*tmp6
        d3rs(3,3,5) = d3rs(3,3,5) + coss*tmp3*tmp5 + cosq*tmp5
        d3rs(1,1,6) = d3rs(1,1,6) + coss*tmp1*tmp6 + cosq*tmp6
        d3rs(2,1,6) = d3rs(2,1,6) + coss*tmp6*tmp6 + 0.5*cosq*(tmp1 + tmp2)
        d3rs(2,2,6) = d3rs(2,2,6) + coss*tmp2*tmp6 + cosq*tmp6
        d3rs(3,1,6) = d3rs(3,1,6) + coss*tmp5*tmp6 + 0.5d0*cosq*tmp4
        d3rs(3,2,6) = d3rs(3,2,6) + coss*tmp4*tmp6 + 0.5d0*cosq*tmp5
        d3rs(3,3,6) = d3rs(3,3,6) + coss*tmp3*tmp6
!
        d3is(3,1,1) = d3is(3,1,1) - sins*tmp5*tmp1 - 2.0d0*sinq*tmp5
        d3is(3,2,1) = d3is(3,2,1) - sins*tmp4*tmp1 - sinq*tmp4
        d3is(3,3,1) = d3is(3,3,1) - sins*tmp3*tmp1 - sinq*tmp3
        d3is(3,1,2) = d3is(3,1,2) - sins*tmp5*tmp2 - sinq*tmp5
        d3is(3,2,2) = d3is(3,2,2) - sins*tmp4*tmp2 - 2.0d0*sinq*tmp4
        d3is(3,3,2) = d3is(3,3,2) - sins*tmp3*tmp2 - sinq*tmp3
        d3is(1,1,3) = d3is(1,1,3) - sins*tmp1*tmp3 - sinq*tmp1
        d3is(2,1,3) = d3is(2,1,3) - sins*tmp6*tmp3 - sinq*tmp6
        d3is(2,2,3) = d3is(2,2,3) - sins*tmp2*tmp3 - sinq*tmp2
        d3is(3,1,3) = d3is(3,1,3) - sins*tmp5*tmp3 - 2.0d0*sinq*tmp5
        d3is(3,2,3) = d3is(3,2,3) - sins*tmp4*tmp3 - 2.0d0*sinq*tmp4
        d3is(3,3,3) = d3is(3,3,3) - sins*tmp3*tmp3 - 3.0d0*sinq*tmp3
        d3is(1,1,4) = d3is(1,1,4) - sins*tmp1*tmp4
        d3is(2,1,4) = d3is(2,1,4) - sins*tmp6*tmp4 - 0.5d0*sinq*tmp5
        d3is(2,2,4) = d3is(2,2,4) - sins*tmp2*tmp4 - 0.5d0*sinq*tmp4
        d3is(3,1,4) = d3is(3,1,4) - sins*tmp5*tmp4 - 0.5d0*sinq*tmp6
        d3is(3,2,4) = d3is(3,2,4) - sins*tmp4*tmp4 - 0.5d0*sinq*(tmp2 + tmp3)
        d3is(3,3,4) = d3is(3,3,4) - sins*tmp3*tmp4 - sinq*tmp4
        d3is(1,1,5) = d3is(1,1,5) - sins*tmp1*tmp5 - sinq*tmp5
        d3is(2,1,5) = d3is(2,1,5) - sins*tmp6*tmp5 - 0.5d0*sinq*tmp4
        d3is(2,2,5) = d3is(2,2,5) - sins*tmp2*tmp5
        d3is(3,1,5) = d3is(3,1,5) - sins*tmp5*tmp5 - 0.5d0*sinq*(tmp1 + tmp3)
        d3is(3,2,5) = d3is(3,2,5) - sins*tmp4*tmp5 - 0.5d0*sinq*tmp6
        d3is(3,3,5) = d3is(3,3,5) - sins*tmp3*tmp5 - sinq*tmp5
        d3is(1,1,6) = d3is(1,1,6) - sins*tmp1*tmp6 - sinq*tmp6
        d3is(2,1,6) = d3is(2,1,6) - sins*tmp6*tmp6 - 0.5d0*sinq*(tmp1 + tmp2)
        d3is(2,2,6) = d3is(2,2,6) - sins*tmp2*tmp6 - sinq*tmp6
        d3is(3,1,6) = d3is(3,1,6) - sins*tmp5*tmp6 - 0.5d0*sinq*tmp4
        d3is(3,2,6) = d3is(3,2,6) - sins*tmp4*tmp6 - 0.5d0*sinq*tmp5
        d3is(3,3,6) = d3is(3,3,6) - sins*tmp3*tmp6
      elseif (ndim.eq.2) then
        d3rs(3,1,1) = d3rs(3,1,1) + sinsz*tmp1*xrkk + 2.0d0*sinr*ztrm1*xrkk
        d3rs(3,2,1) = d3rs(3,2,1) + sinsz*tmp1*yrkk + sinr*ztrm1*yrkk
        d3rs(3,3,1) = d3rs(3,3,1) - cosszz*tmp1 - cosr*trmzz
        d3rs(3,1,2) = d3rs(3,1,2) + sinsz*tmp2*xrkk + sinr*ztrm1*xrkk
        d3rs(3,2,2) = d3rs(3,2,2) + sinsz*tmp2*yrkk + 2.0d0*sinr*ztrm1*yrkk
        d3rs(3,3,2) = d3rs(3,3,2) - cosszz*tmp2 - cosr*trmzz
        d3rs(3,1,6) = d3rs(3,1,6) + sinsz*tmp6*xrkk
        d3rs(3,2,6) = d3rs(3,2,6) + sinsz*tmp6*yrkk + sinr*ztrm1*xrkk
        d3rs(3,3,6) = d3rs(3,3,6) - cosszz*tmp6
!
        d3rs(1,1,6) = d3rs(1,1,6) + coss*tmp1*tmp6
        d3rs(2,1,6) = d3rs(2,1,6) + coss*tmp6*tmp6 + cosq*tmp1
        d3rs(2,2,6) = d3rs(2,2,6) + coss*tmp2*tmp6 + 2.0*cosq*tmp6
!
        d3is(3,1,1) = d3is(3,1,1) + cossz*tmp1*xrkk + 2.0d0*cosr*ztrm1*xrkk
        d3is(3,2,1) = d3is(3,2,1) + cossz*tmp1*yrkk + cosr*ztrm1*yrkk
        d3is(3,3,1) = d3is(3,3,1) + sinszz*tmp1 + sinr*trmzz
        d3is(3,1,2) = d3is(3,1,2) + cossz*tmp2*xrkk + cosr*ztrm1*xrkk
        d3is(3,2,2) = d3is(3,2,2) + cossz*tmp2*yrkk + 2.0d0*cosr*ztrm1*yrkk
        d3is(3,3,2) = d3is(3,3,2) + sinszz*tmp2 + sinr*trmzz
        d3is(3,1,6) = d3is(3,1,6) + cossz*tmp6*xrkk
        d3is(3,2,6) = d3is(3,2,6) + cossz*tmp6*yrkk + cosr*ztrm1*xrkk
        d3is(3,3,6) = d3is(3,3,6) + sinszz*tmp6
!
        d3is(1,1,6) = d3is(1,1,6) - sins*tmp1*tmp6
        d3is(2,1,6) = d3is(2,1,6) - sins*tmp6*tmp6 - sinq*tmp1
        d3is(2,2,6) = d3is(2,2,6) - sins*tmp2*tmp6 - 2.0*sinq*tmp6
      endif
    endif
  enddo
  if (.not.lgamma) then
    do iv = 1,nkvec0
      xrkk = xrk0(iv)
      yrkk = yrk0(iv)
      zrkk = zrk0(iv)
      if (ndim.eq.3) then
        arg = xrkk*xd + yrkk*yd + zrkk*zd
        if (ldoc6) then
          cosa = cos(arg)
          sina = sin(arg)
          sktrm1 = (ktrm0(iv)*qfct - ktrm60(iv)*c6tot)
          sktrm2 = (ktrm20(iv)*qfct - ktrm620(iv)*c6tot)
          cosq = cosa*sktrm1
          sinq = sina*sktrm1
          coss = cosa*sktrm2
          sins = sina*sktrm2
        else
          cosa = cos(arg)*qfct
          sina = sin(arg)*qfct
          cosq = cosa*ktrm0(iv)
          sinq = sina*ktrm0(iv)
          coss = cosa*ktrm20(iv)
          sins = sina*ktrm20(iv)
        endif
      elseif (ndim.eq.2) then
!
!  2-D
!
        arg = xrkk*xd + yrkk*yd
        cosa = cos(arg)*qfct
        sina = sin(arg)*qfct
!
        kvec = ktrm20(iv)
        dexp1 = exp(kvec*zd)
        dexp2 = 1.0_dp/dexp1
        darg1 = kvec*rhseta + etaz
        darg2 = kvec*rhseta - etaz
        dexp3 = exp(-(darg1)**2)
        dexp4 = exp(-(darg2)**2)
        derfc1 = derfc(darg1)
        derfc2 = derfc(darg2)
        kexperfc = dexp1*derfc1 + dexp2*derfc2
        d11m22 = dexp1*derfc1 - dexp2*derfc2
        d13m24 = dexp1*dexp3 - dexp2*dexp4
        d13p24 = dexp1*dexp3 + dexp2*dexp4
        ztrm1 = (kvec*d11m22 - tweatpi*d13m24)
        trmzz = (kvec*kvec*kexperfc - 2.0_dp*tweatpi* &
          (kvec*d13p24 - seta*(darg1*dexp1*dexp3+ darg2*dexp2*dexp4)))
        trmzzz = kvec*kvec*kvec*d11m22 - tweatpi*(3.0_dp*kvec*kvec*d13m24  &
        - 6.0_dp*kvec*seta*(dexp1*darg1*dexp3-dexp2*darg2*dexp4) &
        + 4.0*eta*(dexp1*darg1*darg1*dexp3-dexp2*darg2*darg2*dexp4) &
        - 2.0_dp*eta*d13m24)
        cosr = cosa*ktrm0(iv)
        sinr = sina*ktrm0(iv)
        cosq = cosr*kexperfc
        sinq = sinr*kexperfc
        if (lstr) then
          rkvec = 1.0_dp/kvec
          strm1 = rkvec*(-rkvec*kexperfc + zd*d11m22 - rpieta*d13p24)
          sztrm2 = rkvec*(zd*kvec*kexperfc + tweatpi*rkvec*d13m24)
          sztrm3 = rkvec*(kvec*kexperfc + kvec*kvec*zd*d11m22 - kvec*zd*tweatpi*d13m24 + &
            tweatpi*d13p24 - 2.0_dp*tweatpi*seta*rkvec*(darg1*dexp1*dexp3 + darg2*dexp2*dexp4))
          coss = cosr*strm1
          sinsz = sinr*sztrm2
          cosszz = cosr*sztrm3
        endif
      endif
      tmp1 = xrkk*xrkk
      tmp2 = yrkk*yrkk
      tmp3 = zrkk*zrkk
      tmp4 = yrkk*zrkk
      tmp5 = xrkk*zrkk
      tmp6 = xrkk*yrkk
!
!  Calculate real third derivatives
!
      d3(1,1,1) = d3(1,1,1) + sinq*tmp1*xrkk
      d3(2,1,1) = d3(2,1,1) + sinq*tmp6*xrkk
      d3(2,2,1) = d3(2,2,1) + sinq*tmp2*xrkk
      d3(2,2,2) = d3(2,2,2) + sinq*tmp2*yrkk
      if (ndim.eq.3) then
        d3(3,1,1) = d3(3,1,1) + sinq*tmp5*xrkk
        d3(3,2,1) = d3(3,2,1) + sinq*tmp4*xrkk
        d3(3,3,1) = d3(3,3,1) + sinq*tmp3*xrkk
        d3(3,2,2) = d3(3,2,2) + sinq*tmp4*yrkk
        d3(3,3,2) = d3(3,3,2) + sinq*tmp3*yrkk
        d3(3,3,3) = d3(3,3,3) + sinq*tmp3*zrkk
      elseif (ndim.eq.2) then
! 1 z component
        d3(3,1,1) = d3(3,1,1) - cosr*tmp1*ztrm1
        d3(3,2,1) = d3(3,2,1) - cosr*tmp6*ztrm1
        d3(3,2,2) = d3(3,2,2) - cosr*tmp2*ztrm1
! 2 z components
        d3(3,3,1) = d3(3,3,1) - sinr*trmzz*xrkk
        d3(3,3,2) = d3(3,3,2) - sinr*trmzz*yrkk
! 3 z components
        d3(3,3,3) = d3(3,3,3) + cosr*trmzzz
      endif
      if (lstr) then
        if (ndim.eq.3) then
          dt1 = coss*tmp1
          d3s(1,1,1) = d3s(1,1,1) + dt1*tmp1
          d3s(2,1,1) = d3s(2,1,1) + dt1*tmp6
          d3s(3,1,1) = d3s(3,1,1) + dt1*tmp5
          d3s(2,2,1) = d3s(2,2,1) + dt1*tmp2
          d3s(3,2,1) = d3s(3,2,1) + dt1*tmp4
          d3s(3,3,1) = d3s(3,3,1) + dt1*tmp3
          d3s(1,1,1) = d3s(1,1,1) + 3.0d0*cosq*tmp1
          d3s(2,1,1) = d3s(2,1,1) + 2.0d0*cosq*tmp6
          d3s(3,1,1) = d3s(3,1,1) + 2.0d0*cosq*tmp5
          d3s(2,2,1) = d3s(2,2,1) + cosq*tmp2
          d3s(3,2,1) = d3s(3,2,1) + cosq*tmp4
          d3s(3,3,1) = d3s(3,3,1) + cosq*tmp3
          dt1 = coss*tmp2
          d3s(1,1,2) = d3s(1,1,2) + dt1*tmp1
          d3s(2,1,2) = d3s(2,1,2) + dt1*tmp6
          d3s(3,1,2) = d3s(3,1,2) + dt1*tmp5
          d3s(2,2,2) = d3s(2,2,2) + dt1*tmp2
          d3s(3,2,2) = d3s(3,2,2) + dt1*tmp4
          d3s(3,3,2) = d3s(3,3,2) + dt1*tmp3
          d3s(1,1,2) = d3s(1,1,2) + cosq*tmp1
          d3s(2,1,2) = d3s(2,1,2) + 2.0d0*cosq*tmp6
          d3s(3,1,2) = d3s(3,1,2) + cosq*tmp5
          d3s(2,2,2) = d3s(2,2,2) + 3.0d0*cosq*tmp2
          d3s(3,2,2) = d3s(3,2,2) + 2.0d0*cosq*tmp4
          d3s(3,3,2) = d3s(3,3,2) + cosq*tmp3
          dt1 = coss*tmp3
          d3s(1,1,3) = d3s(1,1,3) + dt1*tmp1
          d3s(2,1,3) = d3s(2,1,3) + dt1*tmp6
          d3s(3,1,3) = d3s(3,1,3) + dt1*tmp5
          d3s(2,2,3) = d3s(2,2,3) + dt1*tmp2
          d3s(3,2,3) = d3s(3,2,3) + dt1*tmp4
          d3s(3,3,3) = d3s(3,3,3) + dt1*tmp3
          d3s(1,1,3) = d3s(1,1,3) + cosq*tmp1
          d3s(2,1,3) = d3s(2,1,3) + cosq*tmp6
          d3s(3,1,3) = d3s(3,1,3) + 2.0d0*cosq*tmp5
          d3s(2,2,3) = d3s(2,2,3) + cosq*tmp2
          d3s(3,2,3) = d3s(3,2,3) + 2.0d0*cosq*tmp4
          d3s(3,3,3) = d3s(3,3,3) + 3.0d0*cosq*tmp3
          dt1 = coss*tmp4
          d3s(1,1,4) = d3s(1,1,4) + dt1*tmp1
          d3s(2,1,4) = d3s(2,1,4) + dt1*tmp6
          d3s(3,1,4) = d3s(3,1,4) + dt1*tmp5
          d3s(2,2,4) = d3s(2,2,4) + dt1*tmp2
          d3s(3,2,4) = d3s(3,2,4) + dt1*tmp4
          d3s(3,3,4) = d3s(3,3,4) + dt1*tmp3
          d3s(2,1,4) = d3s(2,1,4) + 0.5d0*cosq*tmp5
          d3s(3,1,4) = d3s(3,1,4) + 0.5d0*cosq*tmp6
          d3s(2,2,4) = d3s(2,2,4) + cosq*tmp4
          d3s(3,2,4) = d3s(3,2,4) + 0.5d0*cosq*(tmp2 + tmp3)
          d3s(3,3,4) = d3s(3,3,4) + cosq*tmp4
          dt1 = coss*tmp5
          d3s(1,1,5) = d3s(1,1,5) + dt1*tmp1
          d3s(2,1,5) = d3s(2,1,5) + dt1*tmp6
          d3s(3,1,5) = d3s(3,1,5) + dt1*tmp5
          d3s(2,2,5) = d3s(2,2,5) + dt1*tmp2
          d3s(3,2,5) = d3s(3,2,5) + dt1*tmp4
          d3s(3,3,5) = d3s(3,3,5) + dt1*tmp3
          d3s(1,1,5) = d3s(1,1,5) + cosq*tmp5
          d3s(2,1,5) = d3s(2,1,5) + 0.5d0*cosq*tmp4
          d3s(3,1,5) = d3s(3,1,5) + 0.5d0*cosq*(tmp1 + tmp3)
          d3s(3,2,5) = d3s(3,2,5) + 0.5d0*cosq*tmp6
          d3s(3,3,5) = d3s(3,3,5) + cosq*tmp5
          dt1 = coss*tmp6
          d3s(1,1,6) = d3s(1,1,6) + dt1*tmp1
          d3s(2,1,6) = d3s(2,1,6) + dt1*tmp6
          d3s(3,1,6) = d3s(3,1,6) + dt1*tmp5
          d3s(2,2,6) = d3s(2,2,6) + dt1*tmp2
          d3s(3,2,6) = d3s(3,2,6) + dt1*tmp4
          d3s(3,3,6) = d3s(3,3,6) + dt1*tmp3
          d3s(1,1,6) = d3s(1,1,6) + cosq*tmp6
          d3s(2,1,6) = d3s(2,1,6) + 0.5d0*cosq*(tmp1 + tmp2)
          d3s(3,1,6) = d3s(3,1,6) + 0.5d0*cosq*tmp4
          d3s(2,2,6) = d3s(2,2,6) + cosq*tmp6
          d3s(3,2,6) = d3s(3,2,6) + 0.5d0*cosq*tmp5
        elseif (ndim.eq.2) then
          d3s(1,1,1) = d3s(1,1,1) + (coss*tmp1 + 3.0d0*cosq)*tmp1
          d3s(2,1,1) = d3s(2,1,1) + (coss*tmp1 + 2.0d0*cosq)*tmp6
          d3s(2,2,1) = d3s(2,2,1) + (coss*tmp1 + cosq)*tmp2
          d3s(1,1,2) = d3s(1,1,2) + (coss*tmp2 + cosq)*tmp1
          d3s(2,1,2) = d3s(2,1,2) + (coss*tmp2 + 2.0d0*cosq)*tmp6
          d3s(2,2,2) = d3s(2,2,2) + (coss*tmp2 + 3.0d0*cosq)*tmp2
          d3s(3,1,1) = d3s(3,1,1) + (sinsz*tmp1 + 2.0d0*sinr*ztrm1)*xrkk
          d3s(3,2,1) = d3s(3,2,1) + (sinsz*tmp1 + sinr*ztrm1)*yrkk
          d3s(3,3,1) = d3s(3,3,1) - cosszz*tmp1 - cosr*trmzz 
          d3s(3,1,2) = d3s(3,1,2) + (sinsz*tmp2 + sinr*ztrm1)*xrkk
          d3s(3,2,2) = d3s(3,2,2) + (sinsz*tmp2 + 2.0d0*sinr*ztrm1)*yrkk
          d3s(3,3,2) = d3s(3,3,2) - cosszz*tmp2 - cosr*trmzz
          d3s(3,1,6) = d3s(3,1,6) + sinsz*tmp6*xrkk
          d3s(3,2,6) = d3s(3,2,6) + sinsz*tmp6*yrkk + sinr*ztrm1*xrkk
          d3s(3,3,6) = d3s(3,3,6) - cosszz*tmp6
          d3s(1,1,6) = d3s(1,1,6) + coss*tmp1*tmp6
          d3s(2,1,6) = d3s(2,1,6) + coss*tmp6*tmp6 + cosq*tmp1
          d3s(2,2,6) = d3s(2,2,6) + (coss*tmp2 + 2.0*cosq)*tmp6
        endif
      endif
    enddo
  else
!
!  Gamma point - just copy values
!
    d3(1,1,1) = d3r(1,1,1)
    d3(2,1,1) = d3r(2,1,1)
    d3(3,1,1) = d3r(3,1,1)
    d3(2,2,1) = d3r(2,2,1)
    d3(3,2,1) = d3r(3,2,1)
    d3(3,3,1) = d3r(3,3,1)
    d3(2,2,2) = d3r(2,2,2)
    d3(3,2,2) = d3r(3,2,2)
    d3(3,3,2) = d3r(3,3,2)
    d3(3,3,3) = d3r(3,3,3)
    if (lstr) then
      do kk = 1,6
        d3s(1,1,kk) = d3rs(1,1,kk)
        d3s(2,1,kk) = d3rs(2,1,kk)
        d3s(3,1,kk) = d3rs(3,1,kk)
        d3s(2,2,kk) = d3rs(2,2,kk)
        d3s(3,2,kk) = d3rs(3,2,kk)
        d3s(3,3,kk) = d3rs(3,3,kk)
      enddo
    endif
  endif
!
  return
  end
