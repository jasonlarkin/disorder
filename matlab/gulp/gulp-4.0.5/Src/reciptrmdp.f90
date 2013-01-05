  subroutine reciptrmdp(xd,yd,zd,lstr,fct,d2,d2s)
!
!  Calculates the reciprocal space contribution to the derivatives
!  of the polarisation energy
!
!   6/00 Created from recipktrmd3
!   2/01 Modifications for 2-D added
!   3/01 Derivatives for 2-D completed
!   3/03 Modified to allow for symmetry
!  11/07 Unused variables cleaned up
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use constants, only : angstoev, pi
  use current
  use kspace
  implicit none
!
!  Passed variables
!
  logical     :: lstr
  real(dp)    :: d2(3,3)
  real(dp)    :: d2s(3,6)
  real(dp)    :: fct
  real(dp)    :: xd
  real(dp)    :: yd
  real(dp)    :: zd
!
!  Local variables
!
  integer(i4) :: iv
  real(dp)    :: arg
  real(dp)    :: cosa
  real(dp)    :: cosq
  real(dp)    :: d11m22
  real(dp)    :: d13m24
  real(dp)    :: d13p24
  real(dp)    :: darg1
  real(dp)    :: darg2
  real(dp)    :: derf
  real(dp)    :: derfc
  real(dp)    :: derfc1
  real(dp)    :: derfc2
  real(dp)    :: derfez
  real(dp)    :: dexp1
  real(dp)    :: dexp2
  real(dp)    :: dexp3
  real(dp)    :: dexp4
  real(dp)    :: dexpz
  real(dp)    :: dtrm1
  real(dp)    :: dtrm2
  real(dp)    :: etaz
  real(dp)    :: etaz2
  real(dp)    :: kexperfc
  real(dp)    :: kvec
  real(dp)    :: rkvec
  real(dp)    :: sina
  real(dp)    :: sinq
  real(dp)    :: sins
  real(dp)    :: strm1
  real(dp)    :: sztrm2
  real(dp)    :: tmp1
  real(dp)    :: tmp2
  real(dp)    :: tmp3
  real(dp)    :: tmp4
  real(dp)    :: tmp5
  real(dp)    :: tmp6
  real(dp)    :: xrkk
  real(dp)    :: yrkk
  real(dp)    :: zrkk
  real(dp)    :: ztrm1
!
!  Define constants
!
  rpieta = 1.0_dp / sqrt(pi * eta)
  rhseta = 0.5_dp / seta
!
  if (ndim.eq.2) then
!
!  First term - K vector independent
!
    etaz = seta*zd
    etaz2 = etaz * etaz
    dexpz  = exp(-etaz2)
    derfez = derf(etaz)
    dtrm2 = - vol4pi*tweatpi*dexpz*angstoev*fct
    d2(3,3) = d2(3,3) + dtrm2
    if (lstr) then
      dtrm1 = - vol4pi*derfez*angstoev*fct
      d2s(3,1) = d2s(3,1) - dtrm1
      d2s(3,2) = d2s(3,2) - dtrm1
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
      arg = xrkk*xd + yrkk*yd + zrkk*zd
      cosa = cos(arg)
      sina = sin(arg)
      cosq = cosa*ktrm(iv)
      sinq = sina*ktrm(iv)
      sins = sina*ktrms(iv)
    elseif (ndim.eq.2) then
      kvec = kmod(iv)
      arg = xrkk*xd + yrkk*yd
      cosa = cos(arg)
      sina = sin(arg)
      cosq = cosa*ktrm(iv)
      sinq = sina*ktrm(iv)
!
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
      if (lstr) then
        rkvec = 1.0_dp/kvec
        strm1 = rkvec*(-rkvec*kexperfc + zd*d11m22 - rpieta*d13p24)
        sztrm2 = rkvec*(zd*kvec*kexperfc + tweatpi*rkvec*d13m24)
        sins = sinq*strm1
        sztrm2 = cosq*sztrm2
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
!  Calculate second derivatives
!
    if (ndim.eq.3) then
      d2(1,1) = d2(1,1) - cosq*tmp1*fct
      d2(2,1) = d2(2,1) - cosq*tmp6*fct
      d2(2,2) = d2(2,2) - cosq*tmp2*fct
      d2(3,1) = d2(3,1) - cosq*tmp5*fct
      d2(3,2) = d2(3,2) - cosq*tmp4*fct
      d2(3,3) = d2(3,3) - cosq*tmp3*fct
    elseif (ndim.eq.2) then
      d2(1,1) = d2(1,1) - cosq*tmp1*kexperfc*fct
      d2(2,1) = d2(2,1) - cosq*tmp6*kexperfc*fct
      d2(2,2) = d2(2,2) - cosq*tmp2*kexperfc*fct
      d2(3,1) = d2(3,1) - sinq*xrkk*ztrm1*fct
      d2(3,2) = d2(3,2) - sinq*yrkk*ztrm1*fct
      d2(3,3) = d2(3,3) + cosq*(kvec*kvec*kexperfc - &
        2.0_dp*tweatpi*(kvec*(dexp1*dexp3+dexp2*dexp4) &
        - seta*(darg1*dexp1*dexp3+darg2*dexp2*dexp4)))*fct
    endif
    if (lstr) then
      if (ndim.eq.3) then
        d2s(1,1) = d2s(1,1) + xrkk*tmp1*sins*fct
        d2s(2,1) = d2s(2,1) + yrkk*tmp1*sins*fct
        d2s(3,1) = d2s(3,1) + zrkk*tmp1*sins*fct
        d2s(1,2) = d2s(1,2) + xrkk*tmp2*sins*fct
        d2s(2,2) = d2s(2,2) + yrkk*tmp2*sins*fct
        d2s(3,2) = d2s(3,2) + zrkk*tmp2*sins*fct
        d2s(1,3) = d2s(1,3) + xrkk*tmp3*sins*fct
        d2s(2,3) = d2s(2,3) + yrkk*tmp3*sins*fct
        d2s(3,3) = d2s(3,3) + zrkk*tmp3*sins*fct
        d2s(1,4) = d2s(1,4) + xrkk*tmp4*sins*fct
        d2s(2,4) = d2s(2,4) + yrkk*tmp4*sins*fct
        d2s(3,4) = d2s(3,4) + zrkk*tmp4*sins*fct
        d2s(1,5) = d2s(1,5) + xrkk*tmp5*sins*fct
        d2s(2,5) = d2s(2,5) + yrkk*tmp5*sins*fct
        d2s(3,5) = d2s(3,5) + zrkk*tmp5*sins*fct
        d2s(1,6) = d2s(1,6) + xrkk*tmp6*sins*fct
        d2s(2,6) = d2s(2,6) + yrkk*tmp6*sins*fct
        d2s(3,6) = d2s(3,6) + zrkk*tmp6*sins*fct
      elseif (ndim.eq.2) then
        d2s(1,1) = d2s(1,1) + xrkk*tmp1*sins*fct
        d2s(2,1) = d2s(2,1) + yrkk*tmp1*sins*fct
        d2s(3,1) = d2s(3,1) - tmp1*sztrm2*fct
        d2s(1,2) = d2s(1,2) + xrkk*tmp2*sins*fct
        d2s(2,2) = d2s(2,2) + yrkk*tmp2*sins*fct
        d2s(3,2) = d2s(3,2) - tmp2*sztrm2*fct
        d2s(1,3) = d2s(1,3) + xrkk*tmp6*sins*fct
        d2s(2,3) = d2s(2,3) + yrkk*tmp6*sins*fct
        d2s(3,3) = d2s(3,3) - tmp6*sztrm2*fct
      endif
!
!  Subtract first derivative terms
!
      if (ndim.eq.3) then
        d2s(1,1) = d2s(1,1) + 2.0_dp*xrkk*sinq*fct
        d2s(2,1) = d2s(2,1) + yrkk*sinq*fct
        d2s(3,1) = d2s(3,1) + zrkk*sinq*fct
        d2s(1,2) = d2s(1,2) + xrkk*sinq*fct
        d2s(2,2) = d2s(2,2) + 2.0_dp*yrkk*sinq*fct
        d2s(3,2) = d2s(3,2) + zrkk*sinq*fct
        d2s(1,3) = d2s(1,3) + xrkk*sinq*fct
        d2s(2,3) = d2s(2,3) + yrkk*sinq*fct
        d2s(3,3) = d2s(3,3) + 2.0_dp*zrkk*sinq*fct
      elseif (ndim.eq.2) then
        d2s(1,1) = d2s(1,1) + 2.0_dp*xrkk*sinq*kexperfc*fct
        d2s(2,1) = d2s(2,1) + yrkk*sinq*kexperfc*fct
        d2s(3,1) = d2s(3,1) - cosq*ztrm1*fct
        d2s(1,2) = d2s(1,2) + xrkk*sinq*kexperfc*fct
        d2s(2,2) = d2s(2,2) + 2.0_dp*yrkk*sinq*kexperfc*fct
        d2s(3,2) = d2s(3,2) - cosq*ztrm1*fct
      endif
!
!  Extra terms - don't quite understand these but it works!!
!
      if (ndim.eq.3) then
        d2s(2,4) = d2s(2,4) + zrkk*sinq*fct
        d2s(1,5) = d2s(1,5) + zrkk*sinq*fct
        d2s(1,6) = d2s(1,6) + yrkk*sinq*fct
      elseif (ndim.eq.2) then
        d2s(1,3) = d2s(1,3) + yrkk*sinq*kexperfc*fct
      endif
    endif
  enddo
!
!  Symmetrise d2
!
  d2(1,2) = d2(2,1)
  d2(1,3) = d2(3,1)
  d2(2,3) = d2(3,2)
!
  return
  end
