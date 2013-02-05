  subroutine EDIP_threebody(nspeci,nspecj,nspeck,xij,yij,zij,rij,xik,yik,zik,rik,Zi,e3i, &
                            de3idr,de3idZi,lgrad1)
!
!  Subroutine to calculate the threebody term for the EDIP potential
!
!  On entry : 
!
!  nspeci          = species number of atom type i 
!  nspecj          = species number of atom type j 
!  nspeck          = species number of atom type k 
!  xij             = x component of i->j vector
!  yij             = y component of i->j vector
!  zij             = z component of i->j vector
!  rij             = length of i->j vector
!  xik             = x component of i->k vector
!  yik             = y component of i->k vector
!  zik             = z component of i->k vector
!  rik             = length of i->k vector
!  Zi              = coordination number of pivot atom i
!  lgrad1          = if .true. calculate the first derivative
!
!  On exit :
!
!  e3i             = the value of the threebody energy
!  de3idr(3)       = the first derivatives of e3i w.r.t. the
!                    three different interatomic vectors
!  de3idZi         = the first derivative of e3i w.r.t. Zi
!
!  10/10 Created from gtheta
!  10/10 Cutoffs added for exponential terms
!  10/10 EDIP linear threebody modifications added
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use edipdata
  use constants,   only : pi
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nspeci
  integer(i4), intent(in)             :: nspecj
  integer(i4), intent(in)             :: nspeck
  real(dp),    intent(in)             :: Zi
  real(dp),    intent(in)             :: rij
  real(dp),    intent(in)             :: rik
  real(dp),    intent(in)             :: xij
  real(dp),    intent(in)             :: yij
  real(dp),    intent(in)             :: zij
  real(dp),    intent(in)             :: xik
  real(dp),    intent(in)             :: yik
  real(dp),    intent(in)             :: zik
  real(dp),    intent(out)            :: e3i
  real(dp),    intent(out)            :: de3idr(3)
  real(dp),    intent(out)            :: de3idZi
  logical,     intent(in)             :: lgrad1
!
!  Local variables
!
  integer(i4)                         :: indij
  integer(i4)                         :: indik
  integer(i4)                         :: indjk
  real(dp)                            :: cZ2
  real(dp)                            :: dcZ2dZ
  real(dp)                            :: costheta
  real(dp)                            :: cos1d(3)
  real(dp)                            :: cutoffij
  real(dp)                            :: cutoffij2
  real(dp)                            :: cutoffik
  real(dp)                            :: cutoffik2
  real(dp)                            :: dgijdrij
  real(dp)                            :: dgikdrik
  real(dp)                            :: dgijdZi
  real(dp)                            :: dgikdZi
  real(dp)                            :: dhijkdcostheta
  real(dp)                            :: dhijkdZi
  real(dp)                            :: dlambdadZi
  real(dp)                            :: dtauZdZ
  real(dp)                            :: exph
  real(dp)                            :: gij
  real(dp)                            :: gik
  real(dp)                            :: hijk
  real(dp)                            :: lambda
  real(dp)                            :: Qh
  real(dp)                            :: kQh2
  real(dp)                            :: rQh
  real(dp)                            :: rrij
  real(dp)                            :: rrik
  real(dp)                            :: rrij2
  real(dp)                            :: rrik2
  real(dp)                            :: r2ij
  real(dp)                            :: r2ik
  real(dp)                            :: r2jk
  real(dp)                            :: tauZ
  real(dp)                            :: x
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: Z0
!
!  Compute index of i-j pair
!
  if (nspeci.ge.nspecj) then
    indij = nspeci*(nspeci - 1) + nspecj
  else
    indij = nspecj*(nspecj - 1) + nspeci
  endif
!
!  Compute index of i-k pair
!
  if (nspeci.ge.nspeck) then
    indik = nspeci*(nspeci - 1) + nspeck
  else
    indik = nspeck*(nspeck - 1) + nspeci
  endif
!
!  Compute index of j-k pair
!
  if (nspecj.ge.nspeck) then
    indjk = nspecj*(nspecj - 1) + nspeck
  else
    indjk = nspeck*(nspeck - 1) + nspecj
  endif
!*******************************
!  Compute and check cut-offs  *
!*******************************
  cutoffij = EDIP2a(indij) + EDIP2aprime(indij)*Zi
  cutoffik = EDIP2a(indik) + EDIP2aprime(indik)*Zi
  cutoffij2 = cutoffij + EDIP3gamma0(indjk,nspeci)*EDIPaccuracy2drmax
  cutoffik2 = cutoffik + EDIP3gamma0(indjk,nspeci)*EDIPaccuracy2drmax
!
  if (rij.lt.cutoffij2.and.rik.lt.cutoffik2) then
!
!  Calculate j->k vector
!
    xjk = xik - xij
    yjk = yik - yij
    zjk = zik - zij
!
!  Calculate interatomic distances
!
    r2ij = xij*xij + yij*yij + zij*zij
    r2ik = xik*xik + yik*yik + zik*zik
    r2jk = xjk*xjk + yjk*yjk + zjk*zjk
!
!  Calculate reciprocal distances
!
    rrij = 1.0_dp/rij
    rrik = 1.0_dp/rik
!
!  Calculate cos(theta) using cosine rule
!
    costheta = 0.5_dp*(r2ij + r2ik - r2jk)*rrij*rrik
!**********************
!  Compute lambda(Z)  *
!**********************
    Z0 = EDIP3Z0(indjk,nspeci)
    lambda = EDIP3lambda0(indjk,nspeci)*exp(-EDIP3lambdap(indjk,nspeci)*(Zi-Z0)**2)
!*******************
!  Compute g(r,Z)  *
!*******************
    call EDIP_expfn(rij,cutoffij,EDIP2aprime(indij),EDIP3gamma0(indjk,nspeci),Zi,gij,dgijdrij,dgijdZi,lgrad1)
    call EDIP_expfn(rik,cutoffik,EDIP2aprime(indik),EDIP3gamma0(indjk,nspeci),Zi,gik,dgikdrik,dgikdZi,lgrad1)
    gij = gij*EDIP3gammap(indjk,nspeci)
    gik = gik*EDIP3gammap(indjk,nspeci)
!*******************
!  Compute tau(Z)  *
!*******************
    call EDIP_tau(Zi,tauZ,dtauZdZ,lgrad1)
!***********************
!  Compute h(theta,Z)  *
!***********************
    if (Zi.ge.3) then
      Qh = EDIP3q(indjk,nspeci)
      rQh = 1.0_dp/Qh
      exph = exp(-Qh*(costheta + tauZ)**2)
      hijk = rQh*(1.0_dp - exph)
      if (lgrad1) then
        dhijkdcostheta = 2.0_dp*exph*(costheta + tauZ)
        dhijkdZi = dhijkdcostheta*dtauZdZ
      endif
    elseif (Zi.le.2) then
      kQh2 = EDIP3kq2(indjk,nspeci)
      hijk = kQh2*(1.0_dp + costheta)
      if (lgrad1) then
        dhijkdcostheta = kQh2
        dhijkdZi = 0.0_dp
      endif
    else
      Qh = EDIP3q(indjk,nspeci)
      rQh = 1.0_dp/Qh
      exph = exp(-Qh*(costheta + tauZ)**2)
!
      x = (Zi - 2.0_dp)
      cZ2 = 0.5_dp*(1.0_dp + cos(pi*x))
!
      kQh2 = EDIP3kq2(indjk,nspeci)
      hijk = cZ2*kQh2*(1.0_dp + costheta) + (1.0_dp - cZ2)*rQh*(1.0_dp - exph)
      if (lgrad1) then
        dcZ2dZ = - 0.5_dp*pi*sin(pi*x)
        dhijkdcostheta = (1.0_dp - cZ2)*2.0_dp*exph*(costheta + tauZ)
        dhijkdZi = dhijkdcostheta*dtauZdZ + dcZ2dZ*(kQh2*(1.0_dp + costheta) - rQh*(1.0_dp - exph))
        dhijkdcostheta = dhijkdcostheta + cZ2*kQh2
      endif
    endif
!**************************************
!  Compute total energy contribution  *
!**************************************
    e3i = lambda*gij*gik*hijk
!**************************
!  Calculate derivatives  *
!**************************
    if (lgrad1) then
!
!  Initialise first derivatives of e3i
!
      de3idr(1:3) = 0.0_dp
      de3idZi = 0.0_dp
!
!  First derivatives of cos(theta)
!
!  1 = ij
!  2 = ik
!  3 = jk
!
      rrij2 = rrij*rrij
      rrik2 = rrik*rrik
      cos1d(1) = rrij*rrik - costheta*rrij2
      cos1d(2) = rrij*rrik - costheta*rrik2
      cos1d(3) = - rrij*rrik
!
!  Derivative of lambda
!
      dlambdadZi = - 2.0_dp*EDIP3lambdap(indjk,nspeci)*(Zi - Z0)*lambda
      de3idZi = de3idZi + gij*gik*hijk*dlambdadZi
!
!  Derivatives of g(r,Z)
!
      dgijdrij = dgijdrij*EDIP3gammap(indjk,nspeci)
      dgikdrik = dgikdrik*EDIP3gammap(indjk,nspeci)
      dgijdZi = dgijdZi*EDIP3gammap(indjk,nspeci)
      dgikdZi = dgikdZi*EDIP3gammap(indjk,nspeci)
!
      de3idZi = de3idZi + lambda*gik*hijk*dgijdZi
      de3idZi = de3idZi + lambda*gij*hijk*dgikdZi
      de3idr(1) = de3idr(1) + lambda*gik*hijk*dgijdrij*rrij
      de3idr(2) = de3idr(2) + lambda*gij*hijk*dgikdrik*rrik
!
!  Derivatives of h(theta,Z)
!
      de3idr(1:3) = de3idr(1:3) + lambda*gij*gik*dhijkdcostheta*cos1d(1:3)
      de3idZi = de3idZi + lambda*gij*gik*dhijkdZi
    endif
  else
    e3i = 0.0_dp
    if (lgrad1) then
      de3idr(1:3) = 0.0_dp
      de3idZi = 0.0_dp
    endif
  endif
!
  return
  end
!
  subroutine EDIP_tau(Z,tauZ,dtauZdZ,lgrad1)
!
!  Subroutine to calculate the tau term for the EDIP potential
!
!  On entry : 
!
!  Zi              = coordination number of pivot atom i
!  lgrad1          = if .true. calculate the first derivative
!
!  On exit :
!
!  tauZ            = the value of tau
!  dtauZdZ         = the first derivatives of tauZ w.r.t. Zi
!
!  10/10 Created 
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: Z
  real(dp),    intent(out)            :: tauZ
  real(dp),    intent(out)            :: dtauZdZ
  logical,     intent(in)             :: lgrad1
!
!  Local variables
!
  real(dp)                            :: dtanhxdx
  real(dp)                            :: exp2x
  real(dp)                            :: tanhx
  real(dp)                            :: twelth
  real(dp)                            :: x
!
!  Find range
!
  if (Z.le.2.0_dp) then
    tauZ = 1.0_dp
    if (lgrad1) then
      dtauZdZ = 0.0_dp
    endif
  elseif (Z.ge.6.0_dp) then
    tauZ = 0.0_dp
    if (lgrad1) then
      dtauZdZ = 0.0_dp
    endif
  else
    twelth = 1.0_dp/12.0_dp
    x = 6.0_dp*(Z - 2.5_dp)
    exp2x = exp(2.0_dp*x)
    tanhx = (exp2x - 1.0_dp)/(exp2x + 1.0_dp)
    tauZ = 1.0_dp - twelth*Z*(1.0_dp + tanhx)
    if (lgrad1) then
      dtanhxdx = 6.0_dp*(1.0_dp - tanhx*tanhx)
      dtauZdZ = - twelth*((1.0_dp + tanhx) + Z*dtanhxdx)
    endif
  endif
!
  return
  end
