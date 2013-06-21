  subroutine meamtotalrho(l,scrho,rhototal)
!
!  Calculates the total density from the components within the MEAM method.
!
!  On entry :
!
!  l        = species number whose functional is being calculated
!  scrho    = array of density components for MEAM orders
!
!  On exit :
!
!  rhototal = density of j at i (if MEAM then this is the contributions for each order)
!
!  The MEAM components in scrho are numbered according to the following:
!
!  1 => order 0 r (standard EAM)
!  2 => order 1 x
!  3 => order 1 y
!  4 => order 1 z
!  5 => order 2 xx
!  6 => order 2 xy
!  7 => order 2 xz
!  8 => order 2 yy
!  9 => order 2 yz
! 10 => order 2 zz
! 11 => order 2 r
! 12 => order 3 xxx
! 13 => order 3 xxy
! 14 => order 3 xxz
! 15 => order 3 xyy
! 16 => order 3 xyz
! 17 => order 3 xzz
! 18 => order 3 yyy
! 19 => order 3 yyz
! 20 => order 3 yzz
! 21 => order 3 zzz
! 22 => order 3 x
! 23 => order 3 y
! 24 => order 3 z
!
!  12/08 Created 
!   4/09 Extended MEAM form with 24 density components added
!   4/09 Exponential combination rule for MEAM components added
!   6/10 Missing factor of 6 added for scrho(16) (xyz)
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
!  Julian Gale, NRI, Curtin University, June 2010
!
  use eam
  use numbers,     only : third
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: l
  real(dp),    intent(out)   :: rhototal
  real(dp),    intent(in)    :: scrho(maxmeamcomponent)
!
!  Local variables
!
  real(dp)                   :: exptrm          ! Exponential of -gamma in MEAM 2NN
  real(dp)                   :: gofrho2         ! G function in MEAM 2NN
  real(dp)                   :: rho0            ! rho_0 in MEAM 2NN
  real(dp)                   :: rho2            ! rho squared in MEAM 1NN or gamma in MEAM 2NN
!
  rho2 = 0.0_dp
  if (maxmeamorder.gt.1) then
!
!  Order = 1
!
    rho2 = rho2 + eamfnmeamcoeff(2,l)*(scrho(2)**2 + scrho(3)**2 + scrho(4)**2)
!
    if (maxmeamorder.gt.2) then
!
!  Order = 2
!
      rho2 = rho2 + eamfnmeamcoeff(3,l)*(scrho(5)**2 + scrho(8)**2 + scrho(10)**2 &
                  + 2.0_dp*(scrho(6)**2 + scrho(7)**2 + scrho(9)**2) &
                  - third*(scrho(11)**2))
!
      if (maxmeamorder.gt.3) then
!
!  Order = 3
!
        rho2 = rho2 + eamfnmeamcoeff(4,l)*(scrho(12)**2 + scrho(18)**2 + scrho(21)**2 &
                    + 3.0_dp*(scrho(13)**2 + scrho(14)**2 + scrho(15)**2 + &
                              scrho(17)**2 + scrho(19)**2 + scrho(20)**2)  &
                    + 6.0_dp*scrho(16)**2)
        if (neamfnmeamtype(l).ne.1) then
          rho2 = rho2 - 0.6_dp*eamfnmeamcoeff(4,l)*(scrho(22)**2 + scrho(23)**2 + scrho(24)**2)
        endif
      endif
    endif
  endif
  if (neamfnmeamcombotype(l).eq.1) then
!-----------------------------
!  MEAM 1NN combination rule |
!-----------------------------
!
!  Add order = 0 density squared to total
!
    rho2 = rho2 + eamfnmeamcoeff(1,l)*(scrho(1)**2)
!
!  Square root to obtain total density
!
    rhototal = sqrt(rho2)
  else
!-----------------------------
!  MEAM 2NN combination rule |
!-----------------------------
!
!  Set rho0
!
    rho0 = scrho(1)
!
!  If rho0 is less than a threshold then need to trap
!
    if (rho0.lt.1.0d-12) then
!
!  Here we assume that if the spherical contribution to rho is close to zero, 
!  that the higher order harmonics will be even closer to zero and therefore that
!  G(gamma) is tending to 1. Error in this limit is not likely to matter given 
!  that prefactor is very small.
!
      rhototal = rho0
    else
!
!  General case
!
      rho2     = rho2/rho0**2
      exptrm   = exp(-rho2)
      gofrho2  = 2.0_dp/(1.0_dp + exptrm)
      rhototal = rho0*gofrho2
    endif
  endif
!
  return
  end
