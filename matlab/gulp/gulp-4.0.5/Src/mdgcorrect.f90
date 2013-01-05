  subroutine mdgcorrect
!
!  Performs the corrector step of the Gear 5th order algorithm
!
!   7/97 Created from mdcorrect
!   2/03 NPT algorithm added
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
!  Julian Gale, NRI, Curtin University, July 2005.
!
  use current
  use derivatives
  use moldyn
  use optimisation
  use velocities
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  real(dp)    :: alpha0
  real(dp)    :: alpha1
  real(dp)    :: alpha3
  real(dp)    :: alpha4
  real(dp)    :: alpha5
  real(dp)    :: ccorr(9)
  real(dp)    :: rrmi
  real(dp)    :: xerr
  real(dp)    :: yerr
  real(dp)    :: zerr
!
!  Gear 5th order coefficients
!
  alpha0 = 3.0_dp/16.0_dp
  alpha1 = 251.0_dp/360.0_dp
  alpha3 = 11.0_dp/18.0_dp
  alpha4 = 1.0_dp/6.0_dp
  alpha5 = 1.0_dp/60.0_dp
!******************************
!  Gear Corrector - NVE only  *
!******************************
!
!  Cell
!
  if (nensemble(ncf).eq.3) then
!
!  Calculate correction
!
    do i = 1,9
      ccorr(i) = 0.0_dp
    enddo
!
!  Correct cell motion
!
    do i = 1,9
      velc(i) = velc(i) + alpha1*ccorr(i)
      c2(i) = c2(i) + ccorr(i)
      c3(i) = c3(i) + alpha3*ccorr(i)
      c4(i) = c4(i) + alpha4*ccorr(i)
      c5(i) = c5(i) + alpha5*ccorr(i)
    enddo
  endif
!
!  Atoms
!
  do i = 1,numat
    if (lopf(i).and..not.lfix(i)) then
      rrmi = rmass(i)
      xerr = -stpsqh*rrmi*xdrv(i) - x2(i)
      yerr = -stpsqh*rrmi*ydrv(i) - y2(i)
      zerr = -stpsqh*rrmi*zdrv(i) - z2(i)
!
      xalat(i) = xalat(i) + xerr*alpha0
      yalat(i) = yalat(i) + yerr*alpha0
      zalat(i) = zalat(i) + zerr*alpha0
!
      velx(i) = velx(i) + xerr*alpha1
      vely(i) = vely(i) + yerr*alpha1
      velz(i) = velz(i) + zerr*alpha1
!
      x2(i) = x2(i) + xerr
      y2(i) = y2(i) + yerr
      z2(i) = z2(i) + zerr
!
      x3(i) = x3(i) + xerr*alpha3
      y3(i) = y3(i) + yerr*alpha3
      z3(i) = z3(i) + zerr*alpha3
!
      x4(i) = x4(i) + xerr*alpha4
      y4(i) = y4(i) + yerr*alpha4
      z4(i) = z4(i) + zerr*alpha4
!
      x5(i) = x5(i) + xerr*alpha5
      y5(i) = y5(i) + yerr*alpha5
      z5(i) = z5(i) + zerr*alpha5
    endif
  enddo
!
  return
  end
