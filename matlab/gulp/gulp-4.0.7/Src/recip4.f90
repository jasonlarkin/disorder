  subroutine recip4(ni,esum4,d2,xmono,ymono,zmono)
!
!  Calculate reciprocal space contribution to sum of 1/r**4
!  interaction.
!
!  ni    = sub-lattice pointer
!  esum4 = sum of 1/r**4 terms
!  d2    = sum of second derivative of 1/r**2
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
  use control
  use current
  use kspace
  implicit none
!
!  Passed variables
!
  integer(i4)        :: ni
  real(dp)           :: d2(3,3)
  real(dp)           :: esum4
  real(dp)           :: xmono
  real(dp)           :: ymono
  real(dp)           :: zmono
!
!  Local variables
!
  integer(i4)        :: iv
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xci
  real(dp)           :: yci
  real(dp)           :: zci
!
  if (lnorecip) return
  xal = xclat(ni)
  yal = yclat(ni)
  zal = zclat(ni)
  xci = xal-xmono
  yci = yal-ymono
  zci = zal-zmono
  do iv = 1,nkvec
    argc(iv) = xrk(iv)*xci
    argc(iv) = yrk(iv)*yci + argc(iv)
    argc(iv) = zrk(iv)*zci + argc(iv)
  enddo
  do iv = 1,nkvec
    csin(iv) = cos(argc(iv))
  enddo
!
!  Sum of 1/r**4
!
  do iv = 1,nkvec
    esum4 = esum4 + csin(iv)*ktrm(iv)
  enddo
!
!  Sum of ra*rb/r**6
!
  do iv = 1,nkvec
    csin(iv) = csin(iv)*sine(iv)
    d2(1,1) = d2(1,1) + csin(iv)*xrk(iv)*xrk(iv)
    d2(2,1) = d2(2,1) + csin(iv)*yrk(iv)*xrk(iv)
    d2(3,1) = d2(3,1) + csin(iv)*zrk(iv)*xrk(iv)
    d2(2,2) = d2(2,2) + csin(iv)*yrk(iv)*yrk(iv)
    d2(3,2) = d2(3,2) + csin(iv)*zrk(iv)*yrk(iv)
    d2(3,3) = d2(3,3) + csin(iv)*zrk(iv)*zrk(iv)
  enddo
!
  return
  end
