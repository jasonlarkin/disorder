  subroutine projthbk0(i,j,nmanyk,nptrmanyk,d33,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
!
!  Calculates the free energy derivative contribution from the
!  third atom k for the i-j dynamical matrix block.
!
!   9/97 Created 
!   5/98 Call modified to handle asymmetry in d33
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
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)      :: i
  integer(i4)      :: j
  integer(i4)      :: nmanyk
  integer(i4)      :: nptrmanyk(*)
  real(dp)         :: d33(54,*)
  real(dp)         :: dt11
  real(dp)         :: dt21
  real(dp)         :: dt31
  real(dp)         :: dt12
  real(dp)         :: dt22
  real(dp)         :: dt32
  real(dp)         :: dt13
  real(dp)         :: dt23
  real(dp)         :: dt33
!
!  Local variables
!
  integer(i4)      :: ii
  integer(i4)      :: k
  real(dp)         :: dfedxik
  real(dp)         :: dfedyik
  real(dp)         :: dfedzik
  real(dp)         :: dfedxjk
  real(dp)         :: dfedyjk
  real(dp)         :: dfedzjk
!
!  Loop over three body k atoms
!
  do ii = 1,nmanyk
    k = nptrmanyk(ii)
!
!  Calculate i-k component - no assumption that d33 is symmetric
!
    dfedxik = dt11*d33(1,ii) + dt21*d33(2,ii) + dt31*d33(3,ii) +  &
              dt12*d33(4,ii) + dt22*d33(5,ii) + dt32*d33(6,ii) +  &
              dt13*d33(7,ii) + dt23*d33(8,ii) + dt33*d33(9,ii)
    dfedyik = dt11*d33(10,ii) + dt21*d33(11,ii) + dt31*d33(12,ii) +  &
              dt12*d33(13,ii) + dt22*d33(14,ii) + dt32*d33(15,ii) +  &
              dt13*d33(16,ii) + dt23*d33(17,ii) + dt33*d33(18,ii)
    dfedzik = dt11*d33(19,ii) + dt21*d33(20,ii) + dt31*d33(21,ii) +  &
              dt12*d33(22,ii) + dt22*d33(23,ii) + dt32*d33(24,ii) +  &
              dt13*d33(25,ii) + dt23*d33(26,ii) + dt33*d33(27,ii)
!
!  Calculate j - k component  -  no assumption that d33 is symmetric
!
    dfedxjk = dt11*d33(28,ii) + dt21*d33(29,ii) + dt31*d33(30,ii) +  &
              dt12*d33(31,ii) + dt22*d33(32,ii) + dt32*d33(33,ii) +  &
              dt13*d33(34,ii) + dt23*d33(35,ii) + dt33*d33(36,ii)
    dfedyjk = dt11*d33(37,ii) + dt21*d33(38,ii) + dt31*d33(39,ii) +  &
              dt12*d33(40,ii) + dt22*d33(41,ii) + dt32*d33(42,ii) +  &
              dt13*d33(43,ii) + dt23*d33(44,ii) + dt33*d33(45,ii)
    dfedzjk = dt11*d33(46,ii) + dt21*d33(47,ii) + dt31*d33(48,ii) +  &
              dt12*d33(49,ii) + dt22*d33(50,ii) + dt32*d33(51,ii) +  &
              dt13*d33(52,ii) + dt23*d33(53,ii) + dt33*d33(54,ii)
!
!  Add on derivative contributions
!
    xdrv(i) = xdrv(i) + dfedxik
    ydrv(i) = ydrv(i) + dfedyik
    zdrv(i) = zdrv(i) + dfedzik
    xdrv(j) = xdrv(j) + dfedxjk
    ydrv(j) = ydrv(j) + dfedyjk
    zdrv(j) = zdrv(j) + dfedzjk
    xdrv(k) = xdrv(k) - dfedxik - dfedxjk
    ydrv(k) = ydrv(k) - dfedyik - dfedyjk
    zdrv(k) = zdrv(k) - dfedzik - dfedzjk
  enddo
!
  return
  end
