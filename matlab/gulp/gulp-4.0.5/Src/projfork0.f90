  subroutine projfork0(nforkl,nptrfork,nptrforl,d34,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
!
!  Calculates the free energy derivative contribution from the
!  third atom k to fourth atom l distance for the i-j dynamical 
!  matrix block in a four-body interaction.
!
!   8/98 Created from projmanyk0
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
  integer(i4)      :: nforkl
  integer(i4)      :: nptrfork(*)
  integer(i4)      :: nptrforl(*)
  real(dp)         :: d34(27,*)
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
  integer(i4)      :: l
  real(dp)         :: dfedxkl
  real(dp)         :: dfedykl
  real(dp)         :: dfedzkl
!
!  Loop over three body k atoms
!
  do ii = 1,nforkl
    k = nptrfork(ii)
    l = nptrforl(ii)
!
!  Calculate k-l component - no assumption that d34 is symmetric
!
    dfedxkl = dt11*d34(1,ii) + dt21*d34(2,ii) + dt31*d34(3,ii) +  &
              dt12*d34(4,ii) + dt22*d34(5,ii) + dt32*d34(6,ii) +  &
              dt13*d34(7,ii) + dt23*d34(8,ii) + dt33*d34(9,ii)
    dfedykl = dt11*d34(10,ii) + dt21*d34(11,ii) + dt31*d34(12,ii) +  &
              dt12*d34(13,ii) + dt22*d34(14,ii) + dt32*d34(15,ii) +  &
              dt13*d34(16,ii) + dt23*d34(17,ii) + dt33*d34(18,ii)
    dfedzkl = dt11*d34(19,ii) + dt21*d34(20,ii) + dt31*d34(21,ii) +  &
              dt12*d34(22,ii) + dt22*d34(23,ii) + dt32*d34(24,ii) +  &
              dt13*d34(25,ii) + dt23*d34(26,ii) + dt33*d34(27,ii)
!
!  Add on derivative contributions
!
    xdrv(k) = xdrv(k) + dfedxkl
    ydrv(k) = ydrv(k) + dfedykl
    zdrv(k) = zdrv(k) + dfedzkl
    xdrv(l) = xdrv(l) - dfedxkl
    ydrv(l) = ydrv(l) - dfedykl
    zdrv(l) = zdrv(l) - dfedzkl
  enddo
!
  return
  end
