  subroutine projfork3(nforkl,nptrfork,nptrforl,d34,d34r, &
    d34i,dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r,dt23r, &
    dt33r,dt11i,dt21i,dt31i,dt12i,dt22i,dt32i,dt13i,dt23i, &
    dt33i,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33, &
    dsqh,iocptr,lzsisa)
!
!  Calculates the free energy derivative contribution from the
!  third atom k to fourth atom l distance for the i-j dynamical
!  matrix block in a four-body interaction.
!
!   8/98 Created from projthbk3
!   8/99 zsisa corrections added
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
  use current, only : nstrains
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)        :: iocptr(*)
  integer(i4)        :: nforkl
  integer(i4)        :: nptrfork(*)
  integer(i4)        :: nptrforl(*)
  logical            :: lzsisa
  real(dp)           :: d34(27,*)
  real(dp)           :: d34i(27,*)
  real(dp)           :: d34r(27,*)
  real(dp)           :: dsqh(maxd2,*)
  real(dp)           :: dt11
  real(dp)           :: dt21
  real(dp)           :: dt31
  real(dp)           :: dt12
  real(dp)           :: dt22
  real(dp)           :: dt32
  real(dp)           :: dt13
  real(dp)           :: dt23
  real(dp)           :: dt33
  real(dp)           :: dt11i
  real(dp)           :: dt21i
  real(dp)           :: dt31i
  real(dp)           :: dt12i
  real(dp)           :: dt22i
  real(dp)           :: dt32i
  real(dp)           :: dt13i
  real(dp)           :: dt23i
  real(dp)           :: dt33i
  real(dp)           :: dt11r
  real(dp)           :: dt21r
  real(dp)           :: dt31r
  real(dp)           :: dt12r
  real(dp)           :: dt22r
  real(dp)           :: dt32r
  real(dp)           :: dt13r
  real(dp)           :: dt23r
  real(dp)           :: dt33r
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: indk
  integer(i4)        :: indl
  integer(i4)        :: jj
  integer(i4)        :: k
  integer(i4)        :: kx
  integer(i4)        :: ky
  integer(i4)        :: kz
  integer(i4)        :: l
  integer(i4)        :: lx
  integer(i4)        :: ly
  integer(i4)        :: lz
  real(dp)           :: dfedxkl
  real(dp)           :: dfedykl
  real(dp)           :: dfedzkl
  real(dp)           :: dfedxkli
  real(dp)           :: dfedykli
  real(dp)           :: dfedzkli
  real(dp)           :: dfedxklr
  real(dp)           :: dfedyklr
  real(dp)           :: dfedzklr
!
!  Loop over four body k-l pairs
!
  do ii = 1,nforkl
    k = nptrfork(ii)
    l = nptrforl(ii)
!******************************************************************
!  Calculate k - l component  -  no assumption that d34 is symmetric  *
!******************************************************************
!
!  Real  -  real
!
    dfedxklr = dt11r*d34r(1,ii) + dt21r*d34r(2,ii) + dt31r*d34r(3,ii) +  &
               dt12r*d34r(4,ii) + dt22r*d34r(5,ii) + dt32r*d34r(6,ii) +  &
               dt13r*d34r(7,ii) + dt23r*d34r(8,ii) + dt33r*d34r(9,ii)
    dfedyklr = dt11r*d34r(10,ii) + dt21r*d34r(11,ii) + dt31r*d34r(12,ii) +  &
               dt12r*d34r(13,ii) + dt22r*d34r(14,ii) + dt32r*d34r(15,ii) +  &
               dt13r*d34r(16,ii) + dt23r*d34r(17,ii) + dt33r*d34r(18,ii)
    dfedzklr = dt11r*d34r(19,ii) + dt21r*d34r(20,ii) + dt31r*d34r(21,ii) +  &
               dt12r*d34r(22,ii) + dt22r*d34r(23,ii) + dt32r*d34r(24,ii) +  &
               dt13r*d34r(25,ii) + dt23r*d34r(26,ii) + dt33r*d34r(27,ii)
!
!  Complex  -  complex
!
    dfedxkli = dt11i*d34i(1,ii) + dt21i*d34i(2,ii) + dt31i*d34i(3,ii) +  &
               dt12i*d34i(4,ii) + dt22i*d34i(5,ii) + dt32i*d34i(6,ii) +  &
               dt13i*d34i(7,ii) + dt23i*d34i(8,ii) + dt33i*d34i(9,ii)
    dfedykli = dt11i*d34i(10,ii) + dt21i*d34i(11,ii) + dt31i*d34i(12,ii) +  &
               dt12i*d34i(13,ii) + dt22i*d34i(14,ii) + dt32i*d34i(15,ii) +  &
               dt13i*d34i(16,ii) + dt23i*d34i(17,ii) + dt33i*d34i(18,ii)
    dfedzkli = dt11i*d34i(19,ii) + dt21i*d34i(20,ii) + dt31i*d34i(21,ii) +  &
               dt12i*d34i(22,ii) + dt22i*d34i(23,ii) + dt32i*d34i(24,ii) +  &
               dt13i*d34i(25,ii) + dt23i*d34i(26,ii) + dt33i*d34i(27,ii)
!
!  Real  -  real  -  on - diagonal
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
    if (lzsisa) then
      indk = 3*(iocptr(k) - 1)
      kx = indk + 1
      ky = indk + 2
      kz = indk + 3
      indl = 3*(iocptr(l) - 1)
      lx = indl + 1
      ly = indl + 2
      lz = indl + 3
      do jj = 1,nstrains
        strderv(jj) = strderv(jj) - (dfedxklr + dfedxkli + dfedxkl)*dsqh(kx,jj)
        strderv(jj) = strderv(jj) - (dfedyklr + dfedykli + dfedykl)*dsqh(ky,jj)
        strderv(jj) = strderv(jj) - (dfedzklr + dfedzkli + dfedzkl)*dsqh(kz,jj)
        strderv(jj) = strderv(jj) + (dfedxklr + dfedxkli + dfedxkl)*dsqh(lx,jj)
        strderv(jj) = strderv(jj) + (dfedyklr + dfedykli + dfedykl)*dsqh(ly,jj)
        strderv(jj) = strderv(jj) + (dfedzklr + dfedzkli + dfedzkl)*dsqh(lz,jj)
      enddo
    else
      xdrv(k) = xdrv(k) + dfedxklr + dfedxkli + dfedxkl
      ydrv(k) = ydrv(k) + dfedyklr + dfedykli + dfedykl
      zdrv(k) = zdrv(k) + dfedzklr + dfedzkli + dfedzkl
      xdrv(l) = xdrv(l) - dfedxklr - dfedxkli - dfedxkl
      ydrv(l) = ydrv(l) - dfedyklr - dfedykli - dfedykl
      zdrv(l) = zdrv(l) - dfedzklr - dfedzkli - dfedzkl
    endif
  enddo
!
  return
  end
