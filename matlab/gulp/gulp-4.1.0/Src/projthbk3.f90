  subroutine projthbk3(i,j,nthbk,nptrthbk,d33,d33r,d33i,dt11r, &
    dt21r,dt31r,dt12r,dt22r,dt32r,dt13r,dt23r,dt33r,dt11i, &
    dt21i,dt31i,dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
    dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33,dsqh,iocptr,lzsisa)
!
!  Calculates the free energy derivative contribution from the
!  third atom k for the i-j dynamical matrix block. Three
!  dimensional version.
!
!   5/98 Created from projthbk0
!   8/99 zsisa corrections added
!   7/05 Style updated
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
  integer(i4)        :: i
  integer(i4)        :: iocptr(*)
  integer(i4)        :: j
  integer(i4)        :: nptrthbk(*)
  integer(i4)        :: nthbk
  logical            :: lzsisa
  real(dp)           :: d33i(54,*)
  real(dp)           :: d33(54,*)
  real(dp)           :: d33r(54,*)
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
  integer(i4)        :: indi
  integer(i4)        :: indj
  integer(i4)        :: indk
  integer(i4)        :: ix
  integer(i4)        :: iy
  integer(i4)        :: iz
  integer(i4)        :: jj
  integer(i4)        :: jx
  integer(i4)        :: jy
  integer(i4)        :: jz
  integer(i4)        :: k
  integer(i4)        :: kx
  integer(i4)        :: ky
  integer(i4)        :: kz
  real(dp)           :: dfedxik
  real(dp)           :: dfedyik
  real(dp)           :: dfedzik
  real(dp)           :: dfedxiki
  real(dp)           :: dfedyiki
  real(dp)           :: dfedziki
  real(dp)           :: dfedxikr
  real(dp)           :: dfedyikr
  real(dp)           :: dfedzikr
  real(dp)           :: dfedxjk
  real(dp)           :: dfedyjk
  real(dp)           :: dfedzjk
  real(dp)           :: dfedxjki
  real(dp)           :: dfedyjki
  real(dp)           :: dfedzjki
  real(dp)           :: dfedxjkr
  real(dp)           :: dfedyjkr
  real(dp)           :: dfedzjkr
!
!  Loop over three body k atoms
!
  do ii = 1,nthbk
    k = nptrthbk(ii)
!******************************************************************
!  Calculate i-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real - real
!
    dfedxikr = dt11r*d33r(1,ii) + dt21r*d33r(2,ii) + dt31r*d33r(3,ii) + &
               dt12r*d33r(4,ii) + dt22r*d33r(5,ii) + dt32r*d33r(6,ii) + &
               dt13r*d33r(7,ii) + dt23r*d33r(8,ii) + dt33r*d33r(9,ii)
    dfedyikr = dt11r*d33r(10,ii) + dt21r*d33r(11,ii) + dt31r*d33r(12,ii) + &
               dt12r*d33r(13,ii) + dt22r*d33r(14,ii) + dt32r*d33r(15,ii) + &
               dt13r*d33r(16,ii) + dt23r*d33r(17,ii) + dt33r*d33r(18,ii)
    dfedzikr = dt11r*d33r(19,ii) + dt21r*d33r(20,ii) + dt31r*d33r(21,ii) + &
               dt12r*d33r(22,ii) + dt22r*d33r(23,ii) + dt32r*d33r(24,ii) + &
               dt13r*d33r(25,ii) + dt23r*d33r(26,ii) + dt33r*d33r(27,ii)
!
!  Complex - complex
!
    dfedxiki = dt11i*d33i(1,ii) + dt21i*d33i(2,ii) + dt31i*d33i(3,ii) + &
               dt12i*d33i(4,ii) + dt22i*d33i(5,ii) + dt32i*d33i(6,ii) + &
               dt13i*d33i(7,ii) + dt23i*d33i(8,ii) + dt33i*d33i(9,ii)
    dfedyiki = dt11i*d33i(10,ii) + dt21i*d33i(11,ii) + dt31i*d33i(12,ii) + &
               dt12i*d33i(13,ii) + dt22i*d33i(14,ii) + dt32i*d33i(15,ii) + &
               dt13i*d33i(16,ii) + dt23i*d33i(17,ii) + dt33i*d33i(18,ii)
    dfedziki = dt11i*d33i(19,ii) + dt21i*d33i(20,ii) + dt31i*d33i(21,ii) + &
               dt12i*d33i(22,ii) + dt22i*d33i(23,ii) + dt32i*d33i(24,ii) + &
               dt13i*d33i(25,ii) + dt23i*d33i(26,ii) + dt33i*d33i(27,ii)
!
!  Real - real - on-diagonal
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
!******************************************************************
!  Calculate j-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real - real
!
    dfedxjkr = dt11r*d33r(28,ii) + dt21r*d33r(29,ii) + dt31r*d33r(30,ii) +  &
               dt12r*d33r(31,ii) + dt22r*d33r(32,ii) + dt32r*d33r(33,ii) +  &
               dt13r*d33r(34,ii) + dt23r*d33r(35,ii) + dt33r*d33r(36,ii)
    dfedyjkr = dt11r*d33r(37,ii) + dt21r*d33r(38,ii) + dt31r*d33r(39,ii) +  &
               dt12r*d33r(40,ii) + dt22r*d33r(41,ii) + dt32r*d33r(42,ii) +  &
               dt13r*d33r(43,ii) + dt23r*d33r(44,ii) + dt33r*d33r(45,ii)
    dfedzjkr = dt11r*d33r(46,ii) + dt21r*d33r(47,ii) + dt31r*d33r(48,ii) +  &
               dt12r*d33r(49,ii) + dt22r*d33r(50,ii) + dt32r*d33r(51,ii) +  &
               dt13r*d33r(52,ii) + dt23r*d33r(53,ii) + dt33r*d33r(54,ii)
!
!  Complex - complex
!
    dfedxjki = dt11i*d33i(28,ii) + dt21i*d33i(29,ii) + dt31i*d33i(30,ii) +  &
               dt12i*d33i(31,ii) + dt22i*d33i(32,ii) + dt32i*d33i(33,ii) +  &
               dt13i*d33i(34,ii) + dt23i*d33i(35,ii) + dt33i*d33i(36,ii)
    dfedyjki = dt11i*d33i(37,ii) + dt21i*d33i(38,ii) + dt31i*d33i(39,ii) +  &
               dt12i*d33i(40,ii) + dt22i*d33i(41,ii) + dt32i*d33i(42,ii) +  &
               dt13i*d33i(43,ii) + dt23i*d33i(44,ii) + dt33i*d33i(45,ii)
    dfedzjki = dt11i*d33i(46,ii) + dt21i*d33i(47,ii) + dt31i*d33i(48,ii) +  &
               dt12i*d33i(49,ii) + dt22i*d33i(50,ii) + dt32i*d33i(51,ii) +  &
               dt13i*d33i(52,ii) + dt23i*d33i(53,ii) + dt33i*d33i(54,ii)
!
!  Real - real - off-diagonal
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
    if (lzsisa) then
      indi = 3*(iocptr(i)-1)
      ix = indi + 1
      iy = indi + 2
      iz = indi + 3
      indj = 3*(iocptr(j)-1)
      jx = indj + 1
      jy = indj + 2
      jz = indj + 3
      indk = 3*(iocptr(k)-1)
      kx = indk + 1
      ky = indk + 2
      kz = indk + 3
      do jj = 1,nstrains
        strderv(jj) = strderv(jj) - (dfedxikr + dfedxiki + dfedxik)*dsqh(ix,jj)
        strderv(jj) = strderv(jj) - (dfedyikr + dfedyiki + dfedyik)*dsqh(iy,jj)
        strderv(jj) = strderv(jj) - (dfedzikr + dfedziki + dfedzik)*dsqh(iz,jj)
        strderv(jj) = strderv(jj) - (dfedxjkr + dfedxjki + dfedxjk)*dsqh(jx,jj)
        strderv(jj) = strderv(jj) - (dfedyjkr + dfedyjki + dfedyjk)*dsqh(jy,jj)
        strderv(jj) = strderv(jj) - (dfedzjkr + dfedzjki + dfedzjk)*dsqh(jz,jj)
        strderv(jj) = strderv(jj) + (dfedxikr + dfedxiki + dfedxjkr + dfedxjki + dfedxik + dfedxjk)*dsqh(kx,jj)
        strderv(jj) = strderv(jj) + (dfedyikr + dfedyiki + dfedyjkr + dfedyjki + dfedyik + dfedyjk)*dsqh(ky,jj)
        strderv(jj) = strderv(jj) + (dfedzikr + dfedziki + dfedzjkr + dfedzjki + dfedzik + dfedzjk)*dsqh(kz,jj)
      enddo
    else
      xdrv(i) = xdrv(i) + dfedxikr + dfedxiki + dfedxik
      ydrv(i) = ydrv(i) + dfedyikr + dfedyiki + dfedyik
      zdrv(i) = zdrv(i) + dfedzikr + dfedziki + dfedzik
      xdrv(j) = xdrv(j) + dfedxjkr + dfedxjki + dfedxjk
      ydrv(j) = ydrv(j) + dfedyjkr + dfedyjki + dfedyjk
      zdrv(j) = zdrv(j) + dfedzjkr + dfedzjki + dfedzjk
      xdrv(k) = xdrv(k) - dfedxikr - dfedxiki - dfedxjkr - dfedxjki - dfedxik - dfedxjk
      ydrv(k) = ydrv(k) - dfedyikr - dfedyiki - dfedyjkr - dfedyjki - dfedyik - dfedyjk
      zdrv(k) = zdrv(k) - dfedzikr - dfedziki - dfedzjkr - dfedzjki - dfedzik - dfedzjk
    endif
  enddo
!
  return
  end
