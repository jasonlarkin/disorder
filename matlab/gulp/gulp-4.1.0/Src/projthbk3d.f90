  subroutine projthbk3d(i,nthbk,nptrthbk,d33,d33r,dt11,dt21,dt31,dt22,dt32,dt33,lzsisa)
!
!  Calculates the free energy derivative contribution from the
!  third atom k for the i-i dynamical matrix block (on diagonal)
!
!   6/98 Created from projthbk3
!   8/99 zsisa corrections added
!  12/07 Unused variables removed
!  11/12 Unused argument iocptr removed
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, November 2012
!
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)         :: i
  integer(i4)         :: nptrthbk(*)
  integer(i4)         :: nthbk
  logical             :: lzsisa
  real(dp)            :: d33(54,*)
  real(dp)            :: d33r(54,*)
  real(dp)            :: dt11
  real(dp)            :: dt21
  real(dp)            :: dt31
  real(dp)            :: dt22
  real(dp)            :: dt32
  real(dp)            :: dt33
!
!  Local variables
!
  integer(i4)         :: ii
  integer(i4)         :: k
  real(dp)            :: dfedxi
  real(dp)            :: dfedyi
  real(dp)            :: dfedzi
  real(dp)            :: dfedxj
  real(dp)            :: dfedyj
  real(dp)            :: dfedzj
!
!  Loop over three body k atoms
!
  do ii = 1,nthbk
    k = nptrthbk(ii)
!
!  Calculate difference between phased on-diagonal elements and pure real
!
!******************************************************************
!  Calculate i-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real - real
!
    dfedxi = dt11*(d33r(1,ii) - d33(1,ii)) +  &
             dt21*(d33r(2,ii) - d33(2,ii)) +  &
             dt31*(d33r(3,ii) - d33(3,ii)) +  &
             dt21*(d33r(4,ii) - d33(4,ii)) +  &
             dt22*(d33r(5,ii) - d33(5,ii)) +  &
             dt32*(d33r(6,ii) - d33(6,ii)) +  &
             dt31*(d33r(7,ii) - d33(7,ii)) +  &
             dt32*(d33r(8,ii) - d33(8,ii)) +  &
             dt33*(d33r(9,ii) - d33(9,ii))
    dfedyi = dt11*(d33r(10,ii) - d33(10,ii)) +  &
             dt21*(d33r(11,ii) - d33(11,ii)) +  &
             dt31*(d33r(12,ii) - d33(12,ii)) +  &
             dt21*(d33r(13,ii) - d33(13,ii)) +  &
             dt22*(d33r(14,ii) - d33(14,ii)) +  &
             dt32*(d33r(15,ii) - d33(15,ii)) +  &
             dt31*(d33r(16,ii) - d33(16,ii)) +  &
             dt32*(d33r(17,ii) - d33(17,ii)) +  &
             dt33*(d33r(18,ii) - d33(18,ii))
    dfedzi = dt11*(d33r(19,ii) - d33(19,ii)) +  &
             dt21*(d33r(20,ii) - d33(20,ii)) +  &
             dt31*(d33r(21,ii) - d33(21,ii)) +  &
             dt21*(d33r(22,ii) - d33(22,ii)) +  &
             dt22*(d33r(23,ii) - d33(23,ii)) +  &
             dt32*(d33r(24,ii) - d33(24,ii)) +  &
             dt31*(d33r(25,ii) - d33(25,ii)) +  &
             dt32*(d33r(26,ii) - d33(26,ii)) +  &
             dt33*(d33r(27,ii) - d33(27,ii))
!******************************************************************
!  Calculate j - k component  -  no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real  -  real
!
    dfedxj = dt11*(d33r(28,ii) - d33(28,ii)) +  &
             dt21*(d33r(29,ii) - d33(29,ii)) +  &
             dt31*(d33r(30,ii) - d33(30,ii)) +  &
             dt21*(d33r(31,ii) - d33(31,ii)) +  &
             dt22*(d33r(32,ii) - d33(32,ii)) +  &
             dt32*(d33r(33,ii) - d33(33,ii)) +  &
             dt31*(d33r(34,ii) - d33(34,ii)) +  &
             dt32*(d33r(35,ii) - d33(35,ii)) +  &
             dt33*(d33r(36,ii) - d33(36,ii))
    dfedyj = dt11*(d33r(37,ii) - d33(37,ii)) +  &
             dt21*(d33r(38,ii) - d33(38,ii)) +  &
             dt31*(d33r(39,ii) - d33(39,ii)) +  &
             dt21*(d33r(40,ii) - d33(40,ii)) +  &
             dt22*(d33r(41,ii) - d33(41,ii)) +  &
             dt32*(d33r(42,ii) - d33(42,ii)) +  &
             dt31*(d33r(43,ii) - d33(43,ii)) +  &
             dt32*(d33r(44,ii) - d33(44,ii)) +  &
             dt33*(d33r(45,ii) - d33(45,ii))
    dfedzj = dt11*(d33r(46,ii) - d33(46,ii)) +  &
             dt21*(d33r(47,ii) - d33(47,ii)) +  &
             dt31*(d33r(48,ii) - d33(48,ii)) +  &
             dt21*(d33r(49,ii) - d33(49,ii)) +  &
             dt22*(d33r(50,ii) - d33(50,ii)) +  &
             dt32*(d33r(51,ii) - d33(51,ii)) +  &
             dt31*(d33r(52,ii) - d33(52,ii)) +  &
             dt32*(d33r(53,ii) - d33(53,ii)) +  &
             dt33*(d33r(54,ii) - d33(54,ii))
!
!  Add on derivative contributions
!
    if (.not.lzsisa) then
      xdrv(i) = xdrv(i) + dfedxi + dfedxj
      ydrv(i) = ydrv(i) + dfedyi + dfedyj
      zdrv(i) = zdrv(i) + dfedzi + dfedzj
      xdrv(k) = xdrv(k) - dfedxi - dfedxj
      ydrv(k) = ydrv(k) - dfedyi - dfedyj
      zdrv(k) = zdrv(k) - dfedzi - dfedzj
    endif
  enddo
!
  return
  end
