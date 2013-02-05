  subroutine projfork3d(nforkl,nptrfork,nptrforl,d34,d34r, &
    dt11,dt21,dt31,dt22,dt32,dt33,dsqh,iocptr,lzsisa)
!
!  Calculates the free energy derivative contribution from the
!  third atom k to fourth atom l distance for the i-i dynamical 
!  matrix block (on diagonal) in a four-body interaction.
!
!   8/98 Created from projthbk3d
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
  real(dp)           :: d34r(27,*)
  real(dp)           :: dsqh(maxd2,*)
  real(dp)           :: dt11
  real(dp)           :: dt21
  real(dp)           :: dt31
  real(dp)           :: dt22
  real(dp)           :: dt32
  real(dp)           :: dt33
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
  real(dp)           :: dfedxi
  real(dp)           :: dfedyi
  real(dp)           :: dfedzi
!
!  Loop over four body k-l pairs
!
  do ii = 1,nforkl
    k = nptrfork(ii)
    l = nptrforl(ii)
!
!  Calculate difference between phased on-diagonal elements and pure real
!
!******************************************************************
!  Calculate k-l component - no assumption that d34 is symmetric  *
!******************************************************************
!
!  Real - real
!
    dfedxi = dt11*(d34r(1,ii)-d34(1,ii)) + dt21*(d34r(2,ii)-d34(2,ii)) + &
             dt31*(d34r(3,ii)-d34(3,ii)) + dt21*(d34r(4,ii)-d34(4,ii)) + &
             dt22*(d34r(5,ii)-d34(5,ii)) + dt32*(d34r(6,ii)-d34(6,ii)) + &
             dt31*(d34r(7,ii)-d34(7,ii)) + dt32*(d34r(8,ii)-d34(8,ii)) + &
             dt33*(d34r(9,ii)-d34(9,ii))
    dfedyi = dt11*(d34r(10,ii)-d34(10,ii)) + dt21*(d34r(11,ii)-d34(11,ii)) + &
             dt31*(d34r(12,ii)-d34(12,ii)) + dt21*(d34r(13,ii)-d34(13,ii)) + &
             dt22*(d34r(14,ii)-d34(14,ii)) + dt32*(d34r(15,ii)-d34(15,ii)) + &
             dt31*(d34r(16,ii)-d34(16,ii)) + dt32*(d34r(17,ii)-d34(17,ii)) + &
             dt33*(d34r(18,ii)-d34(18,ii))
    dfedzi = dt11*(d34r(19,ii)-d34(19,ii)) + dt21*(d34r(20,ii)-d34(20,ii)) + &
             dt31*(d34r(21,ii)-d34(21,ii)) + dt21*(d34r(22,ii)-d34(22,ii)) + &
             dt22*(d34r(23,ii)-d34(23,ii)) + dt32*(d34r(24,ii)-d34(24,ii)) + &
             dt31*(d34r(25,ii)-d34(25,ii)) + dt32*(d34r(26,ii)-d34(26,ii)) + &
             dt33*(d34r(27,ii)-d34(27,ii))
!
!  Add on derivative contributions
!
    if (lzsisa) then
      indk = 3*(iocptr(k)-1)
      kx = indk + 1
      ky = indk + 2
      kz = indk + 3
      indl = 3*(iocptr(l)-1)
      lx = indl + 1
      ly = indl + 2
      lz = indl + 3
      do jj = 1,nstrains
        strderv(jj) = strderv(jj) - dfedxi*dsqh(kx,jj)
        strderv(jj) = strderv(jj) - dfedyi*dsqh(ky,jj)
        strderv(jj) = strderv(jj) - dfedzi*dsqh(kz,jj)
        strderv(jj) = strderv(jj) + dfedxi*dsqh(lx,jj)
        strderv(jj) = strderv(jj) + dfedyi*dsqh(ly,jj)
        strderv(jj) = strderv(jj) + dfedzi*dsqh(lz,jj)
      enddo
    else
      xdrv(k) = xdrv(k) + dfedxi
      ydrv(k) = ydrv(k) + dfedyi
      zdrv(k) = zdrv(k) + dfedzi
      xdrv(l) = xdrv(l) - dfedxi
      ydrv(l) = ydrv(l) - dfedyi
      zdrv(l) = zdrv(l) - dfedzi
    endif
  enddo
!
  return
  end
