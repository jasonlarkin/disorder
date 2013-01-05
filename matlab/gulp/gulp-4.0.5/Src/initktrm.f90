  subroutine initktrm
!
!  Routine initialises the K terms needed by qmatrix element
!
!   8/01 Created from epot3
!   3/05 Sorting of K vectors added for 2-D case
!   9/05 lstr condition removed from ktrms
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
!  Julian Gale, NRI, Curtin University, September 2005
!
  use constants
  use control
  use current
  use kspace
  use shell
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4) :: idk
  integer(i4) :: iv
  integer(i4) :: i
  integer(i4) :: imin
  integer(i4) :: j
  integer(i4) :: k
  real(dp)    :: arge
  real(dp)    :: factor
  real(dp)    :: Gmin
  real(dp)    :: kvv(3)
  real(dp)    :: rk2
  real(dp)    :: rktemp
  real(dp)    :: rrk2
  real(dp)    :: xpon
!
!  Calculate and store k-vectors
!
  if (ndim.eq.3) then
    eta4 = 0.25_dp/eta
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      kvv(3) = kv(3,3)
      do iv = 1,nkvec
        idk = indk(iv)
        i = (idk/6400) - 40
        if (i.eq.0) then
          factor = 1.0_dp
        else
          factor = 2.0_dp
        endif
        idk = idk - (i+40)*6400
        j = (idk/80)-40
        k = idk - (j+40)*80 - 40
        xrk(iv) = i*kvv(1)
        yrk(iv) = j*kvv(2)
        zrk(iv) = k*kvv(3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = -rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        ktrms(iv) = -2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
      enddo
    else
      do iv = 1,nkvec
        idk = indk(iv)
        i = (idk/6400)-40
        idk = idk - (i+40)*6400
        j = (idk/80) - 40
        k = idk - (j+40)*80 - 40
        factor = 2.0_dp
        if (i.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (j.eq.0.and.nkangle.eq.2) then
          factor = 1.0_dp
        elseif (k.eq.0.and.nkangle.eq.3) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(iv) = i*kv(1,1) + j*kv(1,2) + k*kv(1,3)
        yrk(iv) = i*kv(2,1) + j*kv(2,2) + k*kv(2,3)
        zrk(iv) = i*kv(3,1) + j*kv(3,2) + k*kv(3,3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = -rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        ktrms(iv) = -2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
      enddo
    endif
  elseif (ndim.eq.2) then
    rpieta = 1.0_dp / sqrt(pi * eta)
    rhseta = 0.5_dp / seta
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      do iv = 1,nkvec
        idk = indk(iv)
        i = (idk/6400) - 40
        if (i.eq.0) then
          factor = 1.0_dp
        else
          factor = 2.0_dp
        endif
        idk = idk - (i+40)*6400
        j = (idk/80) - 40
        xrk(iv) = i*kvv(1)
        yrk(iv) = j*kvv(2)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv)
        kmod(iv) = sqrt(rk2)
        ktrm(iv) = 0.5_dp*vol4pi*factor/kmod(iv)
      enddo
    else
      do iv = 1,nkvec
        idk = indk(iv)
        i = (idk/6400) - 40
        idk = idk - (i+40)*6400
        j = (idk/80) - 40
        factor = 2.0_dp
        if (i.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(iv) = i*kv(1,1) + j*kv(1,2)
        yrk(iv) = i*kv(2,1) + j*kv(2,2)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv)
        kmod(iv) = sqrt(rk2)
        ktrm(iv) = 0.5_dp*vol4pi*factor/kmod(iv)
      enddo
    endif
!*******************************************
!  Sort K vectors by increasing magnitude  *
!*******************************************
    do i = 1,nkvec
      Gmin = 2.0_dp*rradmax
      do j = i,nkvec
        if (kmod(j).lt.Gmin) then
          imin = j
          Gmin = kmod(j)
        endif
      enddo
      rktemp = kmod(i)
      kmod(i) = kmod(imin)
      kmod(imin) = rktemp
      rktemp = ktrm(i)
      ktrm(i) = ktrm(imin)
      ktrm(imin) = rktemp
      rktemp = xrk(i)
      xrk(i) = xrk(imin)
      xrk(imin) = rktemp
      rktemp = yrk(i)
      yrk(i) = yrk(imin)
      yrk(imin) = rktemp
    enddo
  endif
!
  return
  end
