  subroutine kweight(fx,fy,fz,wght,npgrp)
!
!  Determine the weighting factor for a point in k space
!  As Gulp always converts a unit cell to the primitive
!  form only the primitive Patterson groups are used.
!
!  fx,fy,fz = fractional coordinates of point in k space
!  wght     = weighting factor, initially should be 1.0
!             for a general position
!  npgrp    = the index number of the primitive Patterson group
!
!  10/02 Modified to allow for machine precision effects
!  12/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)       :: npgrp
  real(dp)          :: fx
  real(dp)          :: fy
  real(dp)          :: fz
  real(dp)          :: wght
!
!  Local variables
!
  integer(i4)       :: icase(14)
  integer(i4)       :: iptr
  logical           :: lx
  logical           :: ly
  logical           :: lz
  real(dp)          :: diff
  real(dp)          :: wxy(14)
  real(dp)          :: wyz(14)
  real(dp)          :: wxyz(14)
!
  data icase/1,2,3,4,3,5,6,6,5,6,3,3,7,7/
  data wxy/1.0_dp,1.0_dp,1.0_dp,0.5_dp,0.5_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp, &
           1.0_dp,1.0_dp,0.5_dp,0.5_dp,0.5_dp/
  data wyz/1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp, &
           1.0_dp,1.0_dp,0.5_dp,0.5_dp,0.5_dp/
  data wxyz/1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp,1.0_dp, &
            1.0_dp,0.333333333333333_dp,0.166666666666667_dp, &
            0.166666666666667_dp,0.166666666666667_dp/
!
  wght = 1.0_dp
  iptr = icase(npgrp)
  lx = (abs(fx).lt.1.0d-8.or.abs(fx-0.5_dp).lt.1.0d-8)
  ly = (abs(fy).lt.1.0d-8.or.abs(fy-0.5_dp).lt.1.0d-8)
  lz = (abs(fz).lt.1.0d-8.or.abs(fz-0.5_dp).lt.1.0d-8)
  if (iptr.eq.1) then
!
!  Case 1 : ( P -1 )
!
    if (lx) wght = 0.5_dp*wght
  elseif (iptr.eq.2) then
!
!  Case 2 : ( P 2/M)
!
    if (lx.and.lz) wght = 0.5_dp*wght
    if (ly) wght = 0.5_dp*wght
  elseif (iptr.eq.4) then
!
!  Case 4 : ( P 4/M)
!
    if (lx.and.ly) then
      wght = 0.5_dp*wght
      if (fx.eq.fy) wght = wght*wxy(npgrp)
    endif
    if (lz) wght = 0.5_dp*wght
  elseif (iptr.eq.5) then
!
!  Case 5 : ( P 6/M, P -3)
!
    if (fx.eq.0.5_dp.and.fy.eq.0.5_dp) then
      if (npgrp.eq.6.and.fz.eq.0.5_dp) wght = 0.5_dp*wght
      if (npgrp.eq.9) wght = 0.5_dp*wght
    endif
    if (lz.and.npgrp.eq.9) wght = 0.5_dp*wght
  elseif (iptr.eq.6) then
!
!  Case 6 : ( P -3 M 1 : P -3 1 M : P 6/M M M)
!
    diff = abs(1.0_dp-fx-fy)
    if (npgrp.eq.10) then
      if (diff.lt.1.0d-12) wght = 0.5_dp*wght
      if (fx.eq.fy) wght = 0.5_dp*wght
      if (lz) wght = 0.5_dp*wght
    elseif (npgrp.eq.7) then
      if (diff.lt.1.0d-12) wght = 0.5_dp*wght
      if (fx.eq.fy.and.lz) wght = 0.5_dp*wght
    else
      if (diff.lt.1.0d-12.and.lz) then
        wght = 0.5_dp*wght
      elseif (fy.eq.0.0_dp.and.lz) then
        wght = 0.5_dp*wght
      endif
      if (fx.eq.fy) wght = 0.5_dp*wght
    endif
  elseif (iptr.eq.7) then
!
!  Case 7 : ( R -3 M, R -3)
!
    if (fx.eq.fy) then
      if (fx.eq.fz) then
        wght = wght*wxyz(npgrp)
      else
        wght = wght*wxy(npgrp)
      endif
    elseif (fy.eq.fz) then
      wght = wght*wyz(npgrp)
    endif
  else
!
!  Case 3 : ( P M M M, P 4/M M M, P M -3, P M -3 M)
!
    if (lx) wght = 0.5_dp*wght
    if (ly) wght = 0.5_dp*wght
    if (lz) wght = 0.5_dp*wght
    if (npgrp.eq.5) then
      if (fx.eq.fy) wght = wght*wxy(npgrp)
    else
      if (fx.eq.fy.and.fx.eq.fz) then
        wght = wght*wxyz(npgrp)
      else
        if (fx.eq.fy) wght = wght*wxy(npgrp)
        if (fy.eq.fz) wght = wght*wyz(npgrp)
      endif
    endif
  endif
!
  return
  end
