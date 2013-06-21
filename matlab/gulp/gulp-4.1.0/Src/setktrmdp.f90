  subroutine setktrmdp
!
!  Calculates the reciprocal space terms needed for the 
!  third derivatives for use in the polarisation energy
!  derivatives
!
!   6/00 Created from setktrmd3
!   2/01 Trapping of underflow added
!   2/01 Modifications for 2-D added
!  12/02 Looping modified to handle extreme angles
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
  use constants
  use current
  use ksample
  use kspace
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: jj
  integer(i4)        :: kk
  integer(i4)        :: max1l
  integer(i4)        :: max1l1
  integer(i4)        :: max1u
  integer(i4)        :: max2l
  integer(i4)        :: max2l1
  integer(i4)        :: max2u
  integer(i4)        :: max3l
  integer(i4)        :: max3l1
  integer(i4)        :: max3u
  integer(i4)        :: nkaddx
  integer(i4)        :: nkaddy
  integer(i4)        :: nkaddz
  integer(i4)        :: nradd
  real(dp)           :: anglemax
  real(dp)           :: anglemin
  real(dp)           :: arge
  real(dp)           :: perpk
  real(dp)           :: projk
  real(dp)           :: rk2
  real(dp)           :: rk1x
  real(dp)           :: rk1y
  real(dp)           :: rk1z
  real(dp)           :: rk2x
  real(dp)           :: rk2y
  real(dp)           :: rk2z
  real(dp)           :: rk3x
  real(dp)           :: rk3y
  real(dp)           :: rk3z
  real(dp)           :: rkk1
  real(dp)           :: rkk2
  real(dp)           :: rkk3
  real(dp)           :: rrk2
  real(dp)           :: rlmx
  real(dp)           :: rlmx2
  real(dp)           :: rlmxy
  real(dp)           :: ruk2x
  real(dp)           :: ruk2y
  real(dp)           :: ruk2z
  real(dp)           :: ruk3x
  real(dp)           :: ruk3y
  real(dp)           :: ruk3z
  real(dp)           :: xke
  real(dp)           :: yke
  real(dp)           :: zke
  real(dp)           :: xkf
  real(dp)           :: ykf
  real(dp)           :: zkf
  real(dp)           :: xpon
  real(dp)           :: zero
  real(dp)           :: xxk
  real(dp)           :: xxk2
  real(dp)           :: yyk
  real(dp)           :: yyk2
  real(dp)           :: zzk
!
!  Initialise local variables
!
  zero = 1.0d-10
!
!  Assign local variables
!
  rk1x = kv(1,1)
  rk1y = kv(2,1)
  rk1z = kv(3,1)
  rk2x = kv(1,2)
  rk2y = kv(2,2)
  rk2z = kv(3,2)
  rk3x = kv(1,3)
  rk3y = kv(2,3)
  rk3z = kv(3,3)
  rkk1 = rk1x*rk1x + rk1y*rk1y + rk1z*rk1z
  rkk2 = rk2x*rk2x + rk2y*rk2y + rk2z*rk2z
  rkk3 = rk3x*rk3x + rk3y*rk3y + rk3z*rk3z
  rkk1 = sqrt(rkk1)
  rkk1 = 1.0_dp/rkk1
  if (ndim.eq.3) then
    rkk2 = 1.0_dp/rkk2
    rkk3 = 1.0_dp/rkk3
  elseif (ndim.eq.2) then
    rkk2 = 1.0_dp/rkk2
    rkk3 = 0.0_dp
  elseif (ndim.eq.1) then
    rkk2 = 0.0_dp
    rkk3 = 0.0_dp
  endif
  ruk2x = rkk2*rk2x
  ruk2y = rkk2*rk2y
  ruk2z = rkk2*rk2z
  ruk3x = rkk3*rk3x
  ruk3y = rkk3*rk3y
  ruk3z = rkk3*rk3z
!
!  Set amounts to add to looping indices
!
  if (ndim.eq.3) then
    anglemax = max(alpha,beta)
    anglemax = max(anglemax,gamma)
    anglemin = min(alpha,beta)
    anglemin = min(anglemin,gamma)
    if (anglemax.gt.150.0_dp.or.anglemin.lt.30.0_dp) then
      nkaddx = 8
      nkaddy = 8
      nkaddz = 1
    elseif (anglemax.gt.140.0_dp.or.anglemin.lt.40.0_dp) then
      nkaddx = 7
      nkaddy = 7
      nkaddz = 1
    elseif (anglemax.gt.130.0_dp.or.anglemin.lt.50.0_dp) then
      nkaddx = 6
      nkaddy = 6
      nkaddz = 1
    elseif (anglemax.gt.120.0_dp.or.anglemin.lt.60.0_dp) then
      nkaddx = 5
      nkaddy = 5
      nkaddz = 1
    elseif (anglemax.gt.110.0_dp.or.anglemin.lt.70.0_dp) then
      nkaddx = 4
      nkaddy = 4
      nkaddz = 1
    elseif (anglemax.gt.100.0_dp.or.anglemin.lt.80.0_dp) then
      nkaddx = 3
      nkaddy = 3
      nkaddz = 1
    else
      nkaddx = 2
      nkaddy = 2
      nkaddz = 1
    endif
  elseif (ndim.eq.2) then
    if (alpha.gt.150.0_dp.or.alpha.lt.30.0_dp) then
      nkaddx = 7
      nkaddy = 1
    elseif (alpha.gt.140.0_dp.or.alpha.lt.40.0_dp) then
      nkaddx = 6
      nkaddy = 1
    elseif (alpha.gt.130.0_dp.or.alpha.lt.50.0_dp) then
      nkaddx = 5
      nkaddy = 1
    elseif (alpha.gt.120.0_dp.or.alpha.lt.60.0_dp) then
      nkaddx = 4
      nkaddy = 1
    elseif (alpha.gt.110.0_dp.or.alpha.lt.70.0_dp) then
      nkaddx = 3
      nkaddy = 1
    else
      nkaddx = 2
      nkaddy = 1
    endif
    nkaddz = 0
  elseif (ndim.eq.1) then
    nkaddx = 1
    nkaddy = 0
    nkaddz = 0
  endif
!
  rlmx = rradmax
  rlmx2 = rlmx*rlmx
  nkvec = 0
!*************************
!  Find valid k vectors  *
!*************************
!
!  Determine maximum looping indices, allowing for k point
!
  if (lra) then
    nradd = 1
    max1u = rlmx*rkk1 + nradd
    max1l = rlmx*rkk1 + nradd
    max1l1 = max1l + 1
    xxk = - max1l1*rk1x
    do ii = - max1l,max1u
      xxk = xxk + rk1x
      if (abs(xxk).lt.1.0d-10) xxk = 0.0_dp
      if (abs(xxk).lt.rlmx) then
        xxk2 = xxk*xxk
        rlmxy = rlmx2 - xxk2
        rlmxy = sqrt(rlmxy)
        max2u = rlmxy*rkk2 + nkaddy
        max2l = max2u
        max2l1 = max2l + 1
!
        yyk = - max2l1*rk2y
        do jj = - max2l,max2u
          yyk = yyk + rk2y
          if (abs(yyk).lt.1.0d-10) yyk = 0.0_dp
!
          yyk2 = yyk*yyk
          perpk = rlmx2 - xxk2 - yyk2
          if (perpk.ge.0.0_dp) then
            perpk = sqrt(perpk)
            max3u = perpk*rkk3 + nkaddz
            max3l = max3u
            max3l1 = max3l + 1
!
            zzk = - max3l1*rk3z
            do kk = - max3l,max3u
              zzk = zzk + rk3z
              if (abs(zzk).lt.1.0d-10) zzk = 0.0_dp
              rk2 = xxk2 + yyk2 + zzk*zzk
!
!  Test for zero wavevector and exclude vectors outside the search
!  region, given by rradmx.
!
              if (rk2.gt.zero.and.rk2.le.rlmx2) then
                nkvec = nkvec + 1
                if (nkvec.gt.maxkvec) then
                  maxkvec = nkvec + 100
                  call changemaxkvec
                endif
                xrk(nkvec) = xxk
                yrk(nkvec) = yyk
                zrk(nkvec) = zzk
              endif
!
!  End of ii,jj,kk loops over reciprocal lattice vectors
!
            enddo
          endif
        enddo
      endif
    enddo
  else
    max1u = rlmx*rkk1 + nkaddx
    max1l = rlmx*rkk1 + nkaddx
    max1l1 = max1l + 1
!
    xke = - max1l1*rk1x
    yke = - max1l1*rk1y
    zke = - max1l1*rk1z
    do ii = - max1l,max1u
      xke = xke + rk1x
      yke = yke + rk1y
      zke = zke + rk1z
!
      projk = xke*ruk2x + yke*ruk2y + zke*ruk2z
      max2u = (rlmx-projk)*rkk2 + nkaddy
      max2l = (rlmx+projk)*rkk2 + nkaddy
      max2l1 = max2l + 1
!
      xkf = xke - max2l1*rk2x
      ykf = yke - max2l1*rk2y
      zkf = zke - max2l1*rk2z
      do jj = - max2l,max2u
        xkf = xkf + rk2x
        ykf = ykf + rk2y
        zkf = zkf + rk2z
!
        projk = xkf*ruk3x + ykf*ruk3y + zkf*ruk3z
        perpk = xkf*xkf + ykf*ykf + zkf*zkf
        perpk = perpk - projk*projk
        perpk = rlmx2 - perpk
        if (perpk.ge.0.0_dp) then
          perpk = sqrt(perpk)
          max3u = (perpk-projk)*rkk3 + nkaddz
          max3l = (perpk+projk)*rkk3 + nkaddz
          max3l1 = max3l + 1
!
          xxk = xkf - max3l1*rk3x
          yyk = ykf - max3l1*rk3y
          zzk = zkf - max3l1*rk3z
          do kk = - max3l,max3u
            xxk = xxk + rk3x
            yyk = yyk + rk3y
            zzk = zzk + rk3z
            if (abs(xxk).lt.1.0d-10) xxk = 0.0_dp
            if (abs(yyk).lt.1.0d-10) yyk = 0.0_dp
            if (abs(zzk).lt.1.0d-10) zzk = 0.0_dp
            rk2 = xxk*xxk + yyk*yyk + zzk*zzk
!
!  Test for zero wavevector and exclude vectors outside the search
!  region, given by rradmx.
!
            if (rk2.gt.zero.and.rk2.le.rlmx2) then
              nkvec = nkvec + 1
              if (nkvec.gt.maxkvec) then
                maxkvec = nkvec + 100
                call changemaxkvec
              endif
              xrk(nkvec) = xxk
              yrk(nkvec) = yyk
              zrk(nkvec) = zzk
            endif
!
!  End of ii,jj,kk loops over reciprocal lattice vectors
!
          enddo
        endif
      enddo
    enddo
  endif
!
!  Generate reciprocal space terms
!
  if (ndim.eq.3) then
    do i = 1,nkvec
      rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
      arge = - rk2*eta4
      xpon = exp(arge)
      rrk2 = 1.0_dp/rk2
      ktrm(i) = xpon*vol4pi*rrk2*angstoev
      if (lstr) ktrms(i) = - 2.0_dp*ktrm(i)*(4.0_dp*eta+rk2)*eta4*rrk2
    enddo
  elseif (ndim.eq.2) then
    do i = 1,nkvec
      rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i)
      kmod(i) = sqrt(rk2)
      ktrm(i) = 0.5_dp*vol4pi*angstoev/kmod(i)
    enddo
  endif
!
  return
  end
