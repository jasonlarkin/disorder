  subroutine setktrmd3(nkp,nkvec0,trm6,trm0,trm20,trm60,trm62,trm620)
!
!  Calculates the reciprocal space terms needed for the 
!  third derivatives
!
!  nkp    = k point to be calculated
!
!  The use of right angled saving must be disabled in kindex when
!  calling this routine as the k point breaks the mirror plane
!  symmetry.
!
!   9/97 Created from recip3d3
!   3/01 Modified to accomodate 2-D calculation
!  12/02 Looping extended for extreme angle case
!  11/04 Intent added and sqrt pi taken from module
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
  use control
  use current
  use ksample
  use kspace
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nkp
  integer(i4), intent(out) :: nkvec0
  real(dp),    intent(out) :: trm6(*)
  real(dp),    intent(out) :: trm0(*)
  real(dp),    intent(out) :: trm20(*)
  real(dp),    intent(out) :: trm60(*)
  real(dp),    intent(out) :: trm62(*)
  real(dp),    intent(out) :: trm620(*)
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
  logical            :: lc6loc
  logical            :: lgamma
  real(dp)           :: anglemax
  real(dp)           :: anglemin
  real(dp)           :: arge
  real(dp)           :: c6t1
  real(dp)           :: c6t2
  real(dp)           :: c6t3
  real(dp)           :: c6t4
  real(dp)           :: derfc
  real(dp)           :: perpk
  real(dp)           :: projk
  real(dp)           :: rk
  real(dp)           :: rk2
  real(dp)           :: rk20
  real(dp)           :: rk1x
  real(dp)           :: rk1y
  real(dp)           :: rk1z
  real(dp)           :: rk2x
  real(dp)           :: rk2y
  real(dp)           :: rk2z
  real(dp)           :: rk3x
  real(dp)           :: rk3y
  real(dp)           :: rk3z
  real(dp)           :: rketa2
  real(dp)           :: rkk1
  real(dp)           :: rkk2
  real(dp)           :: rkk3
  real(dp)           :: rkv
  real(dp)           :: rkv2
  real(dp)           :: rrk2
  real(dp)           :: rlmx
  real(dp)           :: rlmx2
  real(dp)           :: rlmxy
  real(dp)           :: ruk1x
  real(dp)           :: ruk1y
  real(dp)           :: ruk1z
  real(dp)           :: ruk2x
  real(dp)           :: ruk2y
  real(dp)           :: ruk2z
  real(dp)           :: ruk3x
  real(dp)           :: ruk3y
  real(dp)           :: ruk3z
  real(dp)           :: xpon
  real(dp)           :: xk
  real(dp)           :: yk
  real(dp)           :: zk
  real(dp)           :: xkd
  real(dp)           :: ykd
  real(dp)           :: zkd
  real(dp)           :: xke
  real(dp)           :: yke
  real(dp)           :: zke
  real(dp)           :: xkf
  real(dp)           :: ykf
  real(dp)           :: zkf
  real(dp)           :: xkv
  real(dp)           :: ykv
  real(dp)           :: zkv
  real(dp)           :: xxk
  real(dp)           :: xxk2
  real(dp)           :: yyk
  real(dp)           :: yyk2
  real(dp)           :: zzk
  real(dp)           :: zero
!
!  Initialise local variables
!
  zero = 1.0d-10
  lc6loc = (lc6.and.ndim.eq.3)
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
  rkk2 = sqrt(rkk2)
  rkk3 = sqrt(rkk3)
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
  ruk1x = rkk1*rk1x
  ruk1y = rkk1*rk1y
  ruk1z = rkk1*rk1z
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
!  Calculate basic k vector
!
  xk = xkpt(nkp)
  yk = ykpt(nkp)
  zk = zkpt(nkp)
  if (ndim.eq.3) then
    lgamma = ((abs(xk)+abs(yk)+abs(zk)).lt.1.0d-8)
  elseif (ndim.eq.2) then
    lgamma = ((abs(xk)+abs(yk)).lt.1.0d-8)
  elseif (ndim.eq.1) then
    lgamma = (abs(xk).lt.1.0d-8)
  endif
  xkv = xk*rk1x + yk*rk2x + zk*rk3x
  ykv = xk*rk1y + yk*rk2y + zk*rk3y
  zkv = xk*rk1z + yk*rk2z + zk*rk3z
!
  rkv2 = xkv*xkv + ykv*ykv + zkv*zkv
  rkv = sqrt(rkv2)
  rlmx = rkv + rradmax
  rlmx2 = rlmx*rlmx
  nkvec = 0
  nkvec0 = 0
!*************************
!  Find valid k vectors  *
!*************************
!
!  Determine maximum looping indices, allowing for k point
!
  if (lra) then
    nradd = 2
    max1u = (rlmx-xkv)*rkk1 + nradd
    max1l = (rlmx+xkv)*rkk1 + nradd
    max1l1 = max1l + 1
    xxk = xkv-max1l1*rk1x
    do ii = -max1l,max1u
      xxk = xxk + rk1x
      if (abs(xxk).lt.rlmx) then
        xxk2 = xxk*xxk
        rlmxy = rlmx2 - xxk2
        rlmxy = sqrt(rlmxy)
        max2u = (rlmxy-ykv)*rkk2 + nkaddy
        max2l = (rlmxy+ykv)*rkk2 + nkaddy
        max2l1 = max2l + 1
!
        yyk = ykv - max2l1*rk2y
        do jj = -max2l,max2u
          yyk = yyk + rk2y
!
          yyk2 = yyk*yyk
          perpk = rlmx2 - xxk2 - yyk2
          if (perpk.ge.0.0_dp) then
            perpk = sqrt(perpk)
            max3u = (perpk-zkv)*rkk3 + nkaddz
            max3l = (perpk+zkv)*rkk3 + nkaddz
            max3l1 = max3l+1
!
            zzk = zkv-max3l1*rk3z
            do kk = -max3l,max3u
              zzk = zzk + rk3z
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
              xkd = xxk - xkv
              ykd = yyk - ykv
              zkd = zzk - zkv
              rk20 = xkd*xkd + ykd*ykd + zkd*zkd
              if (rk20.gt.zero.and.rk20.le.rlmx2) then
                nkvec0 = nkvec0 + 1
                if (nkvec0.gt.maxkvec) then
                  maxkvec = nkvec0 + 100
                  call changemaxkvec
                endif
                xrk0(nkvec0) = xkd
                yrk0(nkvec0) = ykd
                zrk0(nkvec0) = zkd
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
    projk = xkv*ruk1x + ykv*ruk1y + zkv*ruk1z
    max1u = (rlmx-projk)*rkk1 + nkaddx
    max1l = (rlmx+projk)*rkk1 + nkaddx
    max1l1 = max1l + 1
!
    xke = xkv - max1l1*rk1x
    yke = ykv - max1l1*rk1y
    zke = zkv - max1l1*rk1z
    do ii = -max1l,max1u
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
      do jj = -max2l,max2u
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
          do kk = -max3l,max3u
            xxk = xxk + rk3x
            yyk = yyk + rk3y
            zzk = zzk + rk3z
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
            xkd = xxk - xkv
            ykd = yyk - ykv
            zkd = zzk - zkv
            rk20 = xkd*xkd + ykd*ykd + zkd*zkd
            if (rk20.gt.zero.and.rk20.le.rlmx2) then
              nkvec0 = nkvec0 + 1
              if (nkvec0.gt.maxkvec) then
                maxkvec = nkvec0 + 100
                call changemaxkvec
              endif
              xrk0(nkvec0) = xkd
              yrk0(nkvec0) = ykd
              zrk0(nkvec0) = zkd
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
!  Generate reciprocal space term
!
  if (lc6loc) then
    c6t1 = vol4pi*sqrtpi/48.0_dp
!
!  Reciprocal space self term
!
    do i = 1,nkvec
      rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
      arge = -rk2*eta4
      xpon = exp(arge)
      rrk2 = 1.0d0/rk2
      ktrm(i) = xpon*vol4pi*rrk2*angstoev
      rk = sqrt(rk2)
      rketa2 = 0.5_dp*rk/seta
      c6t2 = sqrtpi*derfc(rketa2)
      rketa2 = 1.0_dp/rketa2
      c6t3 = 0.5_dp*rketa2**3-rketa2
      c6t3 = c6t3*xpon
      c6t4 = c6t1*rk2*rk
      trm6(i) = c6t4*(c6t2+c6t3)
      if (lstr) then
        ktrms(i) = -2.0_dp*ktrm(i)*(4.0_dp*eta+rk2)*eta4*rrk2
        trm62(i) = 3.0_dp*trm6(i)*rrk2
        trm62(i) = trm62(i)-c6t4*xpon*(12.0_dp*eta*seta*rrk2*rrk2)/rk
      endif
    enddo
    if (.not.lgamma) then
      do i = 1,nkvec0
        rk2 = xrk0(i)*xrk0(i) + yrk0(i)*yrk0(i) + zrk0(i)*zrk0(i)
        arge = -rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        trm0(i) = xpon*vol4pi*rrk2*angstoev
        rk = sqrt(rk2)
        rketa2 = 0.5_dp*rk/seta
        c6t2 = sqrtpi*derfc(rketa2)
        rketa2 = 1.0_dp/rketa2
        c6t3 = 0.5d0*rketa2**3-rketa2
        c6t3 = c6t3*xpon
        c6t4 = c6t1*rk2*rk
        trm60(i) = c6t4*(c6t2+c6t3)
        if (lstr) then
          trm20(i) = -2.0_dp*trm0(i)*(4.0_dp*eta+rk2)*eta4*rrk2
          trm620(i) = 3.0_dp*trm60(i)*rrk2
          trm620(i) = trm620(i)-c6t4*xpon*(12.0_dp*eta*seta*rrk2*rrk2)/rk
        endif
      enddo
    endif
  else
    if (ndim.eq.3) then
      do i = 1,nkvec
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
        arge = -rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(i) = xpon*vol4pi*rrk2*angstoev
        if (lstr) ktrms(i) = -2.0d0*ktrm(i)*(4.0d0*eta+rk2)*eta4*rrk2
      enddo
      if (.not.lgamma) then
        do i = 1,nkvec0
          rk2 = xrk0(i)*xrk0(i)+yrk0(i)*yrk0(i)+zrk0(i)*zrk0(i)
          arge = -rk2*eta4
          xpon = exp(arge)
          rrk2 = 1.0_dp/rk2
          trm0(i) = xpon*vol4pi*rrk2*angstoev
          if (lstr) trm20(i) = -2.0_dp*trm0(i)*(4.0d0*eta+rk2)*eta4*rrk2
        enddo
      endif
    elseif (ndim.eq.2) then
      do i = 1,nkvec
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i)
        kmod(i) = sqrt(rk2)
        ktrm(i) = 0.5_dp*vol4pi*angstoev/kmod(i)
      enddo
      if (.not.lgamma) then
        do i = 1,nkvec0
          rk2 = xrk0(i)*xrk0(i) + yrk0(i)*yrk0(i)
          trm20(i) = sqrt(rk2)
          trm0(i) = 0.5_dp*vol4pi*angstoev/trm20(i)
        enddo
      endif
    endif
  endif
!
  return
  end
