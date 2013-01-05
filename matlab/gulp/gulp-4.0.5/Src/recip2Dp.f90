  subroutine recip2Dp(xkv,ykv)
!
!  Calculate phased second derivatives in reciprocal space
!  for 2-D phonon calculations.
!
!  nkp    = k point to be calculated
!
!  The use of right angled saving must be disabled in kindex when
!  calling this routine as the k point breaks the mirror plane
!  symmetry.
!
!   2/01 Created from recip3Dp and recip2D
!   3/01 EEM second derivatives finished
!   4/01 Passed argument changed from K point number to K point 
!        coordinate
!  12/02 nkaddx introduced to handle extreme angles
!   9/04 Modifications for generalisation of variable charges added
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
  use derivatives
  use ksample
  use kspace
  use parallel
  use shell
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  real(dp)                                  :: xkv
  real(dp)                                  :: ykv
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: ii
  integer(i4)                               :: iv
  integer(i4)                               :: ix
  integer(i4)                               :: iy
  integer(i4)                               :: iz
  integer(i4)                               :: j
  integer(i4)                               :: jj
  integer(i4)                               :: jx
  integer(i4)                               :: jy
  integer(i4)                               :: jz
  integer(i4)                               :: max1l
  integer(i4)                               :: max1l1
  integer(i4)                               :: max1u
  integer(i4)                               :: max2l
  integer(i4)                               :: max2l1
  integer(i4)                               :: max2u
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nkaddx
  integer(i4)                               :: nkvec0
  integer(i4)                               :: status
  logical                                   :: lgamma
  real(dp)                                  :: accf2
  real(dp)                                  :: arg
  real(dp)                                  :: argtest
  real(dp)                                  :: cosa
  real(dp)                                  :: cosq
  real(dp)                                  :: cosq0
  real(dp)                                  :: cputime
  real(dp)                                  :: d21i
  real(dp)                                  :: d22i
  real(dp)                                  :: d23i
  real(dp)                                  :: d24i
  real(dp)                                  :: d25i
  real(dp)                                  :: d26i
  real(dp)                                  :: d21q
  real(dp)                                  :: d22q
  real(dp)                                  :: d23q
  real(dp)                                  :: d24q
  real(dp)                                  :: d25q
  real(dp)                                  :: d26q
  real(dp)                                  :: d2self
  real(dp)                                  :: dtrm1di
  real(dp)                                  :: dtrm1dj
  real(dp)                                  :: d2trm1dij
  real(dp)                                  :: d2trm1diz
  real(dp)                                  :: d2trm1djz
  real(dp)                                  :: darg1
  real(dp)                                  :: darg2
  real(dp)                                  :: derf
  real(dp)                                  :: derfc
  real(dp)                                  :: derfc1
  real(dp)                                  :: derfc2
  real(dp)                                  :: derfez
  real(dp)                                  :: dexp1
  real(dp)                                  :: dexp2
  real(dp)                                  :: dexp3
  real(dp)                                  :: dexp4
  real(dp)                                  :: dexpz
  real(dp)                                  :: dtrm1
  real(dp)                                  :: dtrm2
  real(dp)                                  :: etaz
  real(dp)                                  :: etaz2
  real(dp)                                  :: etrm
  real(dp)                                  :: fct
  real(dp)                                  :: Gmax
  real(dp)                                  :: kexperfc
  real(dp)                                  :: kvec
  real(dp), dimension(:), allocatable       :: kmod0
  real(dp), dimension(:), allocatable       :: ktrm0
  real(dp), dimension(:), allocatable       :: ktrm3
  real(dp), dimension(:), allocatable       :: ktrm3z
  real(dp), dimension(:), allocatable       :: ktrm4
  real(dp), dimension(:), allocatable       :: ktrm4z
  real(dp)                                  :: oci
  real(dp)                                  :: ocj
  real(dp)                                  :: projk
  real(dp)                                  :: qfct
  real(dp)                                  :: qli
  real(dp)                                  :: qlj
  real(dp)                                  :: rk1x
  real(dp)                                  :: rk1y
  real(dp)                                  :: rk2
  real(dp)                                  :: rk2x
  real(dp)                                  :: rk2y
  real(dp)                                  :: rk20
  real(dp)                                  :: rkk1
  real(dp)                                  :: rkk2
  real(dp)                                  :: rkv
  real(dp)                                  :: rkv2
  real(dp)                                  :: rlmx
  real(dp)                                  :: rlmx2
  real(dp)                                  :: rlmxy
  real(dp)                                  :: ruk1x
  real(dp)                                  :: ruk1y
  real(dp)                                  :: ruk2x
  real(dp)                                  :: ruk2y
  real(dp)                                  :: sina
  real(dp)                                  :: sineq
  real(dp)                                  :: sinq0
  real(dp)                                  :: smallestG
  real(dp)                                  :: time0
  real(dp)                                  :: time1
  real(dp), dimension(:), allocatable       :: tmp1
  real(dp), dimension(:), allocatable       :: tmp2
  real(dp), dimension(:), allocatable       :: tmp3
  real(dp)                                  :: trmzz
  real(dp)                                  :: twoqv
  real(dp)                                  :: xci
  real(dp)                                  :: yci
  real(dp)                                  :: zci
  real(dp)                                  :: xd
  real(dp)                                  :: yd
  real(dp)                                  :: zd
  real(dp)                                  :: xkd
  real(dp)                                  :: ykd
  real(dp)                                  :: xke
  real(dp)                                  :: yke
  real(dp)                                  :: xkf
  real(dp)                                  :: ykf
  real(dp)                                  :: xrkk
  real(dp)                                  :: yrkk
  real(dp)                                  :: xxk
  real(dp)                                  :: xxk2
  real(dp)                                  :: yyk
  real(dp)                                  :: yyk2
  real(dp)                                  :: zero
  real(dp)                                  :: ztrm1
!
  if (lnorecip) return
  time0 = cputime()
!
!  Initialise local variables
!
  zero = 1.0d-10
!
!  Assign local variables
!
  rk1x = kv(1,1)
  rk1y = kv(2,1)
  rk2x = kv(1,2)
  rk2y = kv(2,2)
  rkk1 = rk1x*rk1x + rk1y*rk1y
  rkk2 = rk2x*rk2x + rk2y*rk2y
  rkk1 = sqrt(rkk1)
  rkk1 = 1.0_dp/rkk1
  rkk2 = sqrt(rkk2)
  rkk2 = 1.0_dp/rkk2
  ruk1x = rkk1*rk1x
  ruk1y = rkk1*rk1y
  ruk2x = rkk2*rk2x
  ruk2y = rkk2*rk2y
!
!  Set gamma point flag
!
  lgamma = ((abs(xkv)+abs(ykv)).lt.1.0d-8)
!
  rkv2 = xkv*xkv+ykv*ykv
  rkv = sqrt(rkv2)
  rlmx = rkv + rradmax
  rlmx2 = rlmx*rlmx
  nkvec = 0
  nkvec0 = 0
!
!  Define constants
!
  rpieta = 1.0_dp / sqrt(pi * eta)
  rhseta = 0.5_dp / seta
  accf2 = accf*accf
  argtest = sqrt(3.0+0.5*accf2) - sqrt(3.0)
  smallestG = min(abs(kv(1,1)),abs(kv(2,2)))
!*************************
!  Find valid k vectors  *
!*************************
!
!  Determine maximum looping indices, allowing for k point
!
  if (lra) then
    max1u = (rlmx - xkv)*rkk1 + 1
    max1l = (rlmx + xkv)*rkk1 + 1
    max1l1 = max1l + 1
    xxk = xkv - max1l1*rk1x
    do ii = - max1l,max1u
      xxk = xxk + rk1x
      if (abs(xxk).lt.rlmx) then
        xxk2 = xxk*xxk
        rlmxy = rlmx2 - xxk2
        rlmxy = sqrt(rlmxy)
        max2u = (rlmxy - ykv)*rkk2 + 1
        max2l = (rlmxy + ykv)*rkk2 + 1
        max2l1 = max2l + 1
!
        yyk = ykv - max2l1*rk2y
        do jj = - max2l,max2u
          yyk = yyk + rk2y
!
          yyk2 = yyk*yyk
          rk2 = xxk2 + yyk2
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
            zrk(nkvec) = 0.0_dp
          endif
          xkd = xxk - xkv
          ykd = yyk - ykv
          rk20 = xkd*xkd + ykd*ykd
          if (rk20.gt.zero.and.rk20.le.rlmx2) then
            nkvec0 = nkvec0 + 1
            if (nkvec0.gt.maxkvec) then
              maxkvec = nkvec0 + 100
              call changemaxkvec
            endif
            xrk0(nkvec0) = xkd
            yrk0(nkvec0) = ykd
            zrk0(nkvec0) = 0.0_dp
          endif
!
!  End of ii,jj,kk loops over reciprocal lattice vectors
!
        enddo
      endif
    enddo
  else
!
!  Set amounts to add to looping indices
!
    if (alpha.gt.150.0_dp.or.alpha.lt.30.0_dp) then
      nkaddx = 7
    elseif (alpha.gt.140.0_dp.or.alpha.lt.40.0_dp) then
      nkaddx = 6
    elseif (alpha.gt.130.0_dp.or.alpha.lt.50.0_dp) then
      nkaddx = 5
    elseif (alpha.gt.120.0_dp.or.alpha.lt.60.0_dp) then
      nkaddx = 4
    elseif (alpha.gt.110.0_dp.or.alpha.lt.70.0_dp) then
      nkaddx = 3
    else
      nkaddx = 2
    endif
    projk = xkv*ruk1x + ykv*ruk1y
    max1u = (rlmx - projk)*rkk1 + nkaddx
    max1l = (rlmx + projk)*rkk1 + nkaddx
    max1l1 = max1l + 1
!
    xke = xkv - max1l1*rk1x
    yke = ykv - max1l1*rk1y
    do ii = - max1l,max1u
      xke = xke + rk1x
      yke = yke + rk1y
!
      projk = xke*ruk2x + yke*ruk2y
      max2u = (rlmx - projk)*rkk2 + 1
      max2l = (rlmx + projk)*rkk2 + 1
      max2l1 = max2l + 1
!
      xkf = xke - max2l1*rk2x
      ykf = yke - max2l1*rk2y
      do jj = - max2l,max2u
        xkf = xkf + rk2x
        ykf = ykf + rk2y
!
        rk2 = xkf*xkf + ykf*ykf
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
          xrk(nkvec) = xkf
          yrk(nkvec) = ykf
          zrk(nkvec) = 0.0_dp
        endif
        xkd = xkf - xkv
        ykd = ykf - ykv
        rk20 = xkd*xkd + ykd*ykd
        if (rk20.gt.zero.and.rk20.le.rlmx2) then
          nkvec0 = nkvec0 + 1
          if (nkvec0.gt.maxkvec) then
            maxkvec = nkvec0 + 100
            call changemaxkvec
          endif
          xrk0(nkvec0) = xkd
          yrk0(nkvec0) = ykd
          zrk0(nkvec0) = 0.0_dp
        endif
!
!  End of ii,jj,kk loops over reciprocal lattice vectors
!
      enddo
    enddo
  endif
!
!  Allocate local memory now that maxkvec is known for sure
!
  allocate(kmod0(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','kmod0')
  allocate(ktrm0(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','ktrm0')
  allocate(ktrm3(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','ktrm3')
  allocate(ktrm3z(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','ktrm3z')
  allocate(ktrm4(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','ktrm4')
  allocate(ktrm4z(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','ktrm4z')
  allocate(tmp1(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','tmp1')
  allocate(tmp2(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','tmp2')
  allocate(tmp3(maxkvec),stat=status)
  if (status/=0) call outofmemory('recip2Dp','tmp3')
!
!  Generate reciprocal space term
!
  do i = 1,nkvec
    rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i)
    kmod(i) = sqrt(rk2)
    ktrm(i) = 0.5_dp*vol4pi/kmod(i)
  enddo
  if (.not.lgamma) then
    do i = 1,nkvec0
      rk2 = xrk0(i)*xrk0(i) + yrk0(i)*yrk0(i)
      kmod0(i) = sqrt(rk2)
      ktrm0(i) = 0.5_dp*vol4pi/kmod0(i)
    enddo
  endif
!********************************************
!  Calculate reciprocal space contribution  *
!********************************************
  do iv = 1,nkvec
    tmp1(iv) = xrk(iv)*xrk(iv)
    tmp2(iv) = yrk(iv)*yrk(iv)
    tmp3(iv) = xrk(iv)*yrk(iv)
  enddo
  do i = 1,numat
    oci = occuf(i)
    qli = qf(i)*oci*angstoev
    xci = xclat(i)
    yci = yclat(i)
    zci = zclat(i)
    ni = 3*(i-1)
    ix = ni + 1
    iy = ix + 1
    iz = iy + 1
    do j = 1,i
      ocj = occuf(j)
      qlj = qf(j)*ocj
!
!  Find relative vector between atoms
!
      xd = xclat(j) - xci
      yd = yclat(j) - yci
      zd = zclat(j) - zci
      qfct = qli*qlj
      etaz = seta*zd
      etaz2 = etaz * etaz
!
!  Zero second derivative contributions from this pair
!
      d21q = 0.0_dp
      d22q = 0.0_dp
      d23q = 0.0_dp
      d24q = 0.0_dp
      d25q = 0.0_dp
      d26q = 0.0_dp
      d21i = 0.0_dp
      d22i = 0.0_dp
      d23i = 0.0_dp
      d24i = 0.0_dp
      d25i = 0.0_dp
      d26i = 0.0_dp
!
!  First term - K vector = 0 term
!
!  Only added for gamma point calculation terms since it
!  is explcitly allowed for at a non-zero K point 
!
      derfez = derf(etaz)
      dexpz  = exp(-etaz2)
      twoqv  = qfct*vol4pi
      dtrm2 = - twoqv*tweatpi*dexpz
!
!  Find local kvector cut-off
!
      if (abs(etaz).gt.argtest) then
        Gmax = abs(accf2/zd) 
      else
        Gmax = sqrt(4.0*eta*(accf2-etaz2)) 
      endif
!
!  Second term - K vector dependent
!
      do iv = 1,nkvec
        kvec = kmod(iv)
        if (kvec.le.Gmax) then
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          arg = (xrkk-xkv)*xd + (yrkk-ykv)*yd
          cosa = cos(arg)*qfct
          sina = sin(arg)*qfct
          cosq = cosa*ktrm(iv)
          sineq = sina*ktrm(iv)
          dexp1 = exp(kvec*zd)
          dexp2 = 1.0_dp/dexp1
          darg1 = kvec*rhseta + etaz
          darg2 = kvec*rhseta - etaz
          dexp3 = exp(-(darg1)**2)
          dexp4 = exp(-(darg2)**2)
          derfc1 = derfc(darg1)
          derfc2 = derfc(darg2)
          kexperfc = dexp1*derfc1 + dexp2*derfc2
          ztrm1 = (kvec*(dexp1*derfc1-dexp2*derfc2) - tweatpi*(dexp1*dexp3-dexp2*dexp4))
          trmzz = (kvec*kvec*kexperfc - 2.0_dp*tweatpi*(kvec*(dexp1*dexp3+dexp2*dexp4)  &
                  - seta*(darg1*dexp1*dexp3+darg2*dexp2*dexp4)))
!
!  Phased second derivatives with respect to atoms
!
          d21q = d21q + cosq*tmp1(iv)*kexperfc
          d22q = d22q + cosq*tmp2(iv)*kexperfc
          d23q = d23q - cosq*trmzz
          d24q = d24q + yrkk*sineq*ztrm1
          d25q = d25q + xrkk*sineq*ztrm1
          d26q = d26q + cosq*tmp3(iv)*kexperfc
          d21i = d21i - sineq*tmp1(iv)*kexperfc
          d22i = d22i - sineq*tmp2(iv)*kexperfc
          d23i = d23i + sineq*trmzz
          d24i = d24i + yrkk*cosq*ztrm1
          d25i = d25i + xrkk*cosq*ztrm1
          d26i = d26i - sineq*tmp3(iv)*kexperfc
        endif
      enddo
      if (lgamma) d23q = d23q - dtrm2
      if (i.eq.j) then
!
!  Subtract gamma point terms
!
        if (.not.lgamma) then
          do iv = 1,nkvec0
            kvec = kmod0(iv)
            if (kvec.le.Gmax) then
              xrkk = xrk0(iv)
              yrkk = yrk0(iv)
              arg = xrkk*xd + yrkk*yd
              cosa = cos(arg)*qfct
              sina = sin(arg)*qfct
              cosq0 = cosa*ktrm0(iv)
              sinq0 = sina*ktrm0(iv)
              dexp1 = exp(kvec*zd)
              dexp2 = 1.0_dp/dexp1
              darg1 = kvec*rhseta + etaz
              darg2 = kvec*rhseta - etaz
              dexp3 = exp(-(darg1)**2)
              dexp4 = exp(-(darg2)**2)
              derfc1 = derfc(darg1)
              derfc2 = derfc(darg2)
              kexperfc = dexp1*derfc1 + dexp2*derfc2
              ztrm1 = (kvec*(dexp1*derfc1-dexp2*derfc2) - tweatpi*(dexp1*dexp3-dexp2*dexp4))
              trmzz = (kvec*kvec*kexperfc - 2.0_dp*tweatpi*(kvec*(dexp1*dexp3+dexp2*dexp4) &
                - seta*(darg1*dexp1*dexp3+darg2*dexp2*dexp4)))
              d21q = d21q - cosq0*xrkk*xrkk*kexperfc
              d22q = d22q - cosq0*yrkk*yrkk*kexperfc
              d23q = d23q + cosq0*trmzz
              d24q = d24q - yrkk*sinq0*ztrm1
              d25q = d25q - xrkk*sinq0*ztrm1
              d26q = d26q - cosq0*xrkk*yrkk*kexperfc
            endif
          enddo
          d23q = d23q + dtrm2
        else
          d21q = 0.0_dp
          d22q = 0.0_dp
          d23q = 0.0_dp
          d24q = 0.0_dp
          d25q = 0.0_dp
          d26q = 0.0_dp
        endif
      endif
      nj = 3*(j-1)
      derv2(nj+1,ix) = derv2(nj+1,ix) + d21q
      derv2(nj+2,ix) = derv2(nj+2,ix) + d26q
      derv2(nj+3,ix) = derv2(nj+3,ix) + d25q
      derv2(nj+1,iy) = derv2(nj+1,iy) + d26q
      derv2(nj+2,iy) = derv2(nj+2,iy) + d22q
      derv2(nj+3,iy) = derv2(nj+3,iy) + d24q
      derv2(nj+1,iz) = derv2(nj+1,iz) + d25q
      derv2(nj+2,iz) = derv2(nj+2,iz) + d24q
      derv2(nj+3,iz) = derv2(nj+3,iz) + d23q
      dervi(nj+1,ix) = dervi(nj+1,ix) + d21i
      dervi(nj+2,ix) = dervi(nj+2,ix) + d26i
      dervi(nj+3,ix) = dervi(nj+3,ix) + d25i
      dervi(nj+1,iy) = dervi(nj+1,iy) + d26i
      dervi(nj+2,iy) = dervi(nj+2,iy) + d22i
      dervi(nj+3,iy) = dervi(nj+3,iy) + d24i
      dervi(nj+1,iz) = dervi(nj+1,iz) + d25i
      dervi(nj+2,iz) = dervi(nj+2,iz) + d24i
      dervi(nj+3,iz) = dervi(nj+3,iz) + d23i
    enddo
  enddo
  if (lDoQDeriv2) then
!************************************************************************
!  Calculation of charge derivative contribution to second derivatives  *
!************************************************************************
!
!  To save space :
!  d1i  is stored in trm3
!  d1j  is stored in trm4
!  d2i2 is stored in csin
!  d2ij is stored in argc
!  d2j2 is stored in sine
!
    do iv = 1,nkvec
      csin(iv) = 0.0_dp
      sine(iv) = 0.0_dp
    enddo
    ix = - 2
    iy = - 1
    iz =   0
    do i = 1,numat
      oci = occuf(i)
      qli = qf(i)*oci
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,i
        ocj = occuf(j)
        if (i.eq.j) then
          fct = 0.5_dp
        else
          fct = 1.0_dp
        endif
        qlj = qf(j)*ocj
        jx = jx + 3
        jy = jy + 3
        jz = jz + 3
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        etaz = seta*zd
        etaz2 = etaz * etaz
!
!  First term - K vector independent
!
        derfez = derf(etaz)
        dexpz  = exp(-etaz2)
        fct = fct*angstoev
        etrm  = - vol4pi*(zd*derfez + dexpz*rpieta)*fct
        dtrm1 = - vol4pi*derfez*fct
! d2E/dqi.dqj
        d2trm1dij = etrm*oci*ocj
! d2E/dqi.dz                                                                                                   
        d2trm1diz = dtrm1*oci*qlj
! d2E/dqj.dz
        d2trm1djz = dtrm1*ocj*qli
! dE/dqi
        dtrm1di = etrm*oci*qlj
! dE/dqj
        dtrm1dj = etrm*qli*ocj
!
!  Find local kvector cut-off
!
        if (abs(etaz).gt.argtest) then
          Gmax = abs(accf2/zd)
        else
          Gmax = sqrt(4.0_dp*eta*(accf2-etaz2))
        endif
        if (Gmax.ge.smallestG) then
          do iv = 1,nkvec
            kvec = kmod(iv)
            if (kvec.le.Gmax) then
              xrkk = xrk(iv)
              yrkk = yrk(iv)
              arg = xrkk*xd + yrkk*yd
              cosa = cos(arg)*fct*ktrm(iv)
              sina = sin(arg)*fct*ktrm(iv)
              dexp1 = exp(kvec*zd)
              dexp2 = 1.0_dp/dexp1
              darg1 = kvec*rhseta + etaz
              darg2 = kvec*rhseta - etaz
              dexp3 = exp(-(darg1)**2)
              dexp4 = exp(-(darg2)**2)
              derfc1 = derfc(darg1)
              derfc2 = derfc(darg2)
              kexperfc = dexp1*derfc1 + dexp2*derfc2
              ztrm1 = (kvec*(dexp1*derfc1-dexp2*derfc2) - tweatpi*(dexp1*dexp3-dexp2*dexp4))
! d2E/dqi.dqj                                                                                                  
              argc(iv) = cosa*oci*ocj*kexperfc                                                               
! d2E/dqi.dr                                                                                                   
              ktrm3(iv) = - oci*qlj*sina*kexperfc                                                             
! d2E/dqi.dz                                                                                                   
              ktrm3z(iv) = oci*qlj*cosa*ztrm1                                                                
! d2E/dqj.dr                                                                                                   
              ktrm4(iv) = - ocj*qli*sina*kexperfc 
! d2E/dqj.dz
              ktrm4z(iv) = ocj*qli*cosa*ztrm1
! dE/dqi
              tmp1(iv) = cosa*oci*qlj*kexperfc
! dE/dqj
              ktrm0(iv) = cosa*qli*ocj*kexperfc
            else
              argc(iv) = 0.0_dp
              ktrm3(iv) = 0.0_dp
              ktrm3z(iv) = 0.0_dp
              ktrm4(iv) = 0.0_dp
              ktrm4z(iv) = 0.0_dp
              tmp1(iv) = 0.0_dp
              ktrm0(iv) = 0.0_dp
            endif
          enddo
        else
          do iv = 1,nkvec
            argc(iv) = 0.0_dp
            ktrm3(iv) = 0.0_dp
            ktrm3z(iv) = 0.0_dp
            ktrm4(iv) = 0.0_dp
            ktrm4z(iv) = 0.0_dp
            tmp1(iv) = 0.0_dp
            ktrm0(iv) = 0.0_dp
          enddo
        endif
!
!  Call d2charge
!
        d2self = 0.0_dp
        call d2chargep2D(i,j,nkvec,xrk,yrk,ix,iy,iz,jx,jy,jz,tmp1,ktrm0,ktrm3,ktrm3z, &
                         ktrm4,ktrm4z,csin,argc,sine,d2self,d2trm1dij,d2trm1diz, &
                         d2trm1djz,dtrm1di,dtrm1dj)
      enddo
    enddo
  endif
!
!  Free local memory
!
  deallocate(tmp3,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','tmp3')
  deallocate(tmp2,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','tmp2')
  deallocate(tmp1,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','tmp1')
  deallocate(ktrm4z,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','ktrm4z')
  deallocate(ktrm4,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','ktrm4')
  deallocate(ktrm3z,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','ktrm3z')
  deallocate(ktrm3,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','ktrm3')
  deallocate(ktrm0,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','ktrm0')
  deallocate(kmod0,stat=status)
  if (status/=0) call deallocate_error('recip2Dp','kmod0')
!
!  Timing
!
  time1 = cputime()
  tion = tion + time1 - time0
!
  return
  end
