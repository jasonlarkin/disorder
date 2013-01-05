  subroutine recip12apot(v,xdrv,ydrv,zdrv,lsymalg)
!
!  Calculate reciprocal space contribution to region 1 - region 2a
!  potential. 
!
!  lsymalg = if .true. then use symmetry adapted algorithm
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
  use constants
  use control
  use current
  use defects
  use kspace
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical            :: lsymalg
  real(dp)           :: v(*)
  real(dp)           :: xdrv(*)
  real(dp)           :: ydrv(*)
  real(dp)           :: zdrv(*)
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: idk
  integer(i4)        :: ii
  integer(i4)        :: iv
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: kk
  integer(i4)        :: nloop
  real(dp)           :: arg
  real(dp)           :: arge
  real(dp)           :: cputime
  real(dp)           :: cosa
  real(dp)           :: cosq
  real(dp)           :: csinq
  real(dp)           :: factor
  real(dp)           :: kvv(3)
  real(dp)           :: ocj
  real(dp)           :: qlj
  real(dp)           :: rk2
  real(dp)           :: rrk2
  real(dp)           :: sina
  real(dp)           :: sineq
  real(dp)           :: sinqx
  real(dp)           :: sinqy
  real(dp)           :: sinqz
  real(dp)           :: timel0
  real(dp)           :: time1
  real(dp)           :: xpon
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xd
  real(dp)           :: yd
  real(dp)           :: zd
  real(dp)           :: xrkk
  real(dp)           :: yrkk
  real(dp)           :: zrkk
!
  timel0 = cputime()
!
  eta4 = 0.25_dp/eta
!**********************************
!  Calculate and store k - vectors  *
!**********************************
  if (lra) then
    kvv(1) = kv(1,1)
    kvv(2) = kv(2,2)
    kvv(3) = kv(3,3)
    do i = 1,nkvec
      idk = indk(i)
      ii = (idk/6400) - 40
      if (ii.eq.0) then
        factor = 1.0_dp
      else
        factor = 2.0_dp
      endif
      idk = idk - (ii + 40)*6400
      jj = (idk/80) - 40
      kk = idk - (jj + 40)*80 - 40
      xrk(i) = ii*kvv(1)
      yrk(i) = jj*kvv(2)
      zrk(i) = kk*kvv(3)
      rk2 = xrk(i)*xrk(i)
      rk2 = rk2 + yrk(i)*yrk(i)
      rk2 = rk2 + zrk(i)*zrk(i)
      arge =  - rk2*eta4
      xpon = exp(arge)
      rrk2 = 1.0_dp/rk2
      ktrm(i) = xpon*vol4pi*factor*rrk2
    enddo
  else
    do i = 1,nkvec
      idk = indk(i)
      ii = (idk/6400) - 40
      idk = idk - (ii + 40)*6400
      jj = (idk/80) - 40
      kk = idk - (jj + 40)*80 - 40
      factor = 2.0_dp
      if (ii.eq.0.and.nkangle.eq.1) then
        factor = 1.0_dp
      elseif (jj.eq.0.and.nkangle.eq.2) then
        factor = 1.0_dp
      elseif (kk.eq.0.and.nkangle.eq.3) then
        factor = 1.0_dp
      elseif (nkangle.eq.0) then
        factor = 1.0_dp
      endif
      xrk(i) = ii*kv(1,1) + jj*kv(1,2)+kk*kv(1,3)
      yrk(i) = ii*kv(2,1) + jj*kv(2,2)+kk*kv(2,3)
      zrk(i) = ii*kv(3,1) + jj*kv(3,2)+kk*kv(3,3)
      rk2 = xrk(i)*xrk(i)
      rk2 = rk2 + yrk(i)*yrk(i)
      rk2 = rk2 + zrk(i)*zrk(i)
      arge =  - rk2*eta4
      xpon = exp(arge)
      rrk2 = 1.0_dp/rk2
      ktrm(i) = xpon*vol4pi*factor*rrk2
    enddo
  endif
!
!  End of set - up section
!
  if (lnorecip) return
  if (lsymalg) then
!***********************
!  Symmetry algorithm  *
!***********************
    do i = 1,ndasym
      ii = ndsptr(i)
      xal = xdefe(ii)
      yal = ydefe(ii)
      zal = zdefe(ii)
      do j = 1,numat
        ocj = occuf(j)
        qlj = qf(j)*ocj
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xal
        yd = yclat(j) - yal
        zd = zclat(j) - zal
        csinq = 0.0_dp
        sinqx = 0.0_dp
        sinqy = 0.0_dp
        sinqz = 0.0_dp
        do iv = 1,nkvec
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          zrkk = zrk(iv)
          arg = xrkk*xd + yrkk*yd+zrkk*zd
          cosa = cos(arg)
          sina = sin(arg)
          cosq = cosa*ktrm(iv)
          csinq = csinq + cosq
          sineq = sina*ktrm(iv)
          sinqx = sinqx + sineq*xrkk
          sinqy = sinqy + sineq*yrkk
          sinqz = sinqz + sineq*zrkk
        enddo
!
!  Potential
!
        v(i) = v(i) + csinq*qlj
!
!  Derivatives
!
        xdrv(i) = xdrv(i) + sinqx*qlj
        ydrv(i) = ydrv(i) + sinqy*qlj
        zdrv(i) = zdrv(i) + sinqz*qlj
      enddo
    enddo
  else
!**************************
!  No symmetry algorithm  *
!**************************
    do i = 1,nreg1
      xal = xdefe(i)
      yal = ydefe(i)
      zal = zdefe(i)
      do j = 1,numat
        ocj = occuf(j)
        qlj = qf(j)*ocj
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xal
        yd = yclat(j) - yal
        zd = zclat(j) - zal
        csinq = 0.0_dp
        sinqx = 0.0_dp
        sinqy = 0.0_dp
        sinqz = 0.0_dp
        do iv = 1,nkvec
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          zrkk = zrk(iv)
          arg = xrkk*xd + yrkk*yd+zrkk*zd
          cosa = cos(arg)
          sina = sin(arg)
          cosq = cosa*ktrm(iv)
          csinq = csinq + cosq
          sineq = sina*ktrm(iv)
          sinqx = sinqx + sineq*xrkk
          sinqy = sinqy + sineq*yrkk
          sinqz = sinqz + sineq*zrkk
        enddo
!
!  Potential
!
        v(i) = v(i) + csinq*qlj
!
!  Derivatives
!
        xdrv(i) = xdrv(i) + sinqx*qlj
        ydrv(i) = ydrv(i) + sinqy*qlj
        zdrv(i) = zdrv(i) + sinqz*qlj
      enddo
    enddo
  endif
!
!  Convert units to eV
!
  if (lsymalg) then
    nloop = ndasym
  else
    nloop = nreg1
  endif
  do i = 1,nloop
    v(i) = angstoev*v(i)
    xdrv(i) = angstoev*xdrv(i)
    ydrv(i) = angstoev*ydrv(i)
    zdrv(i) = angstoev*zdrv(i)
  enddo
!
  time1 = cputime()
  treg1 = treg1 + time1 - timel0
!
  return
  end
