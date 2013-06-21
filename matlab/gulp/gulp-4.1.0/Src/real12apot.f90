  subroutine real12apot(v,xdrv,ydrv,zdrv,lsymalg)
!
!  Subroutine for calculating real space electrostatic site 
!  potentials for region 1.
!  Symmetry adapted form
!
!  xdefe = region 1 coordinates
!  xclat = region 2 coordinates per unit cell
!
!   3/03 Nadd increased for small angles
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!  11/12 Modified Wolf sum of Fennell and Gezelter added
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
  use constants
  use control
  use current
  use defects
  use general
  use kspace
  use shell
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
  integer(i4)        :: ii
  integer(i4)        :: j
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
  integer(i4)        :: nloop
  logical            :: l111
  logical            :: lnadd
  real(dp)           :: cmax
  real(dp)           :: cputime
  real(dp)           :: cut2
  real(dp)           :: cut2s
  real(dp)           :: derfc
  real(dp)           :: erffc
  real(dp)           :: etaloc
  real(dp)           :: factor
  real(dp)           :: ocj
  real(dp)           :: perpr
  real(dp)           :: projr
  real(dp)           :: qlj
  real(dp)           :: r
  real(dp)           :: r2
  real(dp)           :: ra
  real(dp)           :: rb
  real(dp)           :: rc
  real(dp)           :: rexp
  real(dp)           :: rk
  real(dp)           :: rp
  real(dp)           :: rpres
  real(dp)           :: rpres2
  real(dp)           :: rpres3
  real(dp)           :: ru1x
  real(dp)           :: ru1y
  real(dp)           :: ru1z
  real(dp)           :: ru2x
  real(dp)           :: ru2y
  real(dp)           :: ru2z
  real(dp)           :: ru3x
  real(dp)           :: ru3y
  real(dp)           :: ru3z
  real(dp)           :: setaloc
  real(dp)           :: setrm
  real(dp)           :: small
  real(dp)           :: time1
  real(dp)           :: time2
  real(dp)           :: trm1
  real(dp)           :: trmi
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xcd
  real(dp)           :: xcd2
  real(dp)           :: ycd
  real(dp)           :: ycd2
  real(dp)           :: zcd
  real(dp)           :: xcdi
  real(dp)           :: ycdi
  real(dp)           :: zcdi
  real(dp)           :: xcdj
  real(dp)           :: ycdj
  real(dp)           :: zcdj
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
  small = 1.0d-12
  time1 = cputime()
  lnadd = .true.
  if (nadd.eq.0) then
    lnadd = .false.
    if (lra) then
      nadd = 1
    else
      if (alpha.lt.30.0_dp.or.beta.lt.30.0_dp.or.gamma.lt.30.0_dp) then
        nadd = 5
      elseif (alpha.gt.150.0_dp.or.beta.gt.150.0_dp.or.gamma.gt.150.0_dp) then
        nadd = 5
      elseif (alpha.lt.50.0_dp.or.beta.lt.50.0_dp.or.gamma.lt.50.0_dp) then
        nadd = 4
      elseif (alpha.gt.130.0_dp.or.beta.gt.130.0_dp.or.gamma.gt.130.0_dp) then
        nadd = 4
      elseif (alpha.lt.70.0_dp.or.beta.lt.70.0_dp.or.gamma.lt.70.0_dp) then
        nadd = 3
      elseif (alpha.gt.110.0_dp.or.beta.gt.110.0_dp.or.gamma.gt.110.0_dp) then
        nadd = 3
      else
        nadd = 2
      endif
    endif
  endif
!
!  Set up local variables
!
  ra = 1.0_dp/a
  rb = 1.0_dp/b
  rc = 1.0_dp/c
  ru1x = r1x*ra
  ru1y = r1y*ra
  ru1z = r1z*ra
  ru2x = r2x*rb
  ru2y = r2y*rb
  ru2z = r2z*rb
  ru3x = r3x*rc
  ru3y = r3y*rc
  ru3z = r3z*rc
!
!  Set up cutoffs
!
  cut2s = cuts*cuts
  if (lwolf) then
    etaloc = etaw*etaw
    setaloc = etaw
    cmax = cutw
    cut2 = cutw*cutw
  else
    etaloc = eta
    setaloc = seta
    cmax = sqrt(rmx2)
    cut2 = rmx2
  endif
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  l111 = .true.
  if (a.lt.cmax.or.b.lt.cmax.or.c.lt.cmax) l111 = .false.
  if (alpha.lt.80.0_dp.or.beta.lt.80.0_dp.or.gamma.lt.80.0_dp) l111 = .false.
  if (alpha.gt.100.0_dp.or.beta.gt.100.0_dp.or.gamma.gt.100.0_dp) l111 = .false.
!
  if (lnoreal) goto 999
!*******************************
!  Region 1 - region 2 energy  *
!*******************************
!
!  Outer loop over sites
!
  if (lsymalg) then
    nloop = ndasym
  else
    nloop = nreg1
  endif
  do i = 1,nloop
    if (lsymalg) then
      ii = ndsptr(i)
    else
      ii = i
    endif
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
!
!  Start of second atom loop
!
    do j = 1,numat
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      qlj = qf(j)
      ocj = occuf(j)
      factor = qlj*ocj*angstoev
      rp = sqrt(cut2)
!*******************************************************
!  Loop over unit cells to find interatomic distances  *
!*******************************************************
      if (lra) then
!
!  Right angled cell
!
        if (l111) then
          xcd = xcrd - 2.0_dp*r1x
          do ii = - 1,1
            xcd = xcd + r1x
            xcd2 = xcd*xcd
            ycd = ycrd - 2.0_dp*r2y
            do jj = - 1,1
              ycd = ycd + r2y
              ycd2 = ycd*ycd
              zcd = zcrd - 2.0_dp*r3z
              do kk = - 1,1
                zcd = zcd + r3z
                r2 = xcd2 + ycd2 + zcd*zcd
                if (r2.lt.small) then
!
!  Remove electrostatic self energy
!
                  setrm = factor*tweatpi
                  if (lwolf) then
                    v(i) = v(i) - setrm - factor*(selfwolf + selfwolfenergy*cutw)
                  else
                    v(i) = v(i) - setrm
                  endif
                elseif (r2.le.cut2) then
                  r = sqrt(r2)
                  rk = 1.0_dp/r
                  erffc = derfc(setaloc*r)
                  trmi = factor*erffc*rk
                  v(i) = v(i) + trmi
                  rexp = tweatpi*exp(-etaloc*r2)
                  trm1 = erffc*rk+rexp
                  if (r2.lt.cut2s) then
                    trm1 = trm1 - rk
                    v(i) = v(i) - factor*rk
                  endif
                  trm1 = factor*trm1*rk*rk
                  xdrv(i) = xdrv(i) + trm1*xcd
                  ydrv(i) = ydrv(i) + trm1*ycd
                  zdrv(i) = zdrv(i) + trm1*zcd
                  if (lwolf) then
                    v(i) = v(i) - factor*(selfwolf - selfwolfenergy*(r - cutw))
                    trm1 = factor*selfwolfforce*rk
                    xdrv(i) = xdrv(i) - trm1*xcd
                    ydrv(i) = ydrv(i) - trm1*ycd
                    zdrv(i) = zdrv(i) - trm1*zcd
                  endif
                endif
              enddo
            enddo
          enddo
        else
          max1u = (rp - xcrd)*ra + nadd
          max1l = (rp + xcrd)*ra + nadd
          max1l1 = max1l + 1
          xcd = xcrd - max1l1*r1x
          do ii = - max1l,max1u
            xcd = xcd + r1x
            if (abs(xcd).lt.rp) then
              xcd2 = xcd*xcd
              rpres2 = rp*rp - xcd2
              rpres = sqrt(rpres2)
              max2u = (rpres - ycrd)*rb + nadd
              max2l = (rpres + ycrd)*rb + nadd
              max2l1 = max2l + 1
              ycd = ycrd - max2l1*r2y
              do jj = - max2l,max2u
                ycd = ycd + r2y
                ycd2 = ycd*ycd
                rpres3 = rpres2 - ycd2
                if (rpres3.gt.0.0_dp) then
                  rpres3 = sqrt(rpres3)
                  max3u = (rpres3-zcrd)*rc + 1
                  max3l = (rpres3+zcrd)*rc + 1
                  max3l1 = max3l + 1
                  zcd = zcrd - max3l1*r3z
                  do kk = - max3l,max3u
                    zcd = zcd + r3z
                    r2 = xcd2 + ycd2 + zcd*zcd
                    if (r2.lt.small) then
!
!  Remove electrostatic self energy
!
                      setrm = factor*tweatpi
                      if (lwolf) then
                        v(i) = v(i) - setrm - factor*(selfwolf + selfwolfenergy*cutw)
                      else
                        v(i) = v(i) - setrm
                      endif
                    elseif (r2.le.cut2) then
                      r = sqrt(r2)
                      rk = 1.0_dp/r
                      erffc = derfc(setaloc*r)
                      trmi = factor*erffc*rk
                      v(i) = v(i) + trmi
                      rexp = tweatpi*exp(-etaloc*r2)
                      trm1 = erffc*rk + rexp
                      if (r2.lt.cut2s) then
                        trm1 = trm1 - rk
                        v(i) = v(i) - factor*rk
                      endif
                      trm1 = factor*trm1*rk*rk
                      xdrv(i) = xdrv(i) + trm1*xcd
                      ydrv(i) = ydrv(i) + trm1*ycd
                      zdrv(i) = zdrv(i) + trm1*zcd
                      if (lwolf) then
                        v(i) = v(i) - factor*(selfwolf - selfwolfenergy*(r - cutw))
                        trm1 = factor*selfwolfforce*rk
                        xdrv(i) = xdrv(i) - trm1*xcd
                        ydrv(i) = ydrv(i) - trm1*ycd
                        zdrv(i) = zdrv(i) - trm1*zcd
                      endif
                    endif
                  enddo
                endif
              enddo
            endif
          enddo
        endif
      else
!
!  General cell
!
        if (l111) then
          xcdi = xcrd - 2.0_dp*r1x
          ycdi = ycrd - 2.0_dp*r1y
          zcdi = zcrd - 2.0_dp*r1z
          do ii = -1,1
            xcdi = xcdi + r1x
            ycdi = ycdi + r1y
            zcdi = zcdi + r1z
            xcdj = xcdi - 2.0_dp*r2x
            ycdj = ycdi - 2.0_dp*r2y
            zcdj = zcdi - 2.0_dp*r2z
            do jj = -1,1
              xcdj = xcdj + r2x
              ycdj = ycdj + r2y
              zcdj = zcdj + r2z
              xcd = xcdj - 2.0_dp*r3x
              ycd = ycdj - 2.0_dp*r3y
              zcd = zcdj - 2.0_dp*r3z
              do kk = -1,1
                xcd = xcd + r3x
                ycd = ycd + r3y
                zcd = zcd + r3z
                r2 = xcd*xcd + ycd*ycd + zcd*zcd
                if (r2.lt.small) then
!
!  Remove electrostatic self energy
!
                  setrm = factor*tweatpi
                  if (lwolf) then
                    v(i) = v(i) - setrm - factor*(selfwolf + selfwolfenergy*cutw)
                  else
                    v(i) = v(i) - setrm
                  endif
                elseif (r2.le.cut2) then
                  r = sqrt(r2)
                  rk = 1.0_dp/r
                  erffc = derfc(setaloc*r)
                  trmi = factor*erffc*rk
                  v(i) = v(i) + trmi
                  rexp = tweatpi*exp(-etaloc*r2)
                  trm1 = erffc*rk + rexp
                  if (r2.lt.cut2s) then
                    trm1 = trm1 - rk
                    v(i) = v(i) - factor*rk
                  endif
                  trm1 = factor*trm1*rk*rk
                  xdrv(i) = xdrv(i) + trm1*xcd
                  ydrv(i) = ydrv(i) + trm1*ycd
                  zdrv(i) = zdrv(i) + trm1*zcd
                  if (lwolf) then
                    v(i) = v(i) - factor*(selfwolf - selfwolfenergy*(r - cutw))
                    trm1 = factor*selfwolfforce*rk
                    xdrv(i) = xdrv(i) - trm1*xcd
                    ydrv(i) = ydrv(i) - trm1*ycd
                    zdrv(i) = zdrv(i) - trm1*zcd
                  endif
                endif
              enddo
            enddo
          enddo
        else
          projr = xcrd*ru1x + ycrd*ru1y + zcrd*ru1z
          max1u = (rp - projr)*ra + nadd
          max1l = (rp + projr)*ra + nadd
          max1l1 = max1l + 1
          xcdi = xcrd - max1l1*r1x
          ycdi = ycrd - max1l1*r1y
          zcdi = zcrd - max1l1*r1z
          do ii = -max1l,max1u
            xcdi = xcdi + r1x
            ycdi = ycdi + r1y
            zcdi = zcdi + r1z
!
            projr = xcdi*ru2x + ycdi*ru2y + zcdi*ru2z
            max2u = (rp - projr)*rb + nadd
            max2l = (rp + projr)*rb + nadd
            max2l1 = max2l + 1
!
            xcdj = xcdi - max2l1*r2x
            ycdj = ycdi - max2l1*r2y
            zcdj = zcdi - max2l1*r2z
            do jj = -max2l,max2u
              xcdj = xcdj + r2x
              ycdj = ycdj + r2y
              zcdj = zcdj + r2z
!
              projr = xcdj*ru3x + ycdj*ru3y + zcdj*ru3z
              perpr = xcdj*xcdj + ycdj*ycdj + zcdj*zcdj
              perpr = perpr - projr*projr
              perpr = cut2 - perpr
              perpr = sqrt(abs(perpr))
              max3u = (perpr-projr)*rc + nadd
              max3l = (perpr + projr)*rc + nadd
              max3l1 = max3l + 1
!
              xcd = xcdj - max3l1*r3x
              ycd = ycdj - max3l1*r3y
              zcd = zcdj - max3l1*r3z
              do kk = -max3l,max3u
                xcd = xcd + r3x
                ycd = ycd + r3y
                zcd = zcd + r3z
                r2 = xcd*xcd + ycd*ycd + zcd*zcd
                if (r2.lt.small) then
!
!  Remove electrostatic self energy
!
                  setrm = factor*tweatpi
                  if (lwolf) then
                    v(i) = v(i) - setrm - factor*(selfwolf + selfwolfenergy*cutw)
                  else
                    v(i) = v(i) - setrm
                  endif
                elseif (r2.le.cut2) then
                  r = sqrt(r2)
                  rk = 1.0_dp/r
                  erffc = derfc(setaloc*r)
                  trmi = factor*erffc*rk
                  v(i) = v(i) + trmi
                  rexp = tweatpi*exp(-etaloc*r2)
                  trm1 = erffc*rk + rexp
                  if (r2.lt.cut2s) then
                    trm1 = trm1 - rk
                    v(i) = v(i) - factor*rk
                  endif
                  trm1 = factor*trm1*rk*rk
                  xdrv(i) = xdrv(i) + trm1*xcd
                  ydrv(i) = ydrv(i) + trm1*ycd
                  zdrv(i) = zdrv(i) + trm1*zcd
                  if (lwolf) then
                    v(i) = v(i) - factor*(selfwolf - selfwolfenergy*(r - cutw))
                    trm1 = factor*selfwolfforce*rk
                    xdrv(i) = xdrv(i) - trm1*xcd
                    ydrv(i) = ydrv(i) - trm1*ycd
                    zdrv(i) = zdrv(i) - trm1*zcd
                  endif
                endif
              enddo
            enddo
          enddo
        endif
      endif
    enddo
  enddo
!
!  End of real space part
!
999 continue
  if (.not.lnadd) nadd = 0
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
