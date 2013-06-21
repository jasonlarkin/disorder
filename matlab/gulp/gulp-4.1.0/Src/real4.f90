  subroutine real4(ni,esum4,d2,xmono,ymono,zmono)
!
!  Subroutine for calculating the real space contribution to 1/r**4
!  Useage of scratch is made compatible with real2b's useage of
!  region 2a info also stored in this area.
!
!   3/03 Nadd increased for small angles
!  11/07 Unused variables cleaned up
!   2/09 Expanding and contracting of maxdis arrays removed
!   2/09 Argument removed from changemaxdis call
!   5/12 third now used from numbers
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use constants
  use control
  use current
  use datatypes
  use general
  use kspace
  use numbers,        only : third
  use parallel
  use realvectors
  use region2a
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4)        :: ni
  real(dp)           :: d2(3,3)
  real(dp)           :: esum4
  real(dp)           :: xmono
  real(dp)           :: ymono
  real(dp)           :: zmono
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
  integer(i4)        :: nor
  logical            :: lnadd
  real(dp)           :: cubeta
  real(dp)           :: cut2
  real(dp)           :: etrm
  real(dp)           :: expo
  real(dp)           :: perpr
  real(dp)           :: projr
  real(dp)           :: quaeta
  real(dp)           :: quieta
  real(dp)           :: r2
  real(dp)           :: r4
  real(dp)           :: r6
  real(dp)           :: ra
  real(dp)           :: rb
  real(dp)           :: rc
  real(dp)           :: rk2
  real(dp)           :: rk4
  real(dp)           :: rmedium
  real(dp)           :: rp
  real(dp)           :: ru1x
  real(dp)           :: ru1y
  real(dp)           :: ru1z
  real(dp)           :: ru2x
  real(dp)           :: ru2y
  real(dp)           :: ru2z
  real(dp)           :: ru3x
  real(dp)           :: ru3y
  real(dp)           :: ru3z
  real(dp)           :: rx
  real(dp)           :: ry
  real(dp)           :: rz
  real(dp)           :: sexeta
  real(dp)           :: sqreta
  real(dp)           :: trm1
  real(dp)           :: trm2
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xcd
  real(dp)           :: ycd
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
!  Local variables
!
  sqreta = eta*eta
  cubeta = sqreta*eta
  quaeta = cubeta*eta
  quieta = quaeta*eta
  sexeta = quieta*eta
  rmedium = 1.0d-2
!
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
!
!  Create unit vectors
!
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
!  Set up cutoff
!
  cut2 = rmx2
  rp = sqrt(cut2)
  if (lnoreal) goto 999
!***********************************************
!  Region 1 perfect lattice  -  region 2 energy  *
!***********************************************
!
!  Outer loop over sites
!
  nor = 0
  xal = xclat(ni)
  yal = yclat(ni)
  zal = zclat(ni)
  xcrd = xal - xmono
  ycrd = yal - ymono
  zcrd = zal - zmono
!
!  New looping section
!
  projr = xcrd*ru1x + ycrd*ru1y + zcrd*ru1z
  max1u = (rp - projr)*ra + nadd
  max1l = (rp + projr)*ra + nadd
  max1l1 = max1l + 1
  xcdi = xcrd - max1l1*r1x
  ycdi = ycrd - max1l1*r1y
  zcdi = zcrd - max1l1*r1z
!
!  Loop over unit cells to find interatomic distances
!
  do ii =  - max1l,max1u
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
    do jj =  - max2l,max2u
      xcdj = xcdj + r2x
      ycdj = ycdj + r2y
      zcdj = zcdj + r2z
!
      projr = xcdj*ru3x + ycdj*ru3y + zcdj*ru3z
      perpr = xcdj*xcdj + ycdj*ycdj + zcdj*zcdj
      perpr = perpr - projr*projr
      perpr = cut2 - perpr
      perpr = sqrt(abs(perpr))
      max3u = (perpr - projr)*rc + 1
      max3l = (perpr + projr)*rc + 1
      max3l1 = max3l + 1
!
      xcd = xcdj - max3l1*r3x
      ycd = ycdj - max3l1*r3y
      zcd = zcdj - max3l1*r3z
!$dir scalar
      do kk =  - max3l,max3u
        xcd = xcd + r3x
        ycd = ycd + r3y
        zcd = zcd + r3z
        r2 = xcd*xcd + ycd*ycd + zcd*zcd
        if (nor.ge.maxdis) then
          maxdis  =  nor  +  100
          call changemaxdis
        endif
        if (r2.lt.rmedium) then
!
!  Use power series expansion with subtraction of explicit 1/r**4 term
!  for short distances.
!
          esum4 = esum4 + sqreta*( - 0.5_dp + r2*(third*eta - 0.125_dp*sqreta*r2))
!
!  Use power series expansion with subtraction of explicit ra*rb/r**6
!  term for short distances.
!
          r4 = r2*r2
          r6 = r2*r4
          trm1 =  - third*cubeta + 0.25_dp*quaeta*r2 - 0.1_dp*quieta*r4
          trm1 = trm1 + sexeta*r6/36.0_dp
          trm1 = 4.0_dp*trm1
          trm2 =  - 0.5_dp*sqreta + third*cubeta*r2 - 0.125_dp*quaeta*r4
          trm2 = trm2 + quieta*r6
          trm2 = 2.0_dp*trm2
          d2(1,1) = d2(1,1) + xcd*xcd*trm1 - trm2
          d2(2,2) = d2(2,2) + ycd*ycd*trm1 - trm2
          d2(3,3) = d2(3,3) + zcd*zcd*trm1 - trm2
          d2(2,1) = d2(2,1) + ycd*xcd*trm1
          d2(3,1) = d2(3,1) + zcd*xcd*trm1
          d2(3,2) = d2(3,2) + zcd*ycd*trm1
        elseif (r2.le.cut2) then
!
!  Store vector
!
          nor = nor + 1
          deriv(nor) = r2
          xtmp(nor) = xcd
          ytmp(nor) = ycd
          ztmp(nor) = zcd
        endif
      enddo
    enddo
  enddo
!
!  Calculate real space contribution to 1/r**4
!
  if (nor.gt.0) then
    do i = 1,nor
      r2 = deriv(i)
      rk2 = 1.0_dp/r2
      rk4 = rk2*rk2
      rx = xtmp(i)
      ry = ytmp(i)
      rz = ztmp(i)
!
!  1/r**4 term
!
      etrm = eta*r2
      expo = exp( - etrm)
      esum4 = esum4 + rk4*(1.0_dp + etrm)*expo
!
!  ra*rb/r**6 term
!
      trm1 = 2.0_dp*rk2*(eta + rk2)
      trm2 = trm1*expo
      d2(1,1) = d2(1,1) - trm2
      d2(2,2) = d2(2,2) - trm2
      d2(3,3) = d2(3,3) - trm2
      trm1 = 4.0_dp*rk2*(trm1 + sqreta)*expo
      d2(1,1) = d2(1,1) + rx*rx*trm1
      d2(2,1) = d2(2,1) + rx*ry*trm1
      d2(3,1) = d2(3,1) + rx*rz*trm1
      d2(2,2) = d2(2,2) + ry*ry*trm1
      d2(3,2) = d2(3,2) + ry*rz*trm1
      d2(3,3) = d2(3,3) + rz*rz*trm1
    enddo
  endif
!
!  Units will converted to eV/Angs**4 and d2 symmetrised on return
!
  if (.not.lnadd) nadd = 0
!***************
!  Exit point  *
!***************
999 continue
!
  return
  end
