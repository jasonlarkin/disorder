  subroutine set2a
!
!  Generate region 2a
!
!  xclat = region 2 coordinates per unit cell
!
!  For three body calcs it is necessary to have all ions that interact
!  in region 2. If necessary increase region 2 radius to allow for 
!  this. Same applies to four body terms.
!
!  10/96 Buffer region in region 2a added for finding bonds linking 
!        region 1 atoms
!   4/97 Extension of region 2a for many-body potentials added
!   6/97 Region 2a now expanded to include all ions with an
!        interaction with region 1 - this should simplify the
!        defect calculation and speed it up at the expense of
!        memory.
!   8/02 Modifications for Brenner potential added
!   3/03 Setting of nadd made safer
!  10/05 Inversion potential added
!   6/06 Inversion squared potential added
!  10/07 Angle-angle cross potential added
!  11/07 Dummy initialisation added of ind for non-molecule case
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   4/08 Goto statements removed
!  11/08 xcosangleangle potential added
!   6/09 Module name changed from three to m_three
!   3/13 Shell location algorithm changed to be based on use of ncsptr
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use brennerdata,    only : bR2, bTR2
  use configurations, only : lbsmat
  use control
  use current
  use datatypes
  use defects
  use element
  use general
  use four
  use general
  use kspace
  use m_three
  use molecule
  use parallel
  use region2a
  use shell
  use sutton
  use symmetry
  use two
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: ind
  integer(i4)        :: indm
  integer(i4)        :: ix
  integer(i4)        :: iy
  integer(i4)        :: iz
  integer(i4)        :: ixi
  integer(i4)        :: iyi
  integer(i4)        :: izi
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
  integer(i4)        :: maxe1l
  integer(i4)        :: maxe1u
  integer(i4)        :: maxe2l
  integer(i4)        :: maxe2u
  integer(i4)        :: maxe3l
  integer(i4)        :: maxe3u
  integer(i4)        :: ncores
  integer(i4)        :: ni
  integer(i4)        :: nlreg2
  logical            :: lnadd
  logical            :: lnewdefalg
  real(dp)           :: cmax
  real(dp)           :: cut2q
  real(dp)           :: emax
  real(dp)           :: frsum
  real(dp)           :: projr
  real(dp)           :: r
  real(dp)           :: r1
  real(dp)           :: r12
  real(dp)           :: r2
  real(dp)           :: r22
  real(dp)           :: r3
  real(dp)           :: r32
  real(dp)           :: ra
  real(dp)           :: rb
  real(dp)           :: rblink
  real(dp)           :: rbrenmax
  real(dp)           :: rc
  real(dp)           :: rformax
  real(dp)           :: ri
  real(dp)           :: rmax
  real(dp)           :: rmax2
  real(dp)           :: rp
  real(dp)           :: rp2
  real(dp)           :: rscmax
  real(dp)           :: rthbmax
  real(dp)           :: ru1x
  real(dp)           :: ru1y
  real(dp)           :: ru1z
  real(dp)           :: ru2x
  real(dp)           :: ru2y
  real(dp)           :: ru2z
  real(dp)           :: trsum
  real(dp)           :: xas
  real(dp)           :: yas
  real(dp)           :: zas
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
!  Set new defect algorithm flag - must use this algorithm if EAM
!  model is being used.
!
  lnewdefalg = (index(keyword,'newda').ne.0.or.lsuttonc.or.ldcellr)
!
  r1 = reg1(ncf)
  r2 = reg2(ncf)
  r3 = 0.0_dp
  rp = 0.0_dp
  nreg2 = 0
  npreg2 = 0
  ntreg2 = 0
  nlreg2 = 0
!
!  Three body test
!
  if (nthb.gt.0) then
    rthbmax = 0.0_dp
    do i = 1,nthb
      trsum = thr1(i) + thr2(i)
      if (trsum.gt.rthbmax) rthbmax = trsum
    enddo
    r3 = r1 + rthbmax
  endif
!
!  Four body test
!
  if (nfor.gt.0) then
    rformax = 0.0_dp
    do i = 1,nfor
      if (loutofplane(i)) then
        frsum = max(for1(i),for2(i),for3(i))
      else
        if (for4(i).gt.0.0_dp) then
          frsum = for4(i)
        else
          frsum = for1(i) + for2(i) + for3(i)
        endif
      endif
      if (frsum.gt.rformax) rformax = frsum
    enddo
    if ((r1 + rformax).gt.r3) r3 = r1 + rformax
  endif
!             
!  Brenner test
!             
  if (lbrenner) then
    rbrenmax = 0.0_dp
    do i = 1,3
      if (bR2(i).gt.rbrenmax) rbrenmax = bR2(i)
      if (bTR2(i).gt.rbrenmax) rbrenmax = bTR2(i)
    enddo  
    rbrenmax = 3.0_dp*rbrenmax + r1
    if (rbrenmax.gt.r3) r3 = rbrenmax
  endif
!
!  Two-body / electrostatic tests
!
  if (lnewdefalg) then
    if (lwolf) then                   
      cut2q  =  cutw*cutw
    else
      cut2q  =  rmx2
    endif
    emax = sqrt(cut2q)
    cmax = max(rpmax,emax)
    if ((r1 + emax).gt.rp) rp = r1 + emax
    if ((r1 + cmax).gt.rp) rp = r1 + cmax
  else
    rp = r1
  endif
!
!  Many body test
!
  if (lsuttonc) then
    rscmax = 0.0_dp
    do i = 1,npote
      if (nptype(i).eq.19.or.nptype(i).eq.20) then
        if (rpot(i).gt.rscmax) rscmax = rpot(i)
      endif
    enddo
    if ((r1 + rscmax).gt.rp) rp = r1 + rscmax
  endif
  if (lmolmec) then
!
!  Region 2a bonding link list
!
    rblink = 0.0_dp
    do i = 1,numat
      ni = nat(i)
      ri = rcov(ni)
      if (ri.gt.rblink) rblink = ri
    enddo
    rblink = 2.0_dp*rblink
    if ((r1 + rblink).gt.r3) r3 = r1 + rblink
  endif
  if (r2.gt.r3) r3 = r2
  if (r2.gt.rp) rp = r2
  rmax = max(rp,r3)
  if (r1.eq.rmax) return
  r12 = r1*r1
  r22 = r2*r2
  r32 = r3*r3
  rp2 = rp*rp
  rmax2 = max(rp2,r32)
!
!  Set up local variables
!
  ra = 1.0_dp/a
  rb = 1.0_dp/b
  rc = 1.0_dp/c
!
  lnadd  =  .true.
  if (nadd.eq.0) then
    lnadd  =  .false.
    if (lra) then
      nadd  =  1
    else
      if (alpha.lt.30.0_dp.or.beta.lt.30.0_dp.or.gamma.lt.30.0_dp) then
        nadd  =  5
      elseif (alpha.gt.150.0_dp.or.beta.gt.150.0_dp.or.gamma.gt.150.0_dp) then
        nadd  =  5
      elseif (alpha.lt.50.0_dp.or.beta.lt.50.0_dp.or.gamma.lt.50.0_dp) then
        nadd  =  4
      elseif (alpha.gt.130.0_dp.or.beta.gt.130.0_dp.or.gamma.gt.130.0_dp) then
        nadd  =  4
      elseif (alpha.lt.70.0_dp.or.beta.lt.70.0_dp.or.gamma.lt.70.0_dp) then
        nadd  =  3
      elseif (alpha.gt.110.0_dp.or.beta.gt.110.0_dp.or.gamma.gt.110.0_dp) then
        nadd  =  3
      else
        nadd  =  2
      endif
    endif
  endif
!
!  Create unit vectors
!
  ru1x = r1x*ra
  ru1y = r1y*ra
  ru1z = r1z*ra
  ru2x = r2x*rb
  ru2y = r2y*rb
  ru2z = r2z*rb
!****************************************************
!  If ldcellr work out exclude cells from region 1  *
!****************************************************
  if (ldcellr) then
    maxe1u = r1*ra + 1
    maxe1l = maxe1u
    maxe2u = r1*rb + 1
    maxe2l = maxe2u
    maxe3u = r1*rc + 1
    maxe3l = maxe3u
  endif
!******************************
!  Find valid region 2 sites  *
!******************************
!
!  Locate cores first
!
  do i = 1,numat
    if (nat(i).le.maxele) then
      xcrd = xclat(i) - xdc
      ycrd = yclat(i) - ydc
      zcrd = zclat(i) - zdc
!
!  Generate looping indices
!
      projr = xcrd*ru1x + ycrd*ru1y + zcrd*ru1z
      max1u = (rmax - projr)*ra + nadd
      max1l = (rmax + projr)*ra + nadd
      max1l1 = max1l + 1
      xcdi = xcrd - max1l1*r1x
      ycdi = ycrd - max1l1*r1y
      zcdi = zcrd - max1l1*r1z
!
!  Molecule handling
!
      if (lmol) then
        indm = nmolind(i)
        izi = (indm/100) - 5
        ind = indm-100*(izi + 5)
        iyi = (ind/10) - 5
        ind = ind - 10*(iyi + 5)
        ixi = ind - 5
      endif
!
!  Loop over unit cells to find interatomic distances
!
      iiloop: do ii = - max1l,max1u
        xcdi = xcdi + r1x
        ycdi = ycdi + r1y
        zcdi = zcdi + r1z
!
        if (ldcellr) then
          if (ii.ge.-maxe1l.and.ii.le.maxe1u) cycle iiloop
        endif
        projr = xcdi*ru2x + ycdi*ru2y + zcdi*ru2z
        max2u = (rmax - projr)*rb + nadd
        max2l = (rmax + projr)*rb + nadd
        max2l1 = max2l + 1
!
        xcdj = xcdi - max2l1*r2x
        ycdj = ycdi - max2l1*r2y
        zcdj = zcdi - max2l1*r2z
        jjloop: do jj = - max2l,max2u
          xcdj = xcdj + r2x
          ycdj = ycdj + r2y
          zcdj = zcdj + r2z
!
          if (ldcellr) then
            if (jj.ge.-maxe2l.and.jj.le.maxe2u) cycle jjloop
          endif
          max3u = rmax*rc + nadd
          max3l = max3u
          max3l1 = max3l + 1
!
          xcd = xcdj - max3l1*r3x
          ycd = ycdj - max3l1*r3y
          zcd = zcdj - max3l1*r3z
          kkloop: do kk = - max3l,max3u
            xcd = xcd + r3x
            ycd = ycd + r3y
            zcd = zcd + r3z
            if (ldcellr) then
              if (kk.ge.-maxe3l.and.kk.le.maxe3u) cycle kkloop
            endif
            r = xcd*xcd + ycd*ycd + zcd*zcd
            if ((r.gt.r12.or.ldcellr).and.r.le.rmax2) then
!***********************************
!  Valid region 2a or 3-body atom  *
!***********************************
!
!  Check to see if there is space to store region 2 atom
!
              if (nlreg2.eq.maxr2at) then
                maxr2at = nlreg2 + 50
                call changemaxr2at
              endif
              if (lmol) then
                ix = ii - ixi
                iy = jj - iyi
                iz = kk - izi
                ind = (ix + 5) + 10*(iy + 5) + 100*(iz + 5)
              else
                ind = 555
              endif
              nlreg2 = nlreg2 + 1
              nr2a(nlreg2) = nat(i)
              ntr2a(nlreg2) = nftype(i)
              xr2a(nlreg2) = xcd + xdc
              yr2a(nlreg2) = ycd + ydc
              zr2a(nlreg2) = zcd + zdc
              qr2a(nlreg2) = qf(i)
              or2a(nlreg2) = occuf(i)
              rr2a(nlreg2) = radf(i)
              nmr2a(nlreg2) = natmol(i)
              nmir2a(nlreg2) = ind
              nps(nlreg2) = i
              ldbr2a(nlreg2) = lbsmat(nsft + nrelat(i))
              if (r.le.r22) nreg2 = nreg2 + 1
              if (r.le.r32) ntreg2 = ntreg2 + 1
              if (r.le.rp2) npreg2 = npreg2 + 1
            endif
          enddo kkloop
        enddo jjloop
      enddo iiloop
    endif
  enddo
  ncores = nlreg2
!
!  Locate shells by looping over cores and using lattice information
!
  do ii = 1,ncores
!
!  Look up equivalent perfect lattice site, i
!
    i = nps(ii)
!
!  Does this site have a shell j?
!
    j = ncsptr(i)
    if (j.gt.0) then
!
!  Shell found
!
      nlreg2 = nlreg2 + 1
!
!  Check to see if there is space to store region 2 atom
!
      if (nlreg2.gt.maxr2at) then
        maxr2at = nlreg2 + 50
        call changemaxr2at
      endif
!
!  Compute position of shell relative to the core
!
      xas = xr2a(ii) + csvector(1,i)
      yas = yr2a(ii) + csvector(2,i)
      zas = zr2a(ii) + csvector(3,i)
!
!  Compute radius
!
      r = (xas - xdc)**2 + (yas - ydc)**2 + (zas - zdc)**2
!
!  Add information to the list
!
      nr2a(nlreg2) = nat(j)
      ntr2a(nlreg2) = nftype(j)
      xr2a(nlreg2) = xas
      yr2a(nlreg2) = yas
      zr2a(nlreg2) = zas
      qr2a(nlreg2) = qf(j)
      or2a(nlreg2) = occuf(j)
      rr2a(nlreg2) = radf(j)
      nmr2a(nlreg2) = natmol(j)
      nmir2a(nlreg2) = nmir2a(ii)
      nps(nlreg2) = j
      ldbr2a(nlreg2) = lbsmat(nsft + nrelat(j))
      if (r.le.r22) nreg2 = nreg2 + 1
      if (r.le.r32) ntreg2 = ntreg2 + 1
      if (r.le.rp2) npreg2 = npreg2 + 1
    endif
  enddo
  if (.not.lnadd) nadd = 0
!
!  Sort region 2a 
!
  call sort2a
!
  return
  end
