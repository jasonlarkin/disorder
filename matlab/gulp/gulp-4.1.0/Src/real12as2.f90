  subroutine real12as2(eatom1,ereal1,erecip1,ec61,lgrad1,lgrad2)
!
!  Subroutine for calculating defect real energy and up to second derivatives
!
!  xdefe = region 1 coordinates
!  xclat = region 2 coordinates per unit cell
!
!  imode = 1 => defective region 1 - region 2
!
!   6/97 This version created from real12a based upon new strategy
!        of stored region 2 list
!   4/98 ESFF Lennard-Jones form now allowed for
!   1/99 1-4 interaction scaling added
!   9/01 lmolq calculations accelerated using lneedmol
!   2/02 lneedmol algorithm corrected
!  11/02 Wildcard atoms added
!   1/03 Wolf modifications made
!   1/03 Check on Ewald made for nptrmol as well as cut2e
!   4/05 Mods for cosh-spring added
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   5/12 third now used from numbers
!   3/13 Meaning of nreldef value changed
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
  use configurations, only : lbsmat,radcfg
  use constants
  use control
  use current
  use defects
  use derivatives
  use eam,            only : lMEAMden
  use element,        only : maxele
  use general,        only : cutw, etaw
  use kspace
  use molecule
  use numbers,        only : third
  use region2a
  use shell
  use sutton
  use times
  use two
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp),    intent(inout)                   :: eatom1
  real(dp),    intent(inout)                   :: ec61
  real(dp),    intent(inout)                   :: ereal1
  real(dp),    intent(inout)                   :: erecip1
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iar
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
  integer(i4)                                  :: indri
  integer(i4)                                  :: indrj
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: ixx2
  integer(i4)                                  :: iyy2
  integer(i4)                                  :: izz2
  integer(i4)                                  :: j
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: jxx2
  integer(i4)                                  :: jyy2
  integer(i4)                                  :: jzz2
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: neqvi
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: npsi
  integer(i4)                                  :: npsj
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lcspair
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lregion1j
  real(dp)                                     :: c6prod
  real(dp)                                     :: c6self1
  real(dp)                                     :: c6self1d2
  real(dp)                                     :: c6tot
  real(dp)                                     :: cmax
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: cut2w
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: emax
  real(dp)                                     :: eta3
  real(dp)                                     :: factor
  real(dp)                                     :: oci
  real(dp)                                     :: ocii
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rcheck
  real(dp)                                     :: rcheck2
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiffc
  real(dp)                                     :: rmiddle2
  real(dp)                                     :: rp
  real(dp)                                     :: rpd1
  real(dp)                                     :: rpd2
  real(dp)                                     :: rpd3
  real(dp)                                     :: rpd4
  real(dp)                                     :: rpd5
  real(dp)                                     :: rpd6
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: setrm
  real(dp)                                     :: small
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: twoeta3
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xdiffc
  real(dp)                                     :: ydiffc
  real(dp)                                     :: zdiffc
!
  time1 = cputime()
!
!  Local variables
!
  small = 1.0d-12
  if (lwolf) then
    twoeta3 = 2.0_dp*etaw*etaw*third
  else
    twoeta3 = 2.0_dp*eta*third
  endif
  if (lc6) then
    eta3 = eta*eta*eta
    c6self1 = 0.5_dp*eta3*third
    if (lgrad2) then
      c6self1d2 = 0.25_dp*eta3*eta
    endif
  endif
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real12as2','npotl')
!
  eatom1 = 0.0_dp
  ereal1 = 0.0_dp
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2e = rmx2
  cut2s = cuts*cuts
  cut2w = cutw*cutw
  if (lwolf) then        
    cut2q = cut2w        
  else
    cut2q = cut2e
  endif
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  emax = sqrt(cut2q)
  cmax = max(rpmax,emax)
  if (lnoreal) goto 999
!
!  Set up pointers to non-region 1 second derivatives
!
  indj = 3*nreg1
  if (ldbsm) indj = indj + nreg1
  jx = indj + 1
  jy = indj + 2
  jz = indj + 3
!**********************************
!  Region 1  -  region 1/2a energy  *
!**********************************
!
!  Outer loop over region1 asymmetric unit sites
!
  do i = 1,ndasym
    ii = ndsptr(i)
    indi = 3*(i - 1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
    nati = natdefe(ii)
    ntypi = ntypdefe(ii)
    qli = qdefe(ii)
    ocii = occdefe(ii)
    neqvi = ndeqv(i)
    oci = ocii*neqvi
    if (nreldef(ii).gt.0) then
      npsi = nreldef(ii)
    else
      npsi = 0
    endif
    if (ldefbsmat(ii)) then
      radi = radefe(ii)
      indri = 3*ndasym + i
    else
      radi = 0.0_dp
    endif
!
!  Molecule handling
!
    if (lmol) then
      nmi = ndefmol(ii)
      indmi = ndefind(ii)
      call mindtoijk(indmi,ixx,iyy,izz)
    endif
!
!  Find distance from the centre
!
    xdiffc = xal - xdc
    ydiffc = yal - ydc
    zdiffc = zal - zdc
    rdiffc = xdiffc*xdiffc + ydiffc*ydiffc+zdiffc*zdiffc
    rdiffc = sqrt(rdiffc)
    rcheck = rdiffc + cmax+0.5_dp
    rcheck2 = rcheck*rcheck
!***************************
!  Loop over old region 1  *
!***************************
    lregion1j = .true.
    do j = 1,nreg1old
      natj = natp(j)
      ntypj = ntypep(j)
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          lorder12 = .true.
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          lorder12 = .false.
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        lorder12 = .true.
        nat1 = nati
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
      xcrd = xperf(j) - xal
      ycrd = yperf(j) - yal
      zcrd = zperf(j) - zal
      qlj = qp(j)
      ocj = occp(j)
      ofct = oci*ocj
      factor = qli*qlj*ofct*angstoev
      npsj = npsite(j)
      iar = nsft + nrelat(npsj)
      if (lbsmat(iar)) then
        radj = radcfg(iar)
      else
        radj = 0.0_dp
      endif
      radsum = radi + radj
      lbonded = .false.
      l2bonds = .false.
      l3bonds = .false.
!
!  Possible core - shell flag
!
      lcspair = (abs(nat1 - nat2).eq.maxele.or.(ocii + ocj).lt.1.0001_dp)
      if (lmol) then
!
!  Molecule handling
!
        nmj = ndefmolp(j)
        indmj = ndefindp(j)
        lmolok = (nmi.eq.nmj.and.nmi.gt.0)
      else
        lmolok = .false.
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      c6tot = 0.0_dp
      lneedmol  =  (lmol.and..not.lmolq)
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol  =  .true.
            if (nptype(n).eq.8.or.nptype(n).eq.33) then
              if (cuts.gt.rp) rp = cuts
            elseif (lc6) then
              if (nptype(n).eq.1.or.nptype(n).eq.7) then
                c6tot = c6tot + twopot(3,n)
                if (repcut(n).gt.rp) rp = repcut(n)
              elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                c6tot = c6tot + twopot(2,n)
                if (repcut(n).gt.rp) rp = repcut(n)
              else
                if (rpot(n).gt.rp) rp = rpot(n)
              endif
            else
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r  =  cut2p
      cut2 = cut2r
      if (cut2e.gt.cut2.and.lewald) cut2  =  cut2e
      if (cut2w.gt.cut2.and.lwolf)  cut2  =  cut2w
      rp = sqrt(cut2)
!
!  Generate distance squared
!
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
!
!  Molecule  -  check index
!
      nmolonly = 0
      if (lmolok.and.(r.gt.cut2s.or..not.lcspair)) then
        ind = indmj - indmi
        lptrmol = (ind.eq.0)
        if (.not.lptrmol) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lptrmol) then
          if (r.gt.cut2q) nmolonly = 1
          if (npsi.gt.0) then
            call mindtoijk(indmj,jxx,jyy,jzz)
            jxx = jxx - ixx
            jyy = jyy - iyy
            jzz = jzz - izz
            indmmj = nmolind(npsj)
            indmmi = nmolind(npsi)
            call mindtoijk(indmmj,jxx2,jyy2,jzz2)
            call mindtoijk(indmmi,ixx2,iyy2,izz2)
            jxx = jxx + jxx2 - ixx2
            jyy = jyy + jyy2 - iyy2
            jzz = jzz + jzz2 - izz2
            call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
          else
            lbonded   = .false.
            l2bonds   = .false.
            l3bonds   = .false.
            nbtypeij  = 0
            nbtypeij2 = 0
          endif
        else
          lbonded   = .false.
          l2bonds   = .false.
          l3bonds   = .false.
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lptrmol   = .false.
        lbonded   = .false.
        l2bonds   = .false.
        l3bonds   = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
      if (r.lt.small) then
!
!  Remove electrostatic self energy
!
        setrm = factor*tweatpi
        erecip1 = erecip1 - setrm
        if (lc6) then
          c6prod = ofct*c6tot
          ec61 = ec61 + c6self1*c6prod
        endif
        if (lgrad2) then
          setrm = setrm*twoeta3
          if (lc6) setrm = setrm - c6prod*c6self1d2
          derv2(jx,ix) = derv2(jx,ix) - setrm
          derv2(jy,iy) = derv2(jy,iy) - setrm
          derv2(jz,iz) = derv2(jz,iz) - setrm
        endif
      elseif (r.le.cut2.or.lptrmol) then
!
!  Store vector
!
        dist = sqrt(r)
        call twobody1(eatom1,ereal1,ec61,lgrad1,lgrad2,.false.,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                      deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r,cut2q,cut2s, &
                      lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32,sctrm1,sctrm2,qli,qlj,lcspair, &
                      .true.,lregion1j,lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,lregion1j,.false., &
                      lorder12,d1i,d1j,d2i2,d2ij,d2j2)
!
!  Generate products for derivatives
!
        if (lgrad2) then
          rpd1 = xcrd*xcrd
          rpd2 = ycrd*ycrd
          rpd3 = zcrd*zcrd
          rpd4 = ycrd*zcrd
          rpd5 = xcrd*zcrd
          rpd6 = xcrd*ycrd
        endif
        if (lsuttonc) then
          if (.not.lMEAMden) then
            if (lorder12) then
              dscrho(1,i) = dscrho(1,i) + sctrm1*ocj
            else
              dscrho(1,i) = dscrho(1,i) + sctrm2*ocj
            endif
          endif
        endif
!***********************
!  Radial derivatives  *
!***********************
        if (lgrad1) then
          if (radi.gt.0.0_dp) then
            raderv(i) = raderv(i) + rtrm1
            if (lgrad2) then
              derv2(indrj,indri) = derv2(indrj,indri) + rtrm2
            endif
          endif
        endif
!**************************
!  Coordinate Derivatives *
!**************************
!
!  First derivatives
!
        if (lgrad1) then
          xdrv(i) = xdrv(i) - deriv*xcrd
          ydrv(i) = ydrv(i) - deriv*ycrd
          zdrv(i) = zdrv(i) - deriv*zcrd
          if (lgrad2.and.radi.gt.0.0_dp) then
            derv2(indrj,ix) = derv2(indrj,ix) - rderiv*xcrd
            derv2(indrj,iy) = derv2(indrj,iy) - rderiv*ycrd
            derv2(indrj,iz) = derv2(indrj,iz) - rderiv*zcrd
          endif
        endif
!
!  Second derivatives
!
        if (lgrad2) then
          derv2(jx,ix) = derv2(jx,ix) - deriv2*rpd1
          derv2(jx,iy) = derv2(jx,iy) - deriv2*rpd6
          derv2(jx,iz) = derv2(jx,iz) - deriv2*rpd5
          derv2(jy,iy) = derv2(jy,iy) - deriv2*rpd2
          derv2(jy,iz) = derv2(jy,iz) - deriv2*rpd4
          derv2(jz,iz) = derv2(jz,iz) - deriv2*rpd3
          derv2(jx,ix) = derv2(jx,ix) - deriv
          derv2(jy,iy) = derv2(jy,iy) - deriv
          derv2(jz,iz) = derv2(jz,iz) - deriv
!
!  Coordinate  -  radius mixed
!
          if (radi.gt.0.0_dp) then
            derv2(jx,indri) = derv2(jx,indri) + rderiv*xcrd
            derv2(jy,indri) = derv2(jy,indri) + rderiv*ycrd
            derv2(jz,indri) = derv2(jz,indri) + rderiv*zcrd
          endif
        endif
!
!  Symmetrise second derivative matrix
!
        if (lgrad2) then
          derv2(jy,ix) = derv2(jx,iy)
          derv2(jz,ix) = derv2(jx,iz)
          derv2(jz,iy) = derv2(jy,iz)
        endif
      endif
    enddo
!************************
!  Loop over region 2a  *
!************************
    lregion1j = .false.
    rmiddle2 = 0.0_dp
    j = 0
    do while (j.lt.npreg2.and.rmiddle2.lt.rcheck2)
      j = j + 1
      xcrd = xr2a(j) - xal
      ycrd = yr2a(j) - yal
      zcrd = zr2a(j) - zal
      if (.not.lmol) then
!
!  If not a molecular calc and component exceeds maximum
!  cut - off then there is nothing to evaluate
!
        if (abs(xcrd).gt.cmax) goto 10
        if (abs(ycrd).gt.cmax) goto 10
        if (abs(zcrd).gt.cmax) goto 10
      endif
      natj = nr2a(j)
      ntypj = ntr2a(j)
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          lorder12 = .true.
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          lorder12 = .false.
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        lorder12 = .true.
        nat1 = nati
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
      qlj = qr2a(j)
      ocj = or2a(j)
      ofct = oci*ocj
      factor = qli*qlj*ofct*angstoev
      npsj = nps(j)
      iar = nsft + nrelat(npsj)
      if (lbsmat(iar)) then
        radj = radcfg(iar)
        indrj = 3*nreg1 + j
      else
        radj = 0.0_dp
      endif
      radsum = radi + radj
      lbonded = .false.
      l2bonds = .false.
      l3bonds = .false.
!
!  Distance check relative to centre of region 1
!
      xdiffc = xr2a(j) - xdc
      ydiffc = yr2a(j) - ydc
      zdiffc = zr2a(j) - zdc
      rmiddle2 = xdiffc*xdiffc + ydiffc*ydiffc+zdiffc*zdiffc
!
!  Possible core - shell flag
!
      lcspair = (abs(nat1 - nat2).eq.maxele.or.(ocii + ocj).lt.1.0001_dp)
      if (lmol) then
!
!  Molecule handling
!
        nmj = nmr2a(j)
        indmj = nmir2a(j)
        lmolok = (nmi.eq.nmj.and.nmi.gt.0)
      else
        lmolok = .false.
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      c6tot = 0.0_dp
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (nptype(n).eq.8.or.nptype(n).eq.33) then
              if (cuts.gt.rp) rp = cuts
            elseif (lc6) then
              if (nptype(n).eq.1.or.nptype(n).eq.7) then
                c6tot = c6tot + twopot(3,n)
                if (repcut(n).gt.rp) rp = repcut(n)
              elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                c6tot = c6tot + twopot(2,n)
                if (repcut(n).gt.rp) rp = repcut(n)
              else
                if (rpot(n).gt.rp) rp = rpot(n)
              endif
            else
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2r  =  rp*rp
      if (cut2r.gt.cut2p) cut2r  =  cut2p
      cut2  =  cut2r
      if (cut2e.gt.cut2.and.lewald) cut2  =  cut2e
      if (cut2w.gt.cut2.and.lwolf)  cut2  =  cut2w
      rp  =  sqrt(cut2)
!
!  Generate distance squared
!
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule  -  check index
!
      if (lmolok.and.(r.gt.cut2s.or..not.lcspair)) then
        ind = indmj - indmi
        lptrmol = (ind.eq.0)
        if (.not.lptrmol) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lptrmol) then
          if (npsi.gt.0) then
            call mindtoijk(indmj,jxx,jyy,jzz)
            jxx = jxx - ixx
            jyy = jyy - iyy
            jzz = jzz - izz
            indmmj = nmolind(npsj)
            indmmi = nmolind(npsi)
            call mindtoijk(indmmj,jxx2,jyy2,jzz2)
            call mindtoijk(indmmi,ixx2,iyy2,izz2)
            jxx = jxx + jxx2 - ixx2
            jyy = jyy + jyy2 - iyy2
            jzz = jzz + jzz2 - izz2
            call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
          else
            lbonded   = .false.
            l2bonds   = .false.
            l3bonds   = .false.
            nbtypeij  = 0
            nbtypeij2 = 0
          endif
        else
          lbonded   = .false.
          l2bonds   = .false.
          l3bonds   = .false.
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lptrmol   = .false.
        lbonded   = .false.
        l2bonds   = .false.
        l3bonds   = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
      if (r.le.cut2.or.lptrmol) then
!
!  Store vector
!
        dist = sqrt(r)
        call twobody1(eatom1,ereal1,ec61,lgrad1,lgrad2,.false.,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                      deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r,cut2q,cut2s, &
                      lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32,sctrm1,sctrm2,qli,qlj,lcspair, &
                      .true.,lregion1j,lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,lregion1j,.false., &
                      lorder12,d1i,d1j,d2i2,d2ij,d2j2)
        if (lgrad1) then
          call rpair2(i,j,indri,ix,iy,iz,jx,jy,jz,lgrad2,xcrd,ycrd,zcrd,deriv,deriv2,rderiv,rtrm1, &
                      rtrm2,radi,radj,indrj)
        endif
        if (lsuttonc) then
!
!  Need to divide sctrm by symmetry factor
!
          if (.not.lMEAMden) then
            if (lorder12) then
              dscrho(1,i) = dscrho(1,i) + sctrm1*ocj
            else
              dscrho(1,i) = dscrho(1,i) + sctrm2*ocj
            endif
          endif
        endif
        if (lgrad2) then
!
!  Symmetrise second derivative matrix
!
          derv2(jy,ix) = derv2(jx,iy)
          derv2(jz,ix) = derv2(jx,iz)
          derv2(jz,iy) = derv2(jy,iz)
        endif
      endif
 10   continue
    enddo
  enddo
!
!  End of loop over region 2
!
999 continue
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real12as2','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
