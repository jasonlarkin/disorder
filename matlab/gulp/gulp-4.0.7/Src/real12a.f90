  subroutine real12a(eatom1,ereal1,erecip1,ec61,lgrad1,lgrad2,imode)
!
!  Subroutine for calculating defect real energy and up to second derivatives
!
!  xdefe = region 1 coordinates
!  xclat = region 2 coordinates per unit cell
!
!  imode = 1 => defective region 1 - region 2
!  imode = 2 => perfect region 1 - region 2
!
!   3/95 modified for periodic molecules
!   8/95 repcut modification added
!   8/95 Ewald sum for dispersion sums added
!   4/98 ESFF Lennard-Jones form now allowed for
!   1/99 1-4 interaction scaling added
!   9/01 lmolq calculations accelerated using lneedmol
!   2/02 lneedmol algorithm corrected
!   5/02 BSM made core/shell specific and exponential form added
!   5/02 Use of variable bpt before defined corrected
!   5/02 Initialisation of nmolonly fixed
!  11/02 Wildcard atoms added
!   1/03 Wolf modifications made
!   1/03 Check on Ewald made for nptrmol as well as cut2e
!   3/03 Nadd increased for small angles
!   4/05 Mods for cosh-spring added
!  10/06 Contribution from region 1 (def) - region 1 (perf) handled
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!  11/08 x/y/z components passed to twobody1
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Core-shell offset vector now obtained from preset csvector array
!   1/09 Test for whether to call twobody1 now allows for r > cutoff, but ljinregion1 = .true.
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   4/10 Iterative algorithm introduced for general cell case searching
!   5/12 third now used from numbers
!   7/12 Subtraction of shell spring constant removed when distance is less than 
!        small as this leads to incorrect second derivatives
!  11/12 Unused variables removed
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
  use configurations, only : lbsmat,radcfg
  use constants
  use control
  use current
  use defects
  use derivatives
  use eam,            only : maxmeamcomponent
  use element,        only : maxele
  use general
  use kspace
  use molecule
  use numbers,        only : third
  use shell
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: imode
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
  integer(i4)                                  :: idir
  integer(i4)                                  :: ii
  integer(i4)                                  :: ii2
  integer(i4)                                  :: imid
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indri
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixm
  integer(i4)                                  :: iym
  integer(i4)                                  :: izm
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jdir
  integer(i4)                                  :: jj2
  integer(i4)                                  :: jmid
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: kdir
  integer(i4)                                  :: kk
  integer(i4)                                  :: kk2
  integer(i4)                                  :: kmid
  integer(i4)                                  :: m
  integer(i4)                                  :: max1l
  integer(i4)                                  :: max1l1
  integer(i4)                                  :: max1u
  integer(i4)                                  :: max2l
  integer(i4)                                  :: max2l1
  integer(i4)                                  :: max2u
  integer(i4)                                  :: max3l
  integer(i4)                                  :: max3l1
  integer(i4)                                  :: max3u
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nloop
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: npsi
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: nvec
  integer(i4)                                  :: nvecj0
  integer(i4)                                  :: nveck0
  integer(i4)                                  :: status
  logical                                      :: l111
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lallfound1
  logical                                      :: lallfound2
  logical                                      :: lallfound3
  logical                                      :: lbonded
  logical                                      :: lcspair
  logical                                      :: ljinregion1
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lnadd
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  real(dp)                                     :: apt
  real(dp)                                     :: bpt
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
  real(dp)                                     :: etrm
  real(dp)                                     :: etrm1
  real(dp)                                     :: etrm2
  real(dp)                                     :: factor
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: proj1
  real(dp)                                     :: proj2
  real(dp)                                     :: proj3
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: r12
  real(dp)                                     :: r2
  real(dp)                                     :: r2i
  real(dp)                                     :: r2j
  real(dp)                                     :: r2k
  real(dp)                                     :: ra
  real(dp)                                     :: rad2
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rb
  real(dp)                                     :: rc
  real(dp)                                     :: rcx1
  real(dp)                                     :: rcy1
  real(dp)                                     :: rcz1
  real(dp)                                     :: rcx2
  real(dp)                                     :: rcy2
  real(dp)                                     :: rcz2
  real(dp)                                     :: rcx3
  real(dp)                                     :: rcy3
  real(dp)                                     :: rcz3
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiff
  real(dp)                                     :: rnorm
  real(dp)                                     :: rp
  real(dp)                                     :: rpres
  real(dp)                                     :: rpres2
  real(dp)                                     :: rpres3
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: rx
  real(dp)                                     :: ry
  real(dp)                                     :: rz
  real(dp)                                     :: rxi
  real(dp)                                     :: ryi
  real(dp)                                     :: rzi
  real(dp)                                     :: rxj
  real(dp)                                     :: ryj
  real(dp)                                     :: rzj
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: setrm
  real(dp)                                     :: small
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: twoeta3
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcd
  real(dp)                                     :: xcd2
  real(dp)                                     :: ycd
  real(dp)                                     :: ycd2
  real(dp)                                     :: zcd
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xij
  real(dp)                                     :: yij
  real(dp)                                     :: zij
  real(dp)                                     :: xoffset
  real(dp)                                     :: yoffset
  real(dp)                                     :: zoffset
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
  if (status/=0) call outofmemory('real12a','npotl')
!
  eatom1 = 0.0_dp
  ereal1 = 0.0_dp
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
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2e = rmx2
  cut2w = cutw*cutw
  cut2s = cuts*cuts
  if (lwolf) then        
    cut2q = cut2w        
  else
    cut2q = cut2e
  endif
  r12 = reg1(ncf)**2
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  emax = sqrt(cut2q)
  cmax = max(rpmax,emax,2.0_dp*reg1(ncf))
  l111 = .true.
  if (a.lt.cmax.or.b.lt.cmax.or.c.lt.cmax) l111 = .false.
  if (alpha.lt.80.0_dp.or.beta.lt.80.0_dp.or.gamma.lt.80.0_dp) l111 = .false.
  if (alpha.gt.100.0_dp.or.beta.gt.100.0_dp.or.gamma.gt.100.0_dp) l111 = .false.
  if (imode.eq.1) then
    nloop = nreg1
  elseif (imode.eq.2) then
    nloop = nreg1old
  endif
  if (lnoreal) goto 999
!
!  Set up pointers to non-region 1 second derivatives
!
  indj = 3*nloop
  if (ldbsm) indj = indj + nloop
  jx = indj + 1
  jy = indj + 2
  jz = indj + 3
!*******************************
!  Region 1 - region 2 energy  *
!*******************************
!
!  Outer loop over sites
!
  do i = 1,nloop
    indi = 3*(i-1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
!
!  Inner loop over second site
!
    if (imode.eq.1) then
      xal = xdefe(i)
      yal = ydefe(i)
      zal = zdefe(i)
      nati = natdefe(i)
      ntypi = ntypdefe(i)
      qli = qdefe(i)
      oci = occdefe(i)
      if (nreldef(i).gt.0) then
        npsi = npsite(nreldef(i))
      else
        npsi = 0
      endif
      if (ldefbsmat(i)) then
        radi = radefe(i)
        indri = 3*nloop + i
      else
        radi = 0.0_dp
      endif
    elseif (imode.eq.2) then
      xal = xperf(i)
      yal = yperf(i)
      zal = zperf(i)
      nati = natp(i)
      ntypi = ntypep(i)
      qli = qp(i)
      oci = occp(i)
      npsi = npsite(i)
      iar = nsft + nrelat(npsi)
      if (lbsmat(iar)) then
        radi = radcfg(iar)
        indri = 3*nloop + i
      else
        radi = 0.0_dp
      endif
    endif
!
!  Molecule handling
!
    if (lmol) then
      if (imode.eq.1) then
        nmi = ndefmol(i)
        indm = ndefind(i)
      elseif (imode.eq.2) then
        nmi = ndefmolp(i)
        indm = ndefindp(i)
      endif
      call mindtoijk(indm,ixi,iyi,izi)
    endif
!
!  Start of second atom loop
!
    do j = 1,numat
      natj = nat(j)
      ntypj = nftype(j)
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
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      if (natj.gt.maxele) then
!
!  For shells, add the core-shell offset vector since region test only looks at cores
!
        xoffset = xal - xdc + csvector(1,j) 
        yoffset = yal - ydc + csvector(2,j) 
        zoffset = zal - zdc + csvector(3,j) 
      else
        xoffset = xal - xdc
        yoffset = yal - ydc
        zoffset = zal - zdc
      endif
      qlj = qf(j)
      ocj = occuf(j)
      ofct = oci*ocj
      factor = qli*qlj*ofct*angstoev
      iar = nsft + nrelat(j)
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
!  Possible core-shell flag
!
      lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
      if (lmol) then
!
!  Molecule handling
!
        nmj = natmol(j)
        indmj = nmolind(j)
        call mindtoijk(indmj,ixj,iyj,izj)
        ixj = ixj + ixi
        iyj = iyj + iyi
        izj = izj + izi
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
      lneedmol = (lmol.and..not.lmolq)
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol = .true.
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
      if (cut2r.gt.cut2p) cut2r = cut2p
      cut2 = cut2r
      if (cut2e.gt.cut2.and.lewald) cut2 = cut2e
      if (cut2w.gt.cut2.and.lwolf) cut2 = cut2w
      cut2 = max(cut2,4.0_dp*r12)
      rp = sqrt(cut2)
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
!*******************************************************
!  Loop over unit cells to find interatomic distances  *
!*******************************************************
      if (lra) then
!
!  Right angled cell
!
        if (l111) then
          xcd = xcrd - 2.0_dp*r1x
          do ii = -1,1
            xcd = xcd + r1x
            xcd2 = xcd*xcd
            ycd = ycrd - 2.0_dp*r2y
            do jj = -1,1
              ycd = ycd + r2y
              ycd2 = ycd*ycd
              zcd = zcrd - 2.0_dp*r3z
              do kk = -1,1
                zcd = zcd + r3z
                r = xcd2 + ycd2 + zcd*zcd
                rad2 = (xcd + xoffset)**2 + (ycd + yoffset)**2 + (zcd + zoffset)**2
                ljinregion1 = (rad2.lt.r12)
!
!  Molecule - check index
!
                nmolonly = 0
                if (lmolok.and.r.gt.cut2s) then
                  call samemol(lptrmol,nmi,ii,jj,kk,ixj,iyj,izj)
                  if (lptrmol) then
                    if (r.gt.cut2q) nmolonly = 1
                  endif
                  if (lptrmol.and.npsi.gt.0) then
                    ind = nmolind(npsi)
                    call mindtoijk(ind,ixm,iym,izm)
                    ii2 = ii - (ixi+ixm)
                    jj2 = jj - (iyi+iym)
                    kk2 = kk - (izi+izm)
                    call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,j,ii2,jj2,kk2)
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
                    derv2(ix,jx) = derv2(ix,jx) - setrm
                    derv2(iy,jy) = derv2(iy,jy) - setrm
                    derv2(iz,jz) = derv2(iz,jz) - setrm
                  endif
!
!  Breathing shell self terms
!
                  if (radi.gt.0.0_dp.and.nati.eq.natj) then
                    do m = 1,npote
                      if (nptype(m).eq.14) then
                        if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                          apt = twopot(1,m)*oci
                          rdiff = radi - twopot(2,m)
                          eatom1 = eatom1 + 0.5_dp*apt*rdiff*rdiff
                          if (lgrad1) then
                            raderv(i) = raderv(i) + apt*rdiff
                            if (lgrad2) then
                              derv2(indri,indri) = derv2(indri,indri) + apt
                            endif
                          endif
                        endif
                      elseif (nptype(m).eq.17) then
                        if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                          apt = twopot(1,m)*oci
                          bpt = twopot(2,m)
                          rdiff = radi - twopot(3,m)
                          etrm1 = exp(bpt*rdiff)
                          etrm2 = 1.0_dp/etrm1
                          etrm = apt*(etrm1 + etrm2)
                          eatom1 = eatom1 + etrm
                          if (lgrad1) then
                            raderv(i) = raderv(i) + bpt*(etrm1 - etrm2)
                            if (lgrad2) then
                              derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                            endif
                          endif
                        endif
                      elseif (nptype(m).eq.31) then
                        if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                          apt = twopot(1,m)*oci
                          bpt = twopot(2,m)
                          rdiff = radi - twopot(3,m)
                          etrm1 = exp(bpt*rdiff)
                          etrm = apt*etrm1
                          eatom1 = eatom1 + etrm
                          if (lgrad1) then
                            raderv(i) = raderv(i) + bpt*etrm1
                            if (lgrad2) then
                              derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                            endif
                          endif
                        endif
                      endif
                    enddo
                  endif
                elseif (r.le.cut2.or.lptrmol.or.ljinregion1) then
!
!  Store vector
!
                  dist = sqrt(r)
                  call twobody1(eatom1,ereal1,ec61,lgrad1,lgrad2,.false.,1_i4,1_i4,dist,xcd,ycd,zcd,d0i,d0j, &
                                deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r, &
                                cut2q,cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                                sctrm1,sctrm2,qli,qlj,lcspair,.true.,ljinregion1,lbonded,l2bonds,l3bonds, &
                                nbtypeij,nbtypeij2,ljinregion1,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
                  if (lgrad1) then
                    call rpair(i,j,indri,ix,iy,iz,jx,jy,jz,lgrad2,xcd,ycd,zcd,deriv,deriv2,rderiv, &
                               rtrm1,rtrm2,radi,radj)
                  endif
                endif
              enddo
            enddo
          enddo
        else
          max1u = (rp-xcrd)*ra + nadd
          max1l = (rp+xcrd)*ra + nadd
          max1l1 = max1l + 1
          xcd = xcrd - max1l1*r1x
          do ii = -max1l,max1u
            xcd = xcd + r1x
            if (abs(xcd).lt.rp) then
              xcd2 = xcd*xcd
              rpres2 = rp*rp - xcd2
              rpres = sqrt(rpres2)
              max2u = (rpres-ycrd)*rb + nadd
              max2l = (rpres+ycrd)*rb + nadd
              max2l1 = max2l + 1
              ycd = ycrd - max2l1*r2y
              do jj = -max2l,max2u
                ycd = ycd + r2y
                ycd2 = ycd*ycd
                rpres3 = rpres2 - ycd2
                if (rpres3.gt.0.0_dp) then
                  rpres3 = sqrt(rpres3)
                  max3u = (rpres3-zcrd)*rc + 1
                  max3l = (rpres3+zcrd)*rc + 1
                  max3l1 = max3l + 1
                  zcd = zcrd - max3l1*r3z
                  do kk = -max3l,max3u
                    zcd = zcd + r3z
                    r = xcd2 + ycd2 + zcd*zcd
                    rad2 = (xcd + xoffset)**2 + (ycd + yoffset)**2 + (zcd + zoffset)**2
                    ljinregion1 = (rad2.lt.r12)
                    nmolonly = 0
                    if (lmolok.and.r.gt.cut2s) then
                      call samemol(lptrmol,nmi,ii,jj,kk,ixj,iyj,izj)
                      if (lptrmol) then
                        if (r.gt.cut2q) nmolonly = 1
                      endif
                      if (lptrmol.and.npsi.gt.0) then
                        ind = nmolind(npsi)
                        call mindtoijk(ind,ixm,iym,izm)
                        ii2 = ii - (ixi+ixm)
                        jj2 = jj - (iyi+iym)
                        kk2 = kk - (izi+izm)
                        call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,j,ii2,jj2,kk2)
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
                        derv2(ix,jx) = derv2(ix,jx) - setrm
                        derv2(iy,jy) = derv2(iy,jy) - setrm
                        derv2(iz,jz) = derv2(iz,jz) - setrm
                      endif
!
!  Breathing shell self terms
!
                      if (radi.gt.0.0_dp.and.nati.eq.natj) then
                        do m = 1,npote
                          if (nptype(m).eq.14) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              rdiff = radi - twopot(2,m)
                              eatom1 = eatom1 + 0.5_dp*apt*rdiff*rdiff
                              if (lgrad1) then
                                raderv(i) = raderv(i) + apt*rdiff
                                if (lgrad2) derv2(indri,indri) = derv2(indri,indri) + apt
                              endif
                            endif
                          elseif (nptype(m).eq.17) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              bpt = twopot(2,m)
                              rdiff = radi - twopot(3,m)
                              etrm1 = exp(bpt*rdiff)
                              etrm2 = 1.0_dp/etrm1
                              etrm = apt*(etrm1 + etrm2)
                              eatom1 = eatom1 + etrm
                              if (lgrad1) then
                                raderv(i) = raderv(i) + bpt*(etrm1 - etrm2)
                                if (lgrad2) then
                                  derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                                endif
                              endif
                            endif
                          elseif (nptype(m).eq.31) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              bpt = twopot(2,m)
                              rdiff = radi - twopot(3,m)
                              etrm1 = exp(bpt*rdiff)
                              etrm = apt*etrm1
                              eatom1 = eatom1 + etrm
                              if (lgrad1) then
                                raderv(i) = raderv(i) + bpt*etrm1
                                if (lgrad2) then
                                  derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                                endif
                              endif
                            endif
                          endif
                        enddo
                      endif
                    elseif (r.le.cut2.or.lptrmol.or.ljinregion1) then
!
!  Store vector
!
                      dist = sqrt(r)
                      call twobody1(eatom1,ereal1,ec61,lgrad1,lgrad2,.false.,1_i4,1_i4,dist,xcd,ycd,zcd,d0i,d0j, &
                                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                                    cut2r,cut2q,cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2, &
                                    rtrm3,rtrm32,sctrm1,sctrm2,qli,qlj,lcspair,.true.,ljinregion1,lbonded,l2bonds, &
                                    l3bonds,nbtypeij,nbtypeij2,ljinregion1,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
                      if (lgrad1) then
                        call rpair(i,j,indri,ix,iy,iz,jx,jy,jz,lgrad2,xcd,ycd,zcd,deriv,deriv2,rderiv, &
                                   rtrm1,rtrm2,radi,radj)
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
!  Search unit cells iteratively for all images
!
        xij = xcrd
        yij = ycrd
        zij = zcrd
!
!  Find projection of cell vector 3 on to i - j vector
!
        rnorm = xij*xij + yij*yij + zij*zij
        if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
        proj3 = rnorm*recipc*(xij*r3x + yij*r3y + zij*r3z)
        kmid = nint(proj3)
        xij = xij - kmid*r3x
        yij = yij - kmid*r3y
        zij = zij - kmid*r3z
!
!  Find projection of cell vector 2 on to i - j vector
!
        rnorm = xij*xij + yij*yij + zij*zij
        if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
        proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
        jmid = nint(proj2)
        xij = xij - jmid*r2x
        yij = yij - jmid*r2y
        zij = zij - jmid*r2z
!
!  Find projection of cell vector 1 on to i - j vector
!
        rnorm = xij*xij + yij*yij + zij*zij
        if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
        proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
        imid = nint(proj1)
        xij = xij - imid*r1x
        yij = yij - imid*r1y
        zij = zij - imid*r1z
!
!  Initialise number of distances to zero
!
        nvec = 0
!
!  Outer loop over first cell vector direction
!
        do idir = 1,-1,-2
!
!  Reinitialise distance squared
!
          r2i = 10000.0_dp*cut2
!
!  Loop over first cell vector
!
          lallfound1 = .false.
          if (idir.eq.1) then
            ii = 0
          else
            ii = - 1
          endif
!
!  Set initial coordinate vector
!
          rxi = xij + dble(ii)*r1x
          ryi = yij + dble(ii)*r1y
          rzi = zij + dble(ii)*r1z
!
!  Set increment vector
!
          rcx1 = dble(idir)*r1x
          rcy1 = dble(idir)*r1y
          rcz1 = dble(idir)*r1z
!
          do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
            nvecj0 = nvec
!
!  Outer loop over second cell vector direction
!
            do jdir = 1,-1,-2
!
!  Reinitialise saved distance squared
!
              r2j = 10000.0_dp*cut2
!
!  Loop over second cell vector
!
              lallfound2 = .false.
              if (jdir.eq.1) then
                jj = 0
              else
                jj = - 1
              endif
!
!  Set initial coordinate vector
!
              rxj = rxi + dble(jj)*r2x
              ryj = ryi + dble(jj)*r2y
              rzj = rzi + dble(jj)*r2z
!
!  Set increment vector
!
              rcx2 = dble(jdir)*r2x
              rcy2 = dble(jdir)*r2y
              rcz2 = dble(jdir)*r2z
!
              do while (.not.lallfound2)
!
!  Save number of vectors before search over third direction
!
                nveck0 = nvec
!
!  Outer loop over third cell vector direction
!
                do kdir = 1,-1,-2
!
!  Reinitialise saved distance squared
!
                  r2k = 10000.0_dp*cut2
!
!  Loop over third cell vector
!
                  lallfound3 = .false.
                  if (kdir.eq.1) then
                    kk = 0
                  else
                    kk = - 1
                  endif
!
!  Set initial coordinate vector
!
                  rx = rxj + dble(kk)*r3x
                  ry = ryj + dble(kk)*r3y
                  rz = rzj + dble(kk)*r3z
!
!  Set increment vector
!
                  rcx3 = dble(kdir)*r3x
                  rcy3 = dble(kdir)*r3y
                  rcz3 = dble(kdir)*r3z
!
                  do while (.not.lallfound3)
!
!  Calculate square of distance
!
                    r2 = rx*rx + ry*ry + rz*rz
                    rad2 = (rx + xoffset)**2 + (ry + yoffset)**2 + (rz + zoffset)**2
                    ljinregion1 = (rad2.lt.r12)
!
!  Molecule  -  check index
!
                    nmolonly = 0
                    if (lmolok.and.r2.gt.cut2s) then
                      call samemol(lptrmol,nmi,ii-imid,jj-jmid,kk-kmid,ixj,iyj,izj)
                      if (lptrmol) then
                        if (r2.gt.cut2q) nmolonly = 1
                      endif
                      if (lptrmol.and.npsi.gt.0) then
                        ind = nmolind(npsi)
                        call mindtoijk(ind,ixm,iym,izm)
                        ii2 = ii - (ixi + ixm) - imid
                        jj2 = jj - (iyi + iym) - jmid
                        kk2 = kk - (izi + izm) - kmid
                        call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,j,ii2,jj2,kk2)
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
                    if (r2.lt.small) then
!
!  Increment number of vectors
!
                      nvec = nvec + 1
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
                        derv2(ix,jx) = derv2(ix,jx) - setrm
                        derv2(iy,jy) = derv2(iy,jy) - setrm
                        derv2(iz,jz) = derv2(iz,jz) - setrm
                      endif
!
!  Breathing shell self terms
!
                      if (radi.gt.0.0_dp.and.nati.eq.natj) then
                        do m = 1,npote
                          if (nptype(m).eq.14) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              rdiff = radi - twopot(2,m)
                              eatom1 = eatom1 + 0.5_dp*apt*rdiff*rdiff
                              if (lgrad1) then
                                raderv(i) = raderv(i) + apt*rdiff
                                if (lgrad2) derv2(indri,indri) = derv2(indri,indri) + apt
                              endif
                            endif
                          elseif (nptype(m).eq.17) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              bpt = twopot(2,m)
                              rdiff = radi - twopot(3,m)
                              etrm1 = exp(bpt*rdiff)
                              etrm2 = 1.0_dp/etrm1
                              etrm = apt*(etrm1 + etrm2)
                              eatom1 = eatom1 + etrm
                              if (lgrad1) then
                                raderv(i) = raderv(i) + bpt*(etrm1 - etrm2)
                                if (lgrad2) then
                                  derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                                endif
                              endif
                            endif
                          elseif (nptype(m).eq.31) then
                            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                              apt = twopot(1,m)*oci
                              bpt = twopot(2,m)
                              rdiff = radi - twopot(3,m)
                              etrm1 = exp(bpt*rdiff)
                              etrm = apt*etrm1
                              eatom1 = eatom1 + etrm
                              if (lgrad1) then
                                raderv(i) = raderv(i) + bpt*etrm1
                                if (lgrad2) then
                                  derv2(indri,indri) = derv2(indri,indri) + bpt*bpt*etrm
                                endif
                              endif
                            endif
                          endif
                        enddo
                      endif
                    elseif (r2.le.cut2.or.lptrmol.or.ljinregion1) then
!
!  Increment number of vectors
!
                      nvec = nvec + 1
!
!  Store vector
!
                      dist = sqrt(r2)
                      call twobody1(eatom1,ereal1,ec61,lgrad1,lgrad2,.false.,1_i4,1_i4,dist,rx,ry,rz,d0i,d0j, &
                                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r, &
                                    cut2q,cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                                    sctrm1,sctrm2,qli,qlj,lcspair,.true.,ljinregion1,lbonded,l2bonds,l3bonds, &
                                    nbtypeij,nbtypeij2,ljinregion1,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
                      if (lgrad1) then
                        call rpair(i,j,indri,ix,iy,iz,jx,jy,jz,lgrad2,rx,ry,rz,deriv,deriv2, &
                                   rderiv,rtrm1,rtrm2,radi,radj)
                      endif
                    endif
!
!  Increment by third vector
!
                    kk = kk + kdir
                    rx = rx + rcx3
                    ry = ry + rcy3
                    rz = rz + rcz3
!
!  Check to see if this direction is complete
!
                    lallfound3 = (r2.gt.r2k.and.r2.gt.cut2)
                    r2k = r2
                  enddo
                enddo
!
!  Increment by second vector
!
                jj = jj + jdir
                rxj = rxj + rcx2
                ryj = ryj + rcy2
                rzj = rzj + rcz2
!
!  Check to see if this direction is complete
!
                lallfound2 = (r2.gt.r2j.and.r2.gt.cut2.and.nvec.eq.nveck0)
                r2j = r2
              enddo
            enddo
!
!  Increment by first vector
!
            ii = ii + idir
            rxi = rxi + rcx1
            ryi = ryi + rcy1
            rzi = rzi + rcz1
!
!  Check to see if this direction is complete
!
            lallfound1 = (r2.gt.r2i.and.r2.gt.cut2.and.nvec.eq.nvecj0)
            r2i = r2
          enddo
        enddo
      endif
      if (lgrad2) then
!
!  Symmetrise second derivative matrix
!
        derv2(iy,jx) = derv2(ix,jy)
        derv2(iz,jx) = derv2(ix,jz)
        derv2(iz,jy) = derv2(iy,jz)
      endif
    enddo
  enddo
!
!  End of real space part
!
!****************************************
!  Symmetrise second derivative matrix  *
!****************************************
999 if (lgrad2) then
    do i = 1,indj
      derv2(jx,i) = derv2(i,jx)
      derv2(jy,i) = derv2(i,jy)
      derv2(jz,i) = derv2(i,jz)
    enddo
    if (ldbsm) then
      ind = 3*nloop
      do i = 1,nloop
        do j = 1,ind
          derv2(j,ind+i) = derv2(ind+i,j)
        enddo
      enddo
    endif
  endif
  if (.not.lnadd) nadd = 0
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real12a','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
  return
  end
