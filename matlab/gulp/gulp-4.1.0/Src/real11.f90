  subroutine real11(eatom,ereal,lgrad1,lgrad2,imode)
!
!  Subroutine for calculating the region 1 - region 1 energy
!
!  imode = 1 => defective region 1 - defective region 1 
!  imode = 2 => perfect region 1 - perfect region 1
!  imode = 3 => defective region 1 - perfect region 1
!  imode = 4 => perfect region 1 - perfect region 1, but i=1,n/j=1,n
!
!  In imode=2 the derivatives shouldn't be calculated and routine
!  is not set up for these.
!
!   3/95 Corrections added for periodic molecules
!   1/99 1-4 interaction scaling added
!   9/01 lmolq calculations accelerated using lneedmol
!   2/02 lneedmol algorithm corrected
!   5/02 BSM made core/shell specific and exponential form added
!  11/02 Wildcard atoms added
!   4/05 Mods for cosh-spring added
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   5/08 Defect bonding array structure changed
!  11/08 x/y/z components passed to twobody1
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
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
  use eam,            only : maxmeamcomponent
  use element,        only : maxele
  use general,        only : cutw
  use molecule
  use shell
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: imode
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: ereal
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iar
  integer(i4)                                  :: icm
  integer(i4)                                  :: imm
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
  integer(i4)                                  :: indri
  integer(i4)                                  :: indrj
  integer(i4)                                  :: ione
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
  integer(i4)                                  :: jcm
  integer(i4)                                  :: jmm
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: jxx2
  integer(i4)                                  :: jyy2
  integer(i4)                                  :: jzz2
  integer(i4)                                  :: k
  integer(i4)                                  :: kcm
  integer(i4)                                  :: kmm
  integer(i4)                                  :: m
  integer(i4)                                  :: maxlim
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nloop1
  integer(i4)                                  :: nloop2
  integer(i4)                                  :: nloopi
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nor
  integer(i4)                                  :: npot
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: npsi
  integer(i4)                                  :: npsj
  integer(i4)                                  :: npt
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbm
  logical                                      :: lbonded
  logical                                      :: lbreathe
  logical                                      :: lcspair
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  real(dp)                                     :: apt
  real(dp)                                     :: bpt
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
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
  real(dp)                                     :: ec6
  real(dp)                                     :: etrm
  real(dp)                                     :: etrm1
  real(dp)                                     :: etrm2
  real(dp)                                     :: factor
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiff
  real(dp)                                     :: rp
  real(dp)                                     :: rpd1
  real(dp)                                     :: rpd2
  real(dp)                                     :: rpd3
  real(dp)                                     :: rpd4
  real(dp)                                     :: rpd5
  real(dp)                                     :: rpd6
  real(dp)                                     :: rsgn
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: small
  real(dp)                                     :: small2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
  small = 1.0d-12
  small2 = 1.0d-2
  lbm = (imode.eq.3.or.imode.eq.4)
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2s = cuts*cuts
  if (lwolf) then
    cut2q = cutw*cutw   
  else
    cut2q = 1.0d12
  endif
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real11','npotl')
!
!  Zero energies
!
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
!
  if (imode.eq.1) then
    ione = 2
    nloop1 = nreg1
  elseif (imode.eq.2) then
    ione = 2
    nloop1 = nreg1old
  elseif (imode.eq.3) then
    ione = 1
    nloop1 = nreg1
  elseif (imode.eq.4) then
    ione = 1
    nloop1 = nreg1old
  endif
  if (lnoreal) goto 999
!
!  Outer loop over sites
!
  do i = ione,nloop1
    if (imode.eq.1.or.imode.eq.3) then
      xal = xdefe(i)
      yal = ydefe(i)
      zal = zdefe(i)
      nati = natdefe(i)
      ntypi = ntypdefe(i)
      qli = qdefe(i)
      oci = occdefe(i)
      if (nreldef(i).gt.0) then
        npsi = nreldef(i)
      else
        npsi = 0
      endif
      if (ldefbsmat(i)) then
        radi = radefe(i)
        indri = 3*nreg1 + i
      else
        radi = 0.0_dp
      endif
    else
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
        indri = 3*nreg1old + i
      else
        radi = 0.0_dp
      endif
    endif
    indi = 3*(i - 1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
!
!  Molecule handling
!
    if (lmol) then
      if (imode.eq.1.or.imode.eq.3) then
        nmi = ndefmol(i)
        indmi = ndefind(i)
      else
        nmi = ndefmolp(i)
        indmi = ndefindp(i)
      endif
    endif
    if (imode.eq.3.or.imode.eq.4) then
      nloop2 = nreg1old
    else
      nloop2 = i - 1
    endif
!
!  Inner loop over second site
!
    do j = 1,nloop2
      if (imode.eq.1) then
        natj = natdefe(j)
        ntypj = ntypdefe(j)
        qlj = qdefe(j)
        ocj = occdefe(j)
        xcrd = xdefe(j) - xal
        ycrd = ydefe(j) - yal
        zcrd = zdefe(j) - zal
        if (ldefbsmat(j)) then
          radj = radefe(j)
        else
          radj = 0.0_dp
        endif
      else
        natj = natp(j)
        ntypj = ntypep(j)
        qlj = qp(j)
        ocj = occp(j)
        npsj = npsite(j)
        xcrd = xperf(j) - xal
        ycrd = yperf(j) - yal
        zcrd = zperf(j) - zal
        iar = nsft + nrelat(npsj)
        if (lbsmat(iar)) then
          radj = radcfg(iar)
        else
          radj = 0.0_dp
        endif
      endif
      radsum = radi + radj
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
      if (imode.eq.3) then
        indj = 3*nloop1
        if (ldbsm) indj = indj + nloop1
        jx = indj + 1
        jy = indj + 2
        jz = indj + 3
      else
        indj = 3*(j - 1)
        jx = indj + 1
        jy = indj + 2
        jz = indj + 3
        indrj = 3*nloop1 + j
      endif
      ofct = oci*ocj
      factor = qli*qlj*ofct*angstoev
!
!  Possible core - shell flag
!
      lcspair = (abs(nat1 - nat2).eq.maxele.or.(oci + ocj).lt.1.0001_dp)
!
!  Molecule handling
!
      if (lmol) then
        if (imode.eq.1) then
          nmj = ndefmol(j)
          indmj = ndefind(j)
        else
          nmj = ndefmolp(j)
          indmj = ndefindp(j)
        endif
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      lneedmol  =  (lmol.and..not.lmolq)
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol  =  .true.
            if (nptype(n).eq.8.or.nptype(n).eq.33) then
              if (cuts.gt.rp) rp = cuts
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
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
      if (lmol) then
        lmolok = (nmi.eq.nmj.and.nmi.ne.0)
      else
        lmolok = .false.
      endif
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
!
      if (lmolok.and.r.gt.cut2s) then
        ind = indmj - indmi
        lptrmol = (ind.eq.0)
        if (.not.lptrmol) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          call mindtoijk(indmi,ixx,iyy,izz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lptrmol) then
          if (imode.eq.1) then
            lbonded = .false.
            l2bonds = .false.
            l3bonds = .false.
            icm = 1
            do while (icm.le.nbondsdef(i).and..not.lbonded)
              imm = nbondeddef(icm,i)
              lbonded = (imm.eq.j)
              jmm = 1
              jcm = 1
              do while (jcm.le.nbondsdef(j).and..not.l2bonds)
                jmm = nbondeddef(jcm,j)
                l2bonds = (imm.eq.jmm)
                if (.not.l2bonds.and..not.l3bonds) then
                  kmm = 1
                  kcm = 1
                  do while (kcm.le.nbondsdef(jmm).and..not.l3bonds)
                    kmm = nbondeddef(kcm,jmm)
                    l3bonds = (imm.eq.kmm)
                    kcm = kcm + 1
                  enddo
                endif
                jcm = jcm + 1
              enddo
              icm = icm + 1
            enddo
            if (lbonded) then
              l2bonds = .false.
              l3bonds = .false.
            endif
            if (l2bonds) then
              l3bonds = .false.
            endif
          else
            if (npsi.gt.0) then
              call mindtoijk(indmj,jxx,jyy,jzz)
              call mindtoijk(indmi,ixx,iyy,izz)
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
          endif
        else
          lbonded = .false.
          l2bonds = .false.
          l3bonds = .false.
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lptrmol = .false.
        lbonded = .false.
        l2bonds = .false.
        l3bonds = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
      if (abs(r - small2).lt.1.0d-12) r = small2
      if (r.lt.small) then
!
!  Core - shell spring constant at zero distant
!  correct second derivative matrix
!
        if (lgrad2) then
          do k = 1,npots
            npot = npotl(k)
            npt = nptype(npot)
            if (npt.eq.5.or.npt.eq.8.or.npt.eq.33) then
              apt = twopot(1,npot)*ofct
              derv2(ix,jx) = derv2(ix,jx) - apt
              derv2(iy,jy) = derv2(iy,jy) - apt
              derv2(iz,jz) = derv2(iz,jz) - apt
            endif
          enddo
        endif
!
!  Breathing shell self terms
!
        if (lbm.and.radi.gt.0.0_dp.and.nati.eq.natj) then
          do m = 1,npote
            if (nptype(m).eq.14) then
              if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                apt = twopot(1,m)*oci
                rdiff = radi - twopot(2,m)
                if (imode.eq.3) then
                  rsgn = - 1.0_dp
                else
                  rsgn = 1.0_dp
                endif
                eatom = eatom + 0.5_dp*apt*rdiff*rdiff
                if (lgrad1) then
                  raderv(i) = raderv(i) + rsgn*apt*rdiff
                  if (lgrad2) then
                    derv2(indri,indri) = derv2(indri,indri) + rsgn*apt
                  endif
                endif
              endif
            elseif (nptype(m).eq.17) then
              if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                apt = twopot(1,m)*oci
                rdiff = radi - twopot(3,m)
                if (imode.eq.3) then
                  rsgn = - 1.0_dp
                else
                  rsgn = 1.0_dp
                endif
                bpt = twopot(2,m)
                etrm1 = exp(bpt*rdiff)
                etrm2 = 1.0_dp/etrm1
                etrm = apt*(etrm1 + etrm2)
                eatom = eatom + etrm
                if (lgrad1) then
                  raderv(i) = raderv(i) + rsgn*bpt*(etrm1 - etrm2)
                  if (lgrad2) then
                    derv2(indri,indri) = derv2(indri,indri) + rsgn*bpt*bpt*etrm
                  endif
                endif
              endif
            elseif (nptype(m).eq.31) then
              if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
                apt = twopot(1,m)*oci
                rdiff = radi - twopot(3,m)
                if (imode.eq.3) then
                  rsgn = - 1.0_dp
                else
                  rsgn = 1.0_dp
                endif
                bpt = twopot(2,m)
                etrm1 = exp(bpt*rdiff)
                etrm = apt*etrm1
                eatom = eatom + etrm
                if (lgrad1) then
                  raderv(i) = raderv(i) + rsgn*bpt*etrm1
                  if (lgrad2) then
                    derv2(indri,indri) = derv2(indri,indri) + rsgn*bpt*bpt*etrm
                  endif
                endif
              endif
            endif
          enddo
        endif
        goto 1110
      elseif ((imode.ne.3.and.imode.ne.4).or.r.gt.small2) then
!
!  Store vector
!
        nor = 1
        dist = sqrt(r)
      else
        goto 1110
      endif
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
!
!  set core - shell to true to handle case where defect ion
!  is close to a perfect lattice site
!
      call twobody1(eatom,ereal,ec6,lgrad1,lgrad2,.false.,nor,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
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
!
!  For mode 3 the terms are to be subtracted  = > change sign
!
      if (imode.eq.3) then
        if (lgrad1) then
          deriv =  - deriv
          if (radsum.gt.0.0_dp) then
            rtrm1 =  - rtrm1
          endif
        endif
        if (lgrad2) then
          deriv2 =  - deriv2
          if (radsum.gt.0.0_dp) then
            rtrm2 =  - rtrm2
            rderiv =  - rderiv
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
            derv2(indri,indri) = derv2(indri,indri) + rtrm2
          endif
        endif
        if (radj.gt.0.0_dp.and..not.lbm) then
          raderv(j) = raderv(j) + rtrm1
          if (lgrad2) then
            derv2(indrj,indrj) = derv2(indrj,indrj) + rtrm2
            if (radi.gt.0.0_dp) then
              derv2(indri,indrj) = derv2(indri,indrj) + rtrm2
            endif
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
          derv2(indri,ix) = derv2(indri,ix) - rderiv*xcrd
          derv2(indri,iy) = derv2(indri,iy) - rderiv*ycrd
          derv2(indri,iz) = derv2(indri,iz) - rderiv*zcrd
        endif
      endif
      if (lgrad1.and..not.lbm) then
        xdrv(j) = xdrv(j) + deriv*xcrd
        ydrv(j) = ydrv(j) + deriv*ycrd
        zdrv(j) = zdrv(j) + deriv*zcrd
      endif
!
!  Second derivatives
!
      if (lgrad2) then
        derv2(ix,jx) = derv2(ix,jx) - deriv2*rpd1
        derv2(ix,jy) = derv2(ix,jy) - deriv2*rpd6
        derv2(ix,jz) = derv2(ix,jz) - deriv2*rpd5
        derv2(iy,jy) = derv2(iy,jy) - deriv2*rpd2
        derv2(iy,jz) = derv2(iy,jz) - deriv2*rpd4
        derv2(iz,jz) = derv2(iz,jz) - deriv2*rpd3
        derv2(ix,jx) = derv2(ix,jx) - deriv
        derv2(iy,jy) = derv2(iy,jy) - deriv
        derv2(iz,jz) = derv2(iz,jz) - deriv
!
!  Coordinate  -  radius mixed
!
        if (radj.gt.0.0_dp.and..not.lbm) then
          derv2(indrj,jx) = derv2(indrj,jx) + rderiv*xcrd
          derv2(indrj,jy) = derv2(indrj,jy) + rderiv*ycrd
          derv2(indrj,jz) = derv2(indrj,jz) + rderiv*zcrd
          derv2(indrj,ix) = derv2(indrj,ix) - rderiv*xcrd
          derv2(indrj,iy) = derv2(indrj,iy) - rderiv*ycrd
          derv2(indrj,iz) = derv2(indrj,iz) - rderiv*zcrd
        endif
        if (radi.gt.0.0_dp) then
          derv2(indri,jx) = derv2(indri,jx) + rderiv*xcrd
          derv2(indri,jy) = derv2(indri,jy) + rderiv*ycrd
          derv2(indri,jz) = derv2(indri,jz) + rderiv*zcrd
        endif
      endif
1110  continue
!
!  Symmetrise second derivative matrix
!
      if (lgrad2) then
        derv2(iy,jx) = derv2(ix,jy)
        derv2(iz,jx) = derv2(ix,jz)
        derv2(iz,jy) = derv2(iy,jz)
      endif
    enddo
  enddo
!
!  Breathing shell self terms
!
  if (.not.lbm) then
    if (imode.eq.1) then
      nloopi = nreg1
    else
      nloopi = nreg1old
    endif
    do i = 1,nloopi
      if (imode.eq.1) then
        lbreathe = ldefbsmat(i)
        nati = natdefe(i)
        ntypi = ntypdefe(i)
        oci = occdefe(i)
        radi = radefe(i)
      else
        iar = nsft + nrelat(npsite(i))
        lbreathe = lbsmat(iar)
        nati = natp(i)
        ntypi = ntypep(i)
        oci = occp(i)
        radi = radcfg(iar)
      endif
      if (lbreathe) then
        indri = 3*nloopi + i
!******************************
!  Breathing shell self term  *
!******************************
        do m = 1,npote
          if (nptype(m).eq.14) then
            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
              apt = twopot(1,m)*oci
              rdiff = radi - twopot(2,m)
              eatom = eatom + 0.5_dp*apt*rdiff*rdiff
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
              rdiff = radi - twopot(3,m)
              bpt = twopot(2,m)
              etrm1 = exp(bpt*rdiff)
              etrm2 = 1.0_dp/etrm1
              etrm = apt*(etrm1 + etrm2)
              eatom = eatom + etrm
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
              rdiff = radi - twopot(3,m)
              bpt = twopot(2,m)
              etrm1 = exp(bpt*rdiff)
              etrm = apt*etrm1
              eatom = eatom + etrm
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
    enddo
  endif
!
!  End of real space part  -  perform general tasks
!
999 continue
  if (lgrad2) then
!
!  Symmetrise second derivative matrix
!
    if (imode.eq.3) then
      indj = 3*nloop1
      if (ldbsm) indj = indj + nloop1
      jx = indj + 1
      jy = indj + 2
      jz = indj + 3
      do i = 1,indj
        derv2(jx,i) = derv2(i,jx)
        derv2(jy,i) = derv2(i,jy)
        derv2(jz,i) = derv2(i,jz)
      enddo
    else
      maxlim = 3*nloop1
      if (nbsm.gt.0.or.ldbsm) maxlim = maxlim + nloop1
      do i = 2,maxlim
        do j = 1,i - 1
          derv2(j,i) = derv2(i,j)
        enddo
      enddo
    endif
  endif
!
!  Add any C6 terms to ereal
!
  ereal = ereal + ec6
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real11','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
