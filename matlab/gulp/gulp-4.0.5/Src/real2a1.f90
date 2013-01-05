  subroutine real2a1(etot,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indmi,d1x,d1y,d1z, &
                     d1xe,d1ye,d1ze,dx,dy,dz,lgrad1,lgrad2,imode,imode2,neqi,npsi,lg1)
!
!  Calculate first and second derivatives acting on region 2a ion i
!  due to region 1.
!
!  imode = 1 => defective region 1 => add contributions
!  imode = 2 => perfect region 1 => subtract contributions
!  imode2= 0 => no derivatives for region 1 ion
!  imode2= 1 => add derivatives for region 1 ion
!  imode2= 2 => subtract derivatives for region 1 ion
!
!  i   = ion in region 2a
!  nati= atomic number of i
!  ntypi= type number of i
!  xcl = x coordinate of i
!  ycl = y coordinate of i
!  zcl = z coordinate of i
!  qli = charge of i
!  oci = site occupancy of i
!  rai = radius of i
!  nmi = molecule number of i
!  indmi= molecule cell index number of i
!  neqi = no. of equivalent i atoms
!  npsi = perfect lattice site of i
!
!  d1x,d1y,d1z = full gradients on i
!  d1xe,d1ye,d1ze = electrostatic only gradients on i
!
!  If the effect on region 1 is neglected then the region 2a - 1
!  contribution can be symmetrised.
!
!   3/95 Corrections added for periodic molecules
!   8/95 repcut modification added
!   1/99 1-4 interaction scaling added
!  11/02 Wildcard atoms added
!   4/05 Mods for cosh-spring added
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Uninitialised referencing of nmj corrected
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!  11/08 x/y/z components passed to twobody1
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
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
  use region2a
  use shell
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
  integer(i4)                                  :: imode2
  integer(i4)                                  :: indmi
  integer(i4)                                  :: nati
  integer(i4)                                  :: neqi
  integer(i4)                                  :: nmi
  integer(i4)                                  :: npsi
  integer(i4)                                  :: ntypi
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp)                                     :: d1x
  real(dp)                                     :: d1y
  real(dp)                                     :: d1z
  real(dp)                                     :: d1xe
  real(dp)                                     :: d1ye
  real(dp)                                     :: d1ze
  real(dp)                                     :: dx
  real(dp)                                     :: dy
  real(dp)                                     :: dz
  real(dp)                                     :: etot
  real(dp)                                     :: oci
  real(dp)                                     :: qli
  real(dp)                                     :: rai
  real(dp)                                     :: xcl
  real(dp)                                     :: ycl
  real(dp)                                     :: zcl
!
!  Local variables
!
  integer(i4)                                  :: iar
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: ixx2
  integer(i4)                                  :: iyy2
  integer(i4)                                  :: izz2
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: jxx2
  integer(i4)                                  :: jyy2
  integer(i4)                                  :: jzz2
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nloop1
  integer(i4)                                  :: nmj
  integer(i4)                                  :: npsj
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lcspair
  logical                                      :: ldogr
  logical                                      :: lg1
  logical                                      :: lmatch
  logical                                      :: lorder12
  logical                                      :: lptrmol
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d22(3,3)
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
  real(dp)                                     :: dsum
  real(dp)                                     :: dtrm
  real(dp)                                     :: dtrm2
  real(dp)                                     :: dxcrd
  real(dp)                                     :: dycrd
  real(dp)                                     :: dzcrd
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: factor
  real(dp)                                     :: gfct
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: radsum
  real(dp)                                     :: raj
  real(dp)                                     :: rderiv
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
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: t3
  real(dp)                                     :: t4
  real(dp)                                     :: t5
  real(dp)                                     :: t6
  real(dp)                                     :: t7
  real(dp)                                     :: t8
  real(dp)                                     :: t9
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
!  Local variables
!
  if (lgrad2) then
    kx = 3*nreg1 + 1
    if (ldbsm) kx = kx + nreg1
    ky = kx + 1
    kz = ky + 1
  endif
!
!  Zero energies
!
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
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
  if (status/=0) call outofmemory('real2a1','npotl')
!
!  Set loop length
!
  if (imode.eq.1) then
    nloop1 = nreg1
  elseif (imode.eq.2) then
    nloop1 = nreg1old
  endif
!
  do j = 1,nloop1
    ldogr = (mode2a.eq.1)
    gfct = 1.0_dp
    if (imode.eq.1) then
      xal = xdefe(j)
      yal = ydefe(j)
      zal = zdefe(j)
      natj = natdefe(j)
      ntypj = ntypdefe(j)
      qlj = qdefe(j)
      ocj = occdefe(j)
      if (nreldef(j).gt.0) then
        npsj = npsite(nreldef(j))
      else
        npsj = 0
      endif
      if (ldefbsmat(j)) then
        raj = radefe(j)
      else
        raj = 0.0_dp
      endif
      if (ld1sym.and.(.not.lgrad2.or.ld2sym)) then
        if (ndrelop(j).eq.1) then
          jj = ndrel(j)
          gfct = ndeqv(jj)
        else
          ldogr = .false.
        endif
      else
        jj = j
      endif
    else
      jj = j
      xal = xperf(j)
      yal = yperf(j)
      zal = zperf(j)
      natj = natp(j)
      ntypj = ntypep(j)
      qlj = qp(j)
      ocj = occp(j)
      npsj = npsite(j)
      iar = nrelat(npsj) + nsft
      if (lbsmat(iar)) then
        raj = radcfg(iar)
      else
        raj = 0.0_dp
      endif
    endif
    xcrd = xal - xcl
    ycrd = yal - ycl
    zcrd = zal - zcl
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
    ofct = oci*ocj
    factor = qli*qlj*ofct*angstoev
    radsum = rai + raj
!
!  Possible core - shell flag
!
    lcspair = (abs(nat1 - nat2).eq.maxele.or.(oci + ocj).lt.1.0001_dp)
!
!  Molecule handling
!
    lbonded = .false.
    l2bonds = .false.
    l3bonds = .false.
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
    do n = 1,npote
      if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
        if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
          npots = npots + 1
          npotl(npots) = n
          if (nptype(n).eq.8.or.nptype(n).eq.33) then
            if (cuts.gt.rp) rp = cuts
          else
            if (rpot(n).gt.rp) rp = rpot(n)
          endif
        endif
      endif
    enddo
    cut2r = rp*rp
    if (cut2r.gt.cut2p) cut2r = cut2p
    r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
    if (lmol.and.r.gt.cut2s) then
      if (nmi.ne.0.and.nmi.eq.nmj) then
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
          if (npsj.gt.0) then
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
    else
      lptrmol   = .false.
      lbonded   = .false.
      l2bonds   = .false.
      l3bonds   = .false.
      nbtypeij  = 0
      nbtypeij2 = 0
    endif
    dist = sqrt(r)
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
!
!  Note gradient flags are shifted as one higher order is needed
!  due to E3 term
!
    if (ldogr) then
      call twobody1(eatom,ereal,ec6,.true.,lgrad1,lgrad2,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
    else
      call twobody1(eatom,ereal,ec6,lg1,.false.,.false.,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
    endif
    if (lg1) then
!
!  For mode 2 the terms are to be subtracted  = > change sign
!
      if (imode.eq.2) then
        deriv = - deriv
        if (lgrad1.and.ldogr) deriv2 = - deriv2
        if (lgrad2.and.ldogr) deriv3 = - deriv3
        derive0 = - derive0
        derive = - derive
        if (ldbsm) then
          rderiv = - rderiv
          rtrm1 = - rtrm1
          rtrm2 = - rtrm2
        endif
      endif
!
!  First derivatives
!
      dxcrd = deriv*xcrd
      dycrd = deriv*ycrd
      dzcrd = deriv*zcrd
      d1x = d1x - dxcrd
      d1y = d1y - dycrd
      d1z = d1z - dzcrd
      derive = derive*qli*qlj
      d1xe = d1xe - derive*xcrd
      d1ye = d1ye - derive*ycrd
      d1ze = d1ze - derive*zcrd
    endif
    if (imode.eq.1.and.imode2.gt.0.and.lgrad1.and.ldogr) then
!
!  Correct derivatives for region 1 ion
!
      if (imode2.eq.1) then
        xdrv(jj) = xdrv(jj) + dxcrd*gfct
        ydrv(jj) = ydrv(jj) + dycrd*gfct
        zdrv(jj) = zdrv(jj) + dzcrd*gfct
        if (raj.gt.0.0_dp) then
          raderv(jj) = raderv(jj) + rtrm1*gfct
        endif
      else
        xdrv(jj) = xdrv(jj) - dxcrd*gfct
        ydrv(jj) = ydrv(jj) - dycrd*gfct
        zdrv(jj) = zdrv(jj) - dzcrd*gfct
        if (raj.gt.0.0_dp) then
          raderv(jj) = raderv(jj) - rtrm1*gfct
        endif
      endif
    endif
!
    if (imode2.eq.1.and.lgrad1.and.ldogr) then
      rpd1 = xcrd*xcrd
      rpd2 = ycrd*ycrd
      rpd3 = zcrd*zcrd
      rpd4 = ycrd*zcrd
      rpd5 = xcrd*zcrd
      rpd6 = xcrd*ycrd
!
!  Second derivatives  -  signs change from normal routine as this
!  saves summing off diagonal elements to create on diagonal.
!
      d22(1,1) = deriv2*rpd1 + deriv
      d22(1,2) = deriv2*rpd6
      d22(1,3) = deriv2*rpd5
      d22(2,2) = deriv2*rpd2 + deriv
      d22(2,3) = deriv2*rpd4
      d22(3,3) = deriv2*rpd3 + deriv
!
!  Correct derivatives for E3 gradient
!
      d22(2,1) = d22(1,2)
      d22(3,1) = d22(1,3)
      d22(3,2) = d22(2,3)
      xdrv(jj) = xdrv(jj) + 0.5_dp*(dx*d22(1,1) + dy*d22(2,1) + dz*d22(3,1))*gfct
      ydrv(jj) = ydrv(jj) + 0.5_dp*(dx*d22(1,2) + dy*d22(2,2) + dz*d22(3,2))*gfct
      zdrv(jj) = zdrv(jj) + 0.5_dp*(dx*d22(1,3) + dy*d22(2,3) + dz*d22(3,3))*gfct
      if (lgrad2) then
!
!  Correct second derivatives for E3  -  involves the third derivatives
!
        jx = 3*(j - 1) + 1
        jy = jx + 1
        jz = jy + 1
!
        dsum = (dx*xcrd + dy*ycrd + dz*zcrd)
        dtrm = 0.5_dp*deriv3*dsum*gfct
        dtrm2 = 0.5_dp*deriv2*gfct
        rpd1 = rpd1*dtrm
        rpd2 = rpd2*dtrm
        rpd3 = rpd3*dtrm
        rpd4 = rpd4*dtrm
        rpd5 = rpd5*dtrm
        rpd6 = rpd6*dtrm
        t1 = rpd1 + dtrm2*(2.0_dp*xcrd*dx + dsum)
        t2 = rpd6 + dtrm2*(dx*ycrd + dy*xcrd)
        t3 = rpd5 + dtrm2*(dx*zcrd + dz*xcrd)
        t4 = rpd6 + dtrm2*(dx*ycrd + dy*xcrd)
        t5 = rpd2 + dtrm2*(2.0_dp*ycrd*dy + dsum)
        t6 = rpd4 + dtrm2*(dz*ycrd + dy*zcrd)
        t7 = rpd5 + dtrm2*(dx*zcrd + dz*xcrd)
        t8 = rpd4 + dtrm2*(dz*ycrd + dy*zcrd)
        t9 = rpd3 + dtrm2*(2.0_dp*zcrd*dz + dsum)
!
        derv2(jx,kx) = derv2(jx,kx) + t1
        derv2(jy,kx) = derv2(jy,kx) + t2
        derv2(jz,kx) = derv2(jz,kx) + t3
        derv2(jx,ky) = derv2(jx,ky) + t4
        derv2(jy,ky) = derv2(jy,ky) + t5
        derv2(jz,ky) = derv2(jz,ky) + t6
        derv2(jx,kz) = derv2(jx,kz) + t7
        derv2(jy,kz) = derv2(jy,kz) + t8
        derv2(jz,kz) = derv2(jz,kz) + t9
!
        derv2(kx,jx) = derv2(kx,jx) + t1
        derv2(ky,jx) = derv2(ky,jx) + t2
        derv2(kz,jx) = derv2(kz,jx) + t3
        derv2(kx,jy) = derv2(kx,jy) + t4
        derv2(ky,jy) = derv2(ky,jy) + t5
        derv2(kz,jy) = derv2(kz,jy) + t6
        derv2(kx,jz) = derv2(kx,jz) + t7
        derv2(ky,jz) = derv2(ky,jz) + t8
        derv2(kz,jz) = derv2(kz,jz) + t9
      endif
    endif
  enddo
  etot = etot + neqi*(eatom + ereal)
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real2a1','npotl')
!
  return
  end
