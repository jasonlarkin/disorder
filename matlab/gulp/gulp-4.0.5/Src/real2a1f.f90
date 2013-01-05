  subroutine real2a1f(e12ap,e12ad,lgrad1,lgrad2)
!
!  Calculate first and second derivatives acting on region 1 ion i
!  due to displaced region 2a ion.
!
!  imode = 1 => defective region 1 => add contributions
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
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp)                                     :: e12ad
  real(dp)                                     :: e12ap
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
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
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj  
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nloop
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj   
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
  real(dp)                                     :: derivp
  real(dp)                                     :: deriv2p
  real(dp)                                     :: deriv3p
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: dsum
  real(dp)                                     :: dtrm
  real(dp)                                     :: dtrm2
  real(dp)                                     :: dx
  real(dp)                                     :: dy
  real(dp)                                     :: dz
  real(dp)                                     :: dxcrd
  real(dp)                                     :: dycrd
  real(dp)                                     :: dzcrd
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: factor
  real(dp)                                     :: gfct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj  
  real(dp)                                     :: r    
  real(dp)                                     :: radsum
  real(dp)                                     :: rai
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
  real(dp)                                     :: rtrm1p
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm2p
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm3p
  real(dp)                                     :: rtrm32
  real(dp)                                     :: rtrm32p
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
  real(dp)                                     :: xcl   
  real(dp)                                     :: ycl     
  real(dp)                                     :: zcl   
  real(dp)                                     :: xcrd 
  real(dp)                                     :: ycrd  
  real(dp)                                     :: zcrd
  real(dp)                                     :: xcrdp
  real(dp)                                     :: ycrdp 
  real(dp)                                     :: zcrdp
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
!  Zero energy
!
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
  if (status/=0) call outofmemory('real2a1f','npotl')
!
!  Set loop length
!
  if (ld1sym.and.(.not.lgrad2.or.ld2sym)) then
    nloop = ndasym
  else
    nloop = nreg1
  endif
!
  do i = 1,nloop
    if (ld1sym.and.(.not.lgrad2.or.ld2sym)) then
      ii = ndsptr(i)
      gfct = ndeqv(i)
    else
      ii = i
      gfct = 1.0_dp
    endif
    if (lgrad2) then
      ix = 3*(i-1) + 1
      iy = ix + 1
      iz = ix + 2
    endif
    xcl = xdefe(ii)
    ycl = ydefe(ii)
    zcl = zdefe(ii)
    nati = natdefe(ii)
    ntypi = ntypdefe(ii)
    qli = qdefe(ii)
    oci = occdefe(ii)
    if (nreldef(ii).gt.0) then
      npsi = npsite(nreldef(ii))
    else
      npsi = 0
    endif
    if (ldefbsmat(ii)) then
      rai = radefe(ii)
    else
      rai = 0.0_dp
    endif
!
!  Molecule handling
!
    if (lmol) then
      nmi = ndefmol(ii)
      indmi = ndefind(ii)
    endif
    do j = 1,nreg2
      natj = nr2a(j)
      ntypj = ntr2a(j)
      xal = xr2a(j)
      yal = yr2a(j)
      zal = zr2a(j)
      qlj = qr2a(j)
      ocj = or2a(j)
      npsj = nps(j)
      if (ldbr2a(j)) then
        raj = rr2a(j)
      else
        raj = 0.0_dp
      endif
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
!  Possible core-shell flag
!
      lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
      lbonded = .false.
      l2bonds = .false.
      l3bonds = .false.
      if (lmol) then
        nmj = nmr2a(j)
        indmj = nmir2a(j)
      endif
      xcrdp = xcl - xal
      ycrdp = ycl - yal
      zcrdp = zcl - zal
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
!
!  Molecule and bonding checks
!
      r = xcrdp*xcrdp + ycrdp*ycrdp + zcrdp*zcrdp
      if (lmol.and.nmi.ne.0.and.nmi.eq.nmj.and.r.gt.cut2s) then
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
!*****************************************************************
!  Calculate twobody contributions - perfect and then defective  *
!*****************************************************************
!
!  Perfect 2a
!
      eatom = 0.0_dp
      ereal = 0.0_dp
      ec6   = 0.0_dp
      dist  = sqrt(r)
      call twobody1(eatom,ereal,ec6,.true.,lgrad1,lgrad2,1_i4,1_i4,r,xcrdp,ycrdp,zcrdp,d0i,d0j, &
                    derivp,deriv2p,deriv3p,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1p,rtrm2p,rtrm3p,rtrm32p, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
      e12ap = e12ap + (eatom + ereal)*gfct
!
!  Defective 2a
!
      dx = xdis(j)
      dy = ydis(j)
      dz = zdis(j)
      xcrd = xcrdp - dx
      ycrd = ycrdp - dy
      zcrd = zcrdp - dz
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      eatom = 0.0_dp
      ereal = 0.0_dp
      ec6   = 0.0_dp
      dist  = sqrt(r)
      call twobody1(eatom,ereal,ec6,.true.,lgrad1,lgrad2,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
      e12ad = e12ad + (eatom + ereal)*gfct
!
!  For perfect sites the terms are to be subtracted
!
      if (ldbsm) then
        rtrm1 = rtrm1 - rtrm1p
        rtrm2 = rtrm2 - rtrm2p
      endif
!
!  First derivatives
!
      if (lgrad1) then
        deriv = deriv*gfct
        derivp = derivp*gfct
        deriv2 = deriv2*gfct
        dxcrd = deriv*xcrd - derivp*xcrdp
        dycrd = deriv*ycrd - derivp*ycrdp
        dzcrd = deriv*zcrd - derivp*zcrdp
!
!  Correct derivatives for region 1 ion
!
        xdrv(i) = xdrv(i) + dxcrd
        ydrv(i) = ydrv(i) + dycrd
        zdrv(i) = zdrv(i) + dzcrd
        if (rai.gt.0.0_dp) then
          raderv(i) = raderv(i) + rtrm1*gfct
        endif
        rpd1 = xcrd*xcrd
        rpd2 = ycrd*ycrd
        rpd3 = zcrd*zcrd
        rpd4 = ycrd*zcrd
        rpd5 = xcrd*zcrd
        rpd6 = xcrd*ycrd
!
!  Second derivatives - signs change from normal routine as this
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
        xdrv(i) = xdrv(i) + 0.5_dp*(dx*d22(1,1) + dy*d22(2,1) + dz*d22(3,1))
        ydrv(i) = ydrv(i) + 0.5_dp*(dx*d22(1,2) + dy*d22(2,2) + dz*d22(3,2))
        zdrv(i) = zdrv(i) + 0.5_dp*(dx*d22(1,3) + dy*d22(2,3) + dz*d22(3,3))
        if (lgrad2) then
!
!  Correct second derivatives for E3 - involves the third derivatives
!
          dsum = (dx*xcrd + dy*ycrd + dz*zcrd)
          dtrm = 0.5_dp*deriv3*dsum*gfct
          dtrm2 = 0.5_dp*deriv2
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
          if (.not.ld2sym) then
            derv2(ix,kx) = derv2(ix,kx) + t1
            derv2(iy,kx) = derv2(iy,kx) + t2
            derv2(iz,kx) = derv2(iz,kx) + t3
            derv2(ix,ky) = derv2(ix,ky) + t4
            derv2(iy,ky) = derv2(iy,ky) + t5
            derv2(iz,ky) = derv2(iz,ky) + t6
            derv2(ix,kz) = derv2(ix,kz) + t7
            derv2(iy,kz) = derv2(iy,kz) + t8
            derv2(iz,kz) = derv2(iz,kz) + t9
          endif
          derv2(kx,ix) = derv2(kx,ix) + t1
          derv2(ky,ix) = derv2(ky,ix) + t2
          derv2(kz,ix) = derv2(kz,ix) + t3
          derv2(kx,iy) = derv2(kx,iy) + t4
          derv2(ky,iy) = derv2(ky,iy) + t5
          derv2(kz,iy) = derv2(kz,iy) + t6
          derv2(kx,iz) = derv2(kx,iz) + t7
          derv2(ky,iz) = derv2(ky,iz) + t8
          derv2(kz,iz) = derv2(kz,iz) + t9
        endif
      endif
    enddo
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real2a1f','npotl')
!
  return
  end
