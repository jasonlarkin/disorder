  subroutine fouroop12(efor,lgrad1,lgrad2,imode,xderv,yderv,zderv)
!
!  Subroutine for four-body out of plane energy of defects
!
!  imode = 1 => defective regions 1 and 2a
!  imode = 2 => perfect regions 1 and 2a
!
!  Strategy - sift by potential first, then cutoffs
!
!  Now symmetry adapted
!
!   4/97 Created from four12/fouroop
!   8/98 Potential parts placed in separate subroutine
!   8/98 Second derivatives re-written for clarity
!   7/00 First derivative arrays now passed as arguments
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   9/01 lmolq calculations accelerated using lneedmol 
!   7/02 K4 added
!  11/02 Wildcard atom types added
!  10/05 Modified to handle inversion form of out of plane potential
!   6/06 Inversion squared potential added
!   1/07 Wildcard handling in lmatch calls corrected
!   2/07 Bonding types added
!  10/07 Angle-angle cross potential added
!  12/07 Unused variables removed
!   4/08 Minimum cutoff added for out of plane potentials
!   5/08 UFFoop potential added
!   5/08 only3 check added as an option
!   5/08 Defect bonding array structure changed
!   7/08 New type checking algorithm introduced
!   7/08 Handling of xangle potential added w.r.t. to type swapping
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!   6/09 noffset introduced as per four12
!  10/12 Redundant setting of lmax removed
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
!  Julian Gale, NRI, Curtin University, October 2012
!
  use control
  use current
  use defects
  use derivatives
  use four
  use molecule
  use region2a
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: imode
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp),    intent(inout)                   :: efor
  real(dp),    intent(inout)                   :: xderv(*)
  real(dp),    intent(inout)                   :: yderv(*)
  real(dp),    intent(inout)                   :: zderv(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: icm
  integer(i4)                                  :: ii
  integer(i4)                                  :: imm
  integer(i4)                                  :: ind
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
  integer(i4)                                  :: indmmk
  integer(i4)                                  :: indmml
  integer(i4)                                  :: ir
  integer(i4)                                  :: isgn
  integer(i4)                                  :: ixx
  integer(i4)                                  :: ixx2
  integer(i4)                                  :: iyy
  integer(i4)                                  :: iyy2
  integer(i4)                                  :: izz
  integer(i4)                                  :: izz2
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jr
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jxx2
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jyy2
  integer(i4)                                  :: jzz
  integer(i4)                                  :: jzz2
  integer(i4)                                  :: k
  integer(i4)                                  :: kb(6,6)
  integer(i4)                                  :: ki
  integer(i4)                                  :: kj
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kr
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kxx2
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kyy2
  integer(i4)                                  :: kzz
  integer(i4)                                  :: kzz2
  integer(i4)                                  :: l
  integer(i4)                                  :: ll
  integer(i4)                                  :: lr
  integer(i4)                                  :: lxx
  integer(i4)                                  :: lxx2
  integer(i4)                                  :: lyy
  integer(i4)                                  :: lyy2
  integer(i4)                                  :: lzz
  integer(i4)                                  :: lzz2
  integer(i4)                                  :: n
  integer(i4)                                  :: n1x
  integer(i4)                                  :: n1xr
  integer(i4)                                  :: n2x
  integer(i4)                                  :: n2xr
  integer(i4)                                  :: n3x
  integer(i4)                                  :: n3xr
  integer(i4)                                  :: n4x
  integer(i4)                                  :: n4xr
  integer(i4)                                  :: n11
  integer(i4)                                  :: n21
  integer(i4)                                  :: n31
  integer(i4)                                  :: n41
  integer(i4)                                  :: n22
  integer(i4)                                  :: n32
  integer(i4)                                  :: n42
  integer(i4)                                  :: n11r
  integer(i4)                                  :: n21r
  integer(i4)                                  :: n31r
  integer(i4)                                  :: n41r
  integer(i4)                                  :: n22r
  integer(i4)                                  :: n32r
  integer(i4)                                  :: n42r
  integer(i4)                                  :: n3vec(3,4)
  integer(i4)                                  :: nbondi
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: neqi
  integer(i4)                                  :: neqj
  integer(i4)                                  :: neqk
  integer(i4)                                  :: neql
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: noffset
  integer(i4)                                  :: npsi
  integer(i4)                                  :: npsj
  integer(i4)                                  :: npsk
  integer(i4)                                  :: npsl
  integer(i4)                                  :: nr1
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntp2
  integer(i4)                                  :: ntp3
  integer(i4)                                  :: ntp4
  integer(i4)                                  :: ntot
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: ldsl
  logical                                      :: lin1
  logical                                      :: lin2
  logical                                      :: lin3
  logical                                      :: lin4
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: lmatch
  logical                                      :: lmatch2
  logical                                      :: lmatch3
  logical                                      :: lmatchanyof2
  logical                                      :: lmatchanyof3
  logical                                      :: lmolok
  logical                                      :: lneedmol 
  logical                                      :: lnodisp
  logical                                      :: lregion1i
  logical                                      :: lregion1j
  logical                                      :: lregion1k
  logical                                      :: lregion1l
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(56)
  real(dp)                                     :: eterm
  real(dp)                                     :: fpoly(5)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ofct 
  real(dp)                                     :: phi0 
  real(dp)                                     :: r21
  real(dp)                                     :: r212
  real(dp)                                     :: r31
  real(dp)                                     :: r312
  real(dp)                                     :: r32
  real(dp)                                     :: r322
  real(dp)                                     :: r41
  real(dp)                                     :: r412
  real(dp)                                     :: r42
  real(dp)                                     :: r422
  real(dp)                                     :: r43
  real(dp)                                     :: r432
  real(dp)                                     :: rkfor
  real(dp)                                     :: rkfor4
  real(dp)                                     :: rko
  real(dp)                                     :: rn
  real(dp)                                     :: t12
  real(dp)                                     :: t13
  real(dp)                                     :: t14
  real(dp)                                     :: t23
  real(dp)                                     :: t24
  real(dp)                                     :: t34
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr1min
  real(dp)                                     :: tr2min
  real(dp)                                     :: tr3min
  real(dp)                                     :: vec(3,3,4)
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x42
  real(dp)                                     :: y42
  real(dp)                                     :: z42
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
  real(dp)                                     :: xc1
  real(dp)                                     :: yc1
  real(dp)                                     :: zc1
  real(dp)                                     :: xc2
  real(dp)                                     :: yc2
  real(dp)                                     :: zc2
  real(dp)                                     :: xc3
  real(dp)                                     :: yc3
  real(dp)                                     :: zc3
  real(dp)                                     :: xc4
  real(dp)                                     :: yc4
  real(dp)                                     :: zc4
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
!
  ldsl = (ld1sym.and.(.not.lgrad2.or.ld2sym).and.imode.eq.1)
  lnodisp = (index(keyword,'r234').eq.0)
  if (.not.ldsl) then
    lin1 = .false.
    lin2 = .false.
    lin3 = .false.
    lin4 = .false.
  endif
!
!  Initialisation
!
  if (imode.eq.1) then
    nr1 = nreg1
  else
    nr1 = nreg1old
  endif
  ntot = nr1 + ntreg2
  noffset = 3*nr1
  if (ldbsm) noffset = noffset + nr1
!*************************
!  Loop over potentials  *
!*************************
  pots: do n = 1,nfor
    nfortype = nforty(n)
    if (.not.loutofplane(n)) cycle pots
    ntp2 = 2
    ntp3 = 3
    ntp4 = 4
    nt1 = nfspec1(n)
    nt2 = nfspec2(n)
    nt3 = nfspec3(n)
    nt4 = nfspec4(n)
    ntyp1 = nfptyp1(n)
    ntyp2 = nfptyp2(n)
    ntyp3 = nfptyp3(n)
    ntyp4 = nfptyp4(n)
    tr1 = for1(n)**2
    tr2 = for2(n)**2
    tr3 = for3(n)**2
    tr1min = for1min(n)**2
    tr2min = for2min(n)**2
    tr3min = for3min(n)**2
    lbtyp = (mmfexc(n).eq.1)
    lintra_only = (lfintra(n).and..not.lfinter(n))
    linter_only = (lfinter(n).and..not.lfintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
    rkfor = fork(n)
    rkfor4 = forpoly(1,n)
!********************************
!  Loop over middle site 1 / i  *
!********************************
    iloop: do i = 1,ntot
      lregion1i = (i.le.nr1)
      if (lregion1i) then
        if (imode.eq.1) then
          ni = natdefe(i)
          ntypi = ntypdefe(i)
          xc1 = xdefe(i)
          yc1 = ydefe(i)
          zc1 = zdefe(i)
          oci = occdefe(i)
          if (lmol.and.lneedmol) then
            nmi = ndefmol(i)
            indm = ndefind(i)
          endif
          if (nreldef(i).gt.0) then
            npsi = npsite(nreldef(i))
          else
            npsi = 0
          endif
          nbondi = nbondsdef(i)
        else
          ni = natp(i)
          ntypi = ntypep(i)
          xc1 = xperf(i)
          yc1 = yperf(i)
          zc1 = zperf(i)
          oci = occp(i)
          if (lmol.and.lneedmol) then
            nmi = ndefmolp(i)
            indm = ndefindp(i)
          endif
          npsi = npsite(i)
          nbondi = nbonds(npsi)
        endif
        if (ldsl) then
          lin1 = (ndrelop(i).eq.1)
          ir = ndrel(i)
          neqi = ndeqv(ir)
        endif
      else
        lin1 = .false.
        ii = i - nr1
        ni = nr2a(ii)
        ntypi = ntr2a(ii)
        if (lnodisp) then
          xc1 = xr2a(ii)
          yc1 = yr2a(ii)
          zc1 = zr2a(ii)
        else
          xc1 = xr2a(ii) + xdis(ii)
          yc1 = yr2a(ii) + ydis(ii)
          zc1 = zr2a(ii) + zdis(ii)
        endif
        oci = or2a(ii)
        if (lmol.and.lneedmol) then
          nmi = nmr2a(ii)
          indm = nmir2a(ii)
        endif
        npsi = nps(ii)
        nbondi = nbonds(npsi)
      endif
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  Only 3 bonds check
!
      if (lbtyp.and.lonly3oop(n).and.nbondi.ne.3) cycle iloop
!
!  i has been accepted
!
!***********************************
!  Loop over first end site 2 / j  *
!***********************************
      jloop: do j = 1,ntot
        lregion1j = (j.le.nr1)
        if (lregion1j) then
          if (imode.eq.1) then
            nj = natdefe(j)
            ntypj = ntypdefe(j)
            xc2 = xdefe(j)
            yc2 = ydefe(j)
            zc2 = zdefe(j)
            ocj = occdefe(j)
            if (lmol.and.lneedmol) then
              nmj = ndefmol(j)
              indmj = ndefind(j)
            endif
            if (nreldef(j).gt.0) then
              npsj = npsite(nreldef(j))
            else
              npsj = 0
            endif
          else
            nj = natp(j)
            ntypj = ntypep(j)
            xc2 = xperf(j)
            yc2 = yperf(j)
            zc2 = zperf(j)
            ocj = occp(j)
            if (lmol.and.lneedmol) then
              nmj = ndefmolp(j)
              indmj = ndefindp(j)
            endif
            npsj = npsite(j)
          endif
          if (ldsl) then
            lin2 = (ndrelop(j).eq.1)
            jr = ndrel(j)
            neqj = ndeqv(jr)
          endif
        else
          lin2 = .false.
          jj = j - nr1
          nj = nr2a(jj)
          ntypj = ntr2a(jj)
          if (lnodisp) then
            xc2 = xr2a(jj)
            yc2 = yr2a(jj)
            zc2 = zr2a(jj)
          else
            xc2 = xr2a(jj) + xdis(jj)
            yc2 = yr2a(jj) + ydis(jj)
            zc2 = zr2a(jj) + zdis(jj)
          endif
          ocj = or2a(jj)
          if (lmol.and.lneedmol) then
            nmj = nmr2a(jj)
            indmj = nmir2a(jj)
          endif
          npsj = nps(jj)
        endif
!
!  Check j is allowed for n
!
        lmatch3 = lmatchanyof3(nj,ntypj,ntp2,nt2,ntyp2,tr1,tr1min,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
        if (.not.lmatch3) cycle jloop
!
!  Prevent atoms i and j being the same atom
!
        if (j.eq.i) cycle jloop
!
        lbonded = .false.
        if (lmol.and.lneedmol) then
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
          if (lmolok) then
            ind = indmj - indm
            lmolok = (ind.eq.0)
            if (.not.lmolok) then
              call mindtoijk(indmj,jxx,jyy,jzz)
              call mindtoijk(indm,ixx,iyy,izz)
              jxx = jxx - ixx
              jyy = jyy - iyy
              jzz = jzz - izz
              call samemol(lmolok,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
            endif
          endif
          if (lmolok.and.linter_only) cycle jloop
          if (.not.lmolok.and.lintra_only) cycle jloop
          if (lmolok.and.lbtyp) then
            if (imode.eq.1) then
              if (lregion1i.and.lregion1j) then
                lbonded = .false.
                icm = 1
                do while (icm.le.nbondsdef(i).and..not.lbonded)
                  imm = nbondeddef(icm,i)
                  lbonded = (imm.eq.j)
                  icm = icm + 1
                enddo
              else
                if (npsi.gt.0.and.npsj.gt.0) then
                  call mindtoijk(indmj,jxx,jyy,jzz)
                  call mindtoijk(indm,ixx,iyy,izz)
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
                  call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
                else
                  lbonded = .false.
                endif
              endif
            else
              if (npsi.gt.0.and.npsj.gt.0) then
                call mindtoijk(indmj,jxx,jyy,jzz)
                call mindtoijk(indm,ixx,iyy,izz)
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
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
              else
                lbonded = .false.
              endif
            endif
            if (.not.lbonded) cycle jloop
          endif
        else
          lmolok = .false.
        endif
        if (lbtyp.and..not.lmolok) cycle jloop
        x21 = xc2 - xc1
        y21 = yc2 - yc1
        z21 = zc2 - zc1
!
!  Check r21 is OK
!
        r212 = x21*x21 + y21*y21 + z21*z21
        if (r212.lt.1d-10) cycle jloop
        if ((r212.gt.tr1.or.r212.lt.tr1min).and.(.not.lbtyp.or..not.lbonded)) cycle jloop
        r21 = sqrt(r212)
!************************************
!  Loop over second end site 3 / k  *
!************************************
        kloop: do k = 1,j-1
          lregion1k = (k.le.nr1)
          if (lregion1k) then
            if (imode.eq.1) then
              nk = natdefe(k)
              ntypk = ntypdefe(k)
              xc3 = xdefe(k)
              yc3 = ydefe(k)
              zc3 = zdefe(k)
              ock = occdefe(k)
              if (lmol.and.lneedmol) then
                nmk = ndefmol(k)
                indmk = ndefind(k)
              endif
              if (nreldef(k).gt.0) then
                npsk = npsite(nreldef(k))
              else
                npsk = 0
              endif
            else
              nk = natp(k)
              ntypk = ntypep(k)
              xc3 = xperf(k)
              yc3 = yperf(k)
              zc3 = zperf(k)
              ock = occp(k)
              if (lmol.and.lneedmol) then
                nmk = ndefmolp(k)
                indmk = ndefindp(k)
              endif
              npsk = npsite(k)
            endif
            if (ldsl) then
              lin3 = (ndrelop(k).eq.1)
              kr = ndrel(k)
              neqk = ndeqv(kr)
            endif
          else
            lin3 = .false.
            kk = k - nr1
            nk = nr2a(kk)
            ntypk = ntr2a(kk)
            if (lnodisp) then
              xc3 = xr2a(kk)
              yc3 = yr2a(kk)
              zc3 = zr2a(kk)
            else
              xc3 = xr2a(kk) + xdis(kk)
              yc3 = yr2a(kk) + ydis(kk)
              zc3 = zr2a(kk) + zdis(kk)
            endif
            ock = or2a(kk)
            if (lmol.and.lneedmol) then
              nmk = nmr2a(kk)
              indmk = nmir2a(kk)
            endif
            npsk = nps(kk)
          endif
!
!  Check k is allowed for n
!
          lmatch2 = lmatchanyof2(nk,ntypk,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
          if (.not.lmatch2) cycle kloop
!
!  Prevent atoms i and k being the same atom
!
          if (k.eq.i) cycle kloop
!
!  Molecularity check
!
          lbonded = .false.
          if (lmol.and.lneedmol) then
            lmolok = (nmi.eq.nmk.and.nmi.gt.0)
            if (lmolok) then
              ind = indmk - indm
              lmolok = (ind.eq.0)
              if (.not.lmolok) then
                call mindtoijk(indmk,kxx,kyy,kzz)
                call mindtoijk(indm,ixx,iyy,izz)
                kxx = kxx - ixx
                kyy = kyy - iyy
                kzz = kzz - izz
                call samemol(lmolok,nmi,kxx,kyy,kzz,0_i4,0_i4,0_i4)
              endif
            endif
            if (lmolok.and.linter_only) cycle kloop
            if (.not.lmolok.and.lintra_only) cycle kloop
            if (lmolok.and.lbtyp) then
              if (imode.eq.1) then
                if (lregion1i.and.lregion1k) then
                  icm = 1
                  lbonded = .false.
                  do while (icm.le.nbondsdef(i).and..not.lbonded)
                    imm = nbondeddef(icm,i)
                    lbonded = (imm.eq.k)
                    icm = icm + 1
                  enddo
                else
                  if (npsi.gt.0.and.npsk.gt.0) then
                    call mindtoijk(indmk,kxx,kyy,kzz)
                    call mindtoijk(indm,ixx,iyy,izz)
                    kxx = kxx - ixx
                    kyy = kyy - iyy
                    kzz = kzz - izz
                    indmmk = nmolind(npsk)
                    indmmi = nmolind(npsi)
                    call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                    call mindtoijk(indmmi,ixx2,iyy2,izz2)
                    kxx = kxx + kxx2 - ixx2
                    kyy = kyy + kyy2 - iyy2
                    kzz = kzz + kzz2 - izz2
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,npsi,npsk,kxx,kyy,kzz)
                  else
                    lbonded = .false.
                  endif
                endif
              else
                if (npsi.gt.0.and.npsk.gt.0) then
                  call mindtoijk(indmk,kxx,kyy,kzz)
                  call mindtoijk(indm,ixx,iyy,izz)
                  kxx = kxx - ixx
                  kyy = kyy - iyy
                  kzz = kzz - izz
                  indmmk = nmolind(npsk)
                  indmmi = nmolind(npsi)
                  call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                  call mindtoijk(indmmi,ixx2,iyy2,izz2)
                  kxx = kxx + kxx2 - ixx2
                  kyy = kyy + kyy2 - iyy2
                  kzz = kzz + kzz2 - izz2
                  call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,npsi,npsk,kxx,kyy,kzz)
                else
                  lbonded = .false.
                endif
              endif
              if (.not.lbonded) cycle kloop
            endif
          else
            lmolok = .false.
          endif
          if (lbtyp.and..not.lmolok) cycle kloop
          x31 = xc3 - xc1
          y31 = yc3 - yc1
          z31 = zc3 - zc1
!
!  Check r31 is OK
!
          r312 = x31*x31 + y31*y31 + z31*z31
          if (r312.lt.1d-10) cycle kloop
          if ((r312.gt.tr2.or.r312.lt.tr2min).and.(.not.lbtyp.or..not.lbonded)) cycle kloop
          r31 = sqrt(r312)
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
          lloop: do l = 1,k-1
            lregion1l = (l.le.nr1)
            if (lregion1l) then
              if (imode.eq.1) then
                nl = natdefe(l)
                ntypl = ntypdefe(l)
                xc4 = xdefe(l)
                yc4 = ydefe(l)
                zc4 = zdefe(l)
                ocl = occdefe(l)
                if (lmol.and.lneedmol) then
                  nml = ndefmol(l)
                  indml = ndefind(l)
                endif
                if (nreldef(l).gt.0) then
                  npsl = npsite(nreldef(l))
                else
                  npsl = 0
                endif
              else
                nl = natp(l)
                ntypl = ntypep(l)
                xc4 = xperf(l)
                yc4 = yperf(l)
                zc4 = zperf(l)
                ocl = occp(l)
                if (lmol.and.lneedmol) then
                  nml = ndefmolp(l)
                  indml = ndefindp(l)
                endif
                npsl = npsite(l)
              endif
              if (ldsl) then
                lin4 = (ndrelop(l).eq.1)
                lr = ndrel(l)
                neql = ndeqv(lr)
              endif
            else
              lin4 = .false.
              ll = l - nr1
              nl = nr2a(ll)
              ntypl = ntr2a(ll)
              if (lnodisp) then
                xc4 = xr2a(ll)
                yc4 = yr2a(ll)
                zc4 = zr2a(ll)
              else
                xc4 = xr2a(ll) + xdis(ll)
                yc4 = yr2a(ll) + ydis(ll)
                zc4 = zr2a(ll) + zdis(ll)
              endif
              ocl = or2a(ll)
              if (lmol.and.lneedmol) then
                nml = nmr2a(ll)
                indml = nmir2a(ll)
              endif
              npsl = nps(ll)
            endif
!
!  Check l is allowed for n
!
            if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  Prevent atoms i and l being the same atom
!
            if (l.eq.i) cycle lloop
!
!  Molecularity check
!
            lbonded = .false.
            if (lmol.and.lneedmol) then
              lmolok = (nmi.eq.nml.and.nmi.gt.0)
              if (lmolok) then
                ind = indml - indm
                lmolok = (ind.eq.0)
                if (.not.lmolok) then
                  call mindtoijk(indml,lxx,lyy,lzz)
                  call mindtoijk(indm,ixx,iyy,izz)
                  lxx = lxx - ixx
                  lyy = lyy - iyy
                  lzz = lzz - izz
                  call samemol(lmolok,nmi,lxx,lyy,lzz,0_i4,0_i4,0_i4)
                endif
              endif
              if (lmolok.and.linter_only) cycle lloop
              if (.not.lmolok.and.lintra_only) cycle lloop
              if (lmolok.and.lbtyp) then
                if (imode.eq.1) then
                  if (lregion1i.and.lregion1l) then
                    icm = 1
                    lbonded = .false.
                    do while (icm.le.nbondsdef(i).and..not.lbonded)
                      imm = nbondeddef(icm,i)
                      lbonded = (imm.eq.l)
                      icm = icm + 1
                    enddo
                  else
                    if (npsi.gt.0.and.npsl.gt.0) then
                      call mindtoijk(indml,lxx,lyy,lzz)
                      call mindtoijk(indm,ixx,iyy,izz)
                      lxx = lxx - ixx
                      lyy = lyy - iyy
                      lzz = lzz - izz
                      indmml = nmolind(npsl)
                      indmmi = nmolind(npsi)
                      call mindtoijk(indmml,lxx2,lyy2,lzz2)
                      call mindtoijk(indmmi,ixx2,iyy2,izz2)
                      lxx = lxx + lxx2 - ixx2
                      lyy = lyy + lyy2 - iyy2
                      lzz = lzz + lzz2 - izz2
                      call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,npsi,npsl,lxx,lyy,lzz)
                    else
                      lbonded = .false.
                    endif
                  endif
                else
                  if (npsi.gt.0.and.npsl.gt.0) then
                    call mindtoijk(indml,lxx,lyy,lzz)
                    call mindtoijk(indm,ixx,iyy,izz)
                    lxx = lxx - ixx
                    lyy = lyy - iyy
                    lzz = lzz - izz
                    indmml = nmolind(npsl)
                    indmmi = nmolind(npsi)
                    call mindtoijk(indmml,lxx2,lyy2,lzz2)
                    call mindtoijk(indmmi,ixx2,iyy2,izz2)
                    lxx = lxx + lxx2 - ixx2
                    lyy = lyy + lyy2 - iyy2
                    lzz = lzz + lzz2 - izz2
                    call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,npsi,npsl,lxx,lyy,lzz)
                  else
                    lbonded = .false.
                  endif
                endif
                if (.not.lbonded) cycle lloop
              endif
            else
              lmolok = .false.
            endif
            if (lbtyp.and..not.lmolok) cycle lloop
            x41 = xc4 - xc1
            y41 = yc4 - yc1
            z41 = zc4 - zc1
!
!  Check r41 is OK
!
            r412 = x41*x41 + y41*y41 + z41*z41
            if (r412.lt.1d-10) cycle lloop
            if ((r412.gt.tr3.or.r412.lt.tr3min).and.(.not.lbtyp.or..not.lbonded)) cycle lloop
            r41 = sqrt(r412)
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Finish calculating distances
!
            x32 = x31 - x21
            y32 = y31 - y21
            z32 = z31 - z21
            r322 = x32*x32 + y32*y32 + z32*z32
            r32 = sqrt(r322)
            x42 = x41 - x21
            y42 = y41 - y21
            z42 = z41 - z21
            r422 = x42*x42 + y42*y42 + z42*z42
            r42 = sqrt(r422)
            x43 = x41 - x31
            y43 = y41 - y31
            z43 = z41 - z31
            r432 = x43*x43 + y43*y43 + z43*z43
            r43 = sqrt(r432)
!
!  Call subroutine to calculate energy and derivatives
!
            ofct = oci*ocj*ock*ocl
            rko = rkfor*ofct
            if (nfortype.eq.14.or.nfortype.eq.16) then
              if (ntp2.eq.2.and.ntp3.eq.3) then
                fpoly(1) = forpoly(1,n)*ofct
                fpoly(2) = forpoly(2,n)*ofct
                fpoly(3) = forpoly(3,n)
                fpoly(4) = forpoly(4,n)
                fpoly(5) = forpoly(5,n)
              elseif (ntp2.eq.2.and.ntp3.eq.4) then
                fpoly(1) = rko
                rko = forpoly(1,n)*ofct
                fpoly(2) = forpoly(2,n)*ofct
                fpoly(3) = forpoly(4,n)
                fpoly(4) = forpoly(3,n)
                fpoly(5) = forpoly(5,n)
              elseif (ntp2.eq.3.and.ntp3.eq.2) then
                fpoly(1) = forpoly(2,n)*ofct
                fpoly(2) = forpoly(1,n)*ofct
                fpoly(3) = forpoly(3,n)
                fpoly(4) = forpoly(5,n)
                fpoly(5) = forpoly(4,n)
              elseif (ntp2.eq.3.and.ntp3.eq.4) then
                fpoly(1) = rko
                fpoly(2) = forpoly(1,n)*ofct
                rko = forpoly(2,n)*ofct
                fpoly(3) = forpoly(5,n)
                fpoly(4) = forpoly(3,n)
                fpoly(5) = forpoly(4,n)
              elseif (ntp2.eq.4.and.ntp3.eq.2) then
                fpoly(2) = rko
                rko = forpoly(1,n)*ofct
                fpoly(1) = forpoly(2,n)*ofct
                fpoly(3) = forpoly(4,n)
                fpoly(4) = forpoly(5,n)
                fpoly(5) = forpoly(3,n)
              elseif (ntp2.eq.4.and.ntp3.eq.3) then
                fpoly(2) = rko
                rko = forpoly(2,n)*ofct
                fpoly(1) = forpoly(1,n)*ofct
                fpoly(3) = forpoly(5,n)
                fpoly(4) = forpoly(4,n)
                fpoly(5) = forpoly(3,n)
              endif
            elseif (nfortype.eq.15) then
              fpoly(1) = forpoly(1,n)
              fpoly(2) = forpoly(2,n)
              fpoly(3) = forpoly(3,n)
            else
              fpoly(1) = rkfor4*ofct
              fpoly(2) = forpoly(2,n)
            endif
            call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko,rn,phi0,isgn,fpoly, &
                          lgrad1,lgrad2,.false.)
            efor = efor + eterm
!
!  Check that at least one species is in the asymmetric unit
!  for symmetry adapted run otherwise we can skip gradients.
!
            if (ldsl.and..not.lin1.and..not.lin2.and..not.lin3.and..not.lin4) cycle lloop
!*****************************
!  Out of plane derivatives  *
!*****************************
            if (lgrad2) then
!
!  Set coordinate pointers
!
              if (lregion1i) then
                n1x = 3*(i-1) + 1
                if (lin1) then
                  n1xr = 3*(ir-1) + 1
                endif
              else
                n1x = noffset + 1
              endif
              if (lregion1j) then
                n2x = 3*(j-1) + 1
                if (lin2) then
                  n2xr = 3*(jr-1) + 1
                endif
              else
                n2x = noffset + 1
              endif
              if (lregion1k) then
                n3x = 3*(k-1) + 1
                if (lin3) then
                  n3xr = 3*(kr-1) + 1
                endif
              else
                n3x = noffset + 1
              endif
              if (lregion1l) then
                n4x = 3*(l-1) + 1
                if (lin4) then
                  n4xr = 3*(lr-1) + 1
                endif
              else
                n4x = noffset + 1
              endif
            endif
            if (lgrad1) then
!*************************
!  Internal derivatives  *
!*************************
              if (lregion1i) then
                xderv(i) = xderv(i) - x21*e1d(1) - x31*e1d(2) - x41*e1d(3)
                yderv(i) = yderv(i) - y21*e1d(1) - y31*e1d(2) - y41*e1d(3)
                zderv(i) = zderv(i) - z21*e1d(1) - z31*e1d(2) - z41*e1d(3)
              endif
              if (lregion1j) then
                xderv(j) = xderv(j) - x32*e1d(4) + x21*e1d(1) - x42*e1d(5)
                yderv(j) = yderv(j) - y32*e1d(4) + y21*e1d(1) - y42*e1d(5)
                zderv(j) = zderv(j) - z32*e1d(4) + z21*e1d(1) - z42*e1d(5)
              endif
              if (lregion1k) then
                xderv(k) = xderv(k) + x32*e1d(4) - x43*e1d(6) + x31*e1d(2)
                yderv(k) = yderv(k) + y32*e1d(4) - y43*e1d(6) + y31*e1d(2)
                zderv(k) = zderv(k) + z32*e1d(4) - z43*e1d(6) + z31*e1d(2)
              endif
              if (lregion1l) then
                xderv(l) = xderv(l) + x43*e1d(6) + x42*e1d(5) + x41*e1d(3)
                yderv(l) = yderv(l) + y43*e1d(6) + y42*e1d(5) + y41*e1d(3)
                zderv(l) = zderv(l) + z43*e1d(6) + z42*e1d(5) + z41*e1d(3)
              endif
              if (lgrad2) then
!
!  New vector array between atoms to handle sign
!
!  Atom 1
!
                vec(1,1,1) = -x21
                vec(2,1,1) = -y21
                vec(3,1,1) = -z21
                vec(1,2,1) = -x31
                vec(2,2,1) = -y31
                vec(3,2,1) = -z31
                vec(1,3,1) = -x41
                vec(2,3,1) = -y41
                vec(3,3,1) = -z41
!
!  Atom 2
!
                vec(1,1,2) = x21
                vec(2,1,2) = y21
                vec(3,1,2) = z21
                vec(1,2,2) = -x32
                vec(2,2,2) = -y32
                vec(3,2,2) = -z32
                vec(1,3,2) = -x42
                vec(2,3,2) = -y42
                vec(3,3,2) = -z42
!
!  Atom 3
!
                vec(1,1,3) = x31
                vec(2,1,3) = y31
                vec(3,1,3) = z31
                vec(1,2,3) = x32
                vec(2,2,3) = y32
                vec(3,2,3) = z32
                vec(1,3,3) = -x43
                vec(2,3,3) = -y43
                vec(3,3,3) = -z43
!
!  Atom 4
!
                vec(1,1,4) = x41
                vec(2,1,4) = y41
                vec(3,1,4) = z41
                vec(1,2,4) = x42
                vec(2,2,4) = y42
                vec(3,2,4) = z42
                vec(1,3,4) = x43
                vec(2,3,4) = y43
                vec(3,3,4) = z43
!
!  Loop over first coordinate
!
                do kk = 1,3
                  n11 = n1x - 1 + kk
                  n21 = n2x - 1 + kk
                  n31 = n3x - 1 + kk
                  n41 = n4x - 1 + kk
!
!  First term
!
                  if (ld2sym) then
                    if (lin1) then
                      n11r = n1xr - 1 + kk
                      derv2(n21,n11r) = derv2(n21,n11r) - e1d(1)*neqi
                      derv2(n31,n11r) = derv2(n31,n11r) - e1d(2)*neqi
                      derv2(n41,n11r) = derv2(n41,n11r) - e1d(3)*neqi
                    endif
                    if (lin2) then
                      n21r = n2xr - 1 + kk
                      derv2(n11,n21r) = derv2(n11,n21r) - e1d(1)*neqj
                      derv2(n31,n21r) = derv2(n31,n21r) - e1d(4)*neqj
                      derv2(n41,n21r) = derv2(n41,n21r) - e1d(5)*neqj
                    endif
                    if (lin3) then
                      n31r = n3xr - 1 + kk
                      derv2(n11,n31r) = derv2(n11,n31r) - e1d(2)*neqk
                      derv2(n21,n31r) = derv2(n21,n31r) - e1d(4)*neqk
                      derv2(n41,n31r) = derv2(n41,n31r) - e1d(6)*neqk
                    endif
                    if (lin4) then
                      n41r = n4xr - 1 + kk
                      derv2(n11,n41r) = derv2(n11,n41r) - e1d(3)*neql
                      derv2(n21,n41r) = derv2(n21,n41r) - e1d(5)*neql
                      derv2(n31,n41r) = derv2(n31,n41r) - e1d(6)*neql
                    endif
                  else
                    derv2(n21,n11) = derv2(n21,n11) - e1d(1)
                    derv2(n31,n11) = derv2(n31,n11) - e1d(2)
                    derv2(n11,n41) = derv2(n11,n41) - e1d(3)
                    derv2(n21,n31) = derv2(n21,n31) - e1d(4)
                    derv2(n21,n41) = derv2(n21,n41) - e1d(5)
                    derv2(n31,n41) = derv2(n31,n41) - e1d(6)
!
                    derv2(n11,n21) = derv2(n11,n21) - e1d(1)
                    derv2(n11,n31) = derv2(n11,n31) - e1d(2)
                    derv2(n41,n11) = derv2(n41,n11) - e1d(3)
                    derv2(n31,n21) = derv2(n31,n21) - e1d(4)
                    derv2(n41,n21) = derv2(n41,n21) - e1d(5)
                    derv2(n41,n31) = derv2(n41,n31) - e1d(6)
                  endif
!
!  Loop over second coordinate
!
                  do kl = 1,3
                    n22 = n2x - 1 + kl
                    n32 = n3x - 1 + kl
                    n42 = n4x - 1 + kl
!
!  Sum over vectors atom-atom second derivatives
!
                    t12 = 0.0_dp
                    t13 = 0.0_dp
                    t14 = 0.0_dp
                    t23 = 0.0_dp
                    t24 = 0.0_dp
                    t34 = 0.0_dp
                    do ki = 1,3
                      do kj = 1,3
                        t12 = t12 + vec(kk,ki,1)*vec(kl,kj,2)*e2d(kb(n3vec(ki,1),n3vec(kj,2)))
                        t13 = t13 + vec(kk,ki,1)*vec(kl,kj,3)*e2d(kb(n3vec(ki,1),n3vec(kj,3)))
                        t14 = t14 + vec(kk,ki,1)*vec(kl,kj,4)*e2d(kb(n3vec(ki,1),n3vec(kj,4)))
                        t23 = t23 + vec(kk,ki,2)*vec(kl,kj,3)*e2d(kb(n3vec(ki,2),n3vec(kj,3)))
                        t24 = t24 + vec(kk,ki,2)*vec(kl,kj,4)*e2d(kb(n3vec(ki,2),n3vec(kj,4)))
                        t34 = t34 + vec(kk,ki,3)*vec(kl,kj,4)*e2d(kb(n3vec(ki,3),n3vec(kj,4)))
                      enddo
                    enddo
!
                    if (ld2sym) then
                      if (lin1) then
                        derv2(n22,n11r) = derv2(n22,n11r) + t12*neqi
                        derv2(n32,n11r) = derv2(n32,n11r) + t13*neqi
                        derv2(n42,n11r) = derv2(n42,n11r) + t14*neqi
                      endif
                      if (lin2) then
                        n22r = n2xr - 1 + kl
                        derv2(n32,n21r) = derv2(n32,n21r) + t23*neqj
                        derv2(n42,n21r) = derv2(n42,n21r) + t24*neqj
                        derv2(n11,n22r) = derv2(n11,n22r) + t12*neqj
                      endif
                      if (lin3) then
                        n32r = n3xr - 1 + kl
                        derv2(n42,n31r) = derv2(n42,n31r) + t34*neqk
                        derv2(n11,n32r) = derv2(n11,n32r) + t13*neqk
                        derv2(n21,n32r) = derv2(n21,n32r) + t23*neqk
                      endif
                      if (lin4) then
                        n42r = n4xr - 1 + kl
                        derv2(n31,n42r) = derv2(n31,n42r) + t34*neql
                        derv2(n21,n42r) = derv2(n21,n42r) + t24*neql
                        derv2(n11,n42r) = derv2(n11,n42r) + t14*neql
                      endif
                    else
                      derv2(n21,n32) = derv2(n21,n32) + t23
                      derv2(n22,n11) = derv2(n22,n11) + t12
                      derv2(n21,n42) = derv2(n21,n42) + t24
                      derv2(n32,n11) = derv2(n32,n11) + t13
                      derv2(n31,n42) = derv2(n31,n42) + t34
                      derv2(n11,n42) = derv2(n11,n42) + t14
!
                      derv2(n32,n21) = derv2(n32,n21) + t23
                      derv2(n11,n22) = derv2(n11,n22) + t12
                      derv2(n42,n21) = derv2(n42,n21) + t24
                      derv2(n11,n32) = derv2(n11,n32) + t13
                      derv2(n42,n31) = derv2(n42,n31) + t34
                      derv2(n42,n11) = derv2(n42,n11) + t14
                    endif
                  enddo
                enddo
              endif
            endif
!
!  End of inner loops over atoms
!
          enddo lloop
        enddo kloop
      enddo jloop
    enddo iloop
!
!  End of outer loops
!
  enddo pots
!
  return
  end
