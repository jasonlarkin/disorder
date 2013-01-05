  subroutine four12(efor,lgrad1,lgrad2,imode)
!
!  Subroutine for four-body energy of defects
!
!  imode = 1 => defective regions 1 and 2a
!  imode = 2 => perfect regions 1 and 2a
!
!  Strategy - sift by potential first, then cutoffs
!
!  Now symmetry adapted
!
!   1/95 Intra/inter-molecular specification added
!   2/95 Phi0 offset added for standard torsion potential
!   2/95 Bonded specification added for three-body terms
!   3/95 Corrections for periodic molecules added
!  10/96 Displacements no longer added to region 2a coordinates
!        by default
!   4/97 Modified for out of plane potentials
!   4/97 Error in second derivative storage when breathing shells
!        are present corrected
!   8/98 Potential parts placed in separate subroutine
!   8/98 Second derivatives re-written for clarity
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   9/01 lmolq calculations accelerated using lneedmol
!  10/02 Torharm potential added
!  11/02 Wildcard atom types added
!   4/04 Exponentially decaying torsion added
!   4/04 Tapered torsion added
!  11/04 Torangle potential added
!   7/05 Deallocations cleaned
!   9/06 Order of atom search changed to allow for Dreiding
!   9/06 Dreiding scheme for force constant added as an option
!   1/07 Wildcard handling in lmatch calls corrected
!   1/07 UFF4 added
!   2/07 Bonding types and test added
!   4/07 Code reordered so that atom loops are on the outside and potentials on the inside
!   4/07 Screening of j & k for potential middle species added
!   4/07 Bond type checking extended to ndim > 0
!   6/07 lmolok reset to initial value for each potential loop
!  10/07 Angle-angle cross potential added
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  10/07 Handling of n4botype(2,n) modified
!  12/07 Unused variables removed
!   5/08 UFFoop potential added
!   5/08 Defect bonding array structure changed
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Setting of maximum cutoffs now only looks at non-out of plane potentials
!  11/08 New logic for matching species introduced to handle case of the same element
!        being the middle atom, but one with a specific type and the other without.
!  11/08 Corrections for potential dependent swapping of terms according to atom 
!        assignments added.
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
  use control,         only : keyword, ld1sym, ld2sym
  use current
  use defects
  use derivatives
  use four
  use mdlogic
  use molecule
  use region2a
  use times
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout)                      :: efor
  logical,  intent(in)                         :: lgrad1
  logical,  intent(in)                         :: lgrad2
  integer(i4), intent(in)                      :: imode
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: icm
  integer(i4)                                  :: ii
  integer(i4)                                  :: imm
  integer(i4)                                  :: isgn
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: indmmi
  integer(i4)                                  :: indmmj
  integer(i4)                                  :: indmmk
  integer(i4)                                  :: indmml
  integer(i4)                                  :: ir
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: ixx2
  integer(i4)                                  :: iyy2
  integer(i4)                                  :: izz2
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jr
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: jxx2
  integer(i4)                                  :: jyy2
  integer(i4)                                  :: jzz2
  integer(i4)                                  :: k
  integer(i4)                                  :: kb(6,6)
  integer(i4)                                  :: ki
  integer(i4)                                  :: kj
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kr
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: kxx2
  integer(i4)                                  :: kyy2
  integer(i4)                                  :: kzz2
  integer(i4)                                  :: l
  integer(i4)                                  :: ll
  integer(i4)                                  :: lr
  integer(i4)                                  :: lxx
  integer(i4)                                  :: lyy
  integer(i4)                                  :: lzz
  integer(i4)                                  :: lxx2
  integer(i4)                                  :: lyy2
  integer(i4)                                  :: lzz2
  integer(i4)                                  :: n
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
  integer(i4)                                  :: n1x
  integer(i4)                                  :: n1xr
  integer(i4)                                  :: n2x
  integer(i4)                                  :: n2xr
  integer(i4)                                  :: n3x
  integer(i4)                                  :: n3xr
  integer(i4)                                  :: n4x
  integer(i4)                                  :: n4xr
  integer(i4)                                  :: n3vec(3,4)
  integer(i4), dimension(:), allocatable       :: natmiddle
  integer(i4), dimension(:), allocatable       :: ntypmiddle
  integer(i4)                                  :: nbtypeji
  integer(i4)                                  :: nbtypejk
  integer(i4)                                  :: nbtypekl
  integer(i4)                                  :: nbtypeji2
  integer(i4)                                  :: nbtypejk2
  integer(i4)                                  :: nbtypekl2
  integer(i4)                                  :: neq
  integer(i4)                                  :: neqi
  integer(i4)                                  :: neqj
  integer(i4)                                  :: neqk
  integer(i4)                                  :: neql
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: nfornonoop
  integer(i4)                                  :: ni
  integer(i4)                                  :: nil
  integer(i4)                                  :: niltor
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmiddle
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nn
  integer(i4)                                  :: noofp
  integer(i4)                                  :: noffset
  integer(i4)                                  :: npha
  integer(i4)                                  :: npsi
  integer(i4)                                  :: npsj
  integer(i4)                                  :: npsk
  integer(i4)                                  :: npsl
  integer(i4), dimension(:), allocatable       :: nptrnfornonoop
  integer(i4)                                  :: nr1
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntot
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: status
  logical                                      :: l2bondsij
  logical                                      :: l2bondsjk
  logical                                      :: l2bondskl
  logical                                      :: lanybtyp
  logical                                      :: lanyneedmol
  logical                                      :: lbondedij
  logical                                      :: lbondedjk
  logical                                      :: lbondedkl
  logical                                      :: lbtyp
  logical                                      :: ldsl
  logical                                      :: lexactmatch
  logical                                      :: liok
  logical                                      :: limatch1
  logical                                      :: limatch4
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: lin1
  logical                                      :: lin2
  logical                                      :: lin3
  logical                                      :: lin4
  logical                                      :: ljmatch2
  logical                                      :: ljmatch3
  logical                                      :: ljkmatch
  logical                                      :: lkjmatch
  logical                                      :: lkmatch2
  logical                                      :: lkmatch3
  logical                                      :: lmatch
  logical                                      :: lmatchany
  logical                                      :: lmatchpair
  logical                                      :: lmeither
  logical                                      :: lmolok
  logical                                      :: lmolokjk
  logical                                      :: lnodisp
  logical                                      :: lneedmol
  logical                                      :: lregion1i
  logical                                      :: lregion1j
  logical                                      :: lregion1k
  logical                                      :: lregion1l
  logical                                      :: lsamemoljk
  logical                                      :: lswitchil
  logical                                      :: lswitchjk
  logical                                      :: ltsyme_exact
  real(dp)                                     :: cputime
  real(dp)                                     :: cut
  real(dp)                                     :: cutmax
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: eterm
  real(dp)                                     :: fpoly(5)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ofct 
  real(dp)                                     :: phi0 
  real(dp)                                     :: phi0o
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
  real(dp)                                     :: rkforloc
  real(dp)                                     :: rko
  real(dp)                                     :: rn
  real(dp)                                     :: rtmp
  real(dp)                                     :: t12
  real(dp)                                     :: t13
  real(dp)                                     :: t14
  real(dp)                                     :: t23
  real(dp)                                     :: t24
  real(dp)                                     :: t34
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr2max
  real(dp)                                     :: tr3
  real(dp)                                     :: tr4
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
  real(dp), dimension(:), allocatable          :: xderv
  real(dp), dimension(:), allocatable          :: yderv
  real(dp), dimension(:), allocatable          :: zderv
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17,18,5,10,14,17,19,20,6,11,15,18,20,21/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
!
  time1 = cputime()
  ldsl = (ld1sym.and.(.not.lgrad2.or.ld2sym).and.imode.eq.1)
  lnodisp = (index(keyword,'r234').eq.0)       
  if (.not.ldsl) then                          
    lin1 = .false.                             
    lin2 = .false.                             
    lin3 = .false.                             
    lin4 = .false.                             
  endif
  if (imode.eq.1) then
    nr1 = nreg1
  else
    nr1 = nreg1old
  endif
  ntot = nr1 + ntreg2
  noffset = 3*nr1
  if (ldbsm) noffset = noffset + nr1
!
!  Allocate local memory
!
  allocate(natmiddle(2*nfor),stat=status)
  if (status/=0) call outofmemory('four12','natmiddle')
  allocate(ntypmiddle(2*nfor),stat=status)
  if (status/=0) call outofmemory('four12','ntypmiddle')
  allocate(nptrnfornonoop(nfor),stat=status)
  if (status/=0) call outofmemory('four12','nptrnfornonoop')
  allocate(xderv(nr1),stat=status)
  if (status/=0) call outofmemory('four12','xderv')
  allocate(yderv(nr1),stat=status)
  if (status/=0) call outofmemory('four12','yderv')
  allocate(zderv(nr1),stat=status)
  if (status/=0) call outofmemory('four12','zderv')
!
!  Initialisation
!
  efor = 0.0_dp
  if (lgrad1) then
    do i = 1,nr1
      xderv(i) = 0.0_dp
      yderv(i) = 0.0_dp
      zderv(i) = 0.0_dp
    enddo
  endif
!
!  Check how many four-body potentials are of out of plane type and how many aren't
!
  noofp = 0
  nfornonoop = 0
  do n = 1,nfor
    if (loutofplane(n)) then
      noofp = noofp + 1
    else
      nfornonoop = nfornonoop + 1
      nptrnfornonoop(nfornonoop) = n
    endif
  enddo
!
!  Find out if any require molecule information and whether any potential is of bonded type
!
  lanybtyp = .false.
  lanyneedmol = .false.
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      lbtyp = (mmfexc(n).eq.1)
      lintra_only = (lfintra(n).and..not.lfinter(n))
      linter_only = (lfinter(n).and..not.lfintra(n))
      lneedmol = (lintra_only.or.linter_only.or.lbtyp)
      if (lneedmol) lanyneedmol = .true.
      if (lbtyp) lanybtyp = .true.
    endif
  enddo
!
!  Build a list of middle atom species types for potentials
!
  nmiddle = 0
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      if (.not.lmatchany(nfspec2(n),nfptyp2(n),nmiddle,natmiddle,ntypmiddle)) then
        nmiddle = nmiddle + 1
        natmiddle(nmiddle) = nfspec2(n)
        ntypmiddle(nmiddle) = nfptyp2(n)
      endif
      if (.not.lmatchany(nfspec3(n),nfptyp3(n),nmiddle,natmiddle,ntypmiddle)) then
        nmiddle = nmiddle + 1
        natmiddle(nmiddle) = nfspec3(n)
        ntypmiddle(nmiddle) = nfptyp3(n)
      endif
    endif
  enddo
!
!  Find maximum cutoff distance for ends and middle atoms
!
  cutmax = 0.0_dp
  tr2max = 0.0_dp
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      cut = for1(n) + for2(n) + for3(n)
      if (for4(n).gt.0.0_dp) cut = for4(n)
      cutmax = max(cut,cutmax)
      tr2 = for2(n)**2
      tr2max = max(tr2,tr2max)
    endif
  enddo
!****************************************************************************
!  If there are no non out of plane potentials then we can skip everything  *
!****************************************************************************
  if (nfornonoop.eq.0) goto 5
!********************************
!  Loop over middle site 2 / j  *
!********************************
  jloop:  do j = 1,ntot-1
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
    if (.not.lmatchany(nj,ntypj,nmiddle,natmiddle,ntypmiddle)) cycle jloop
!***************************************
!  Loop over second middle site 3 / k  *
!***************************************
    kloop: do k = j+1,ntot
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
      if (.not.lmatchany(nk,ntypk,nmiddle,natmiddle,ntypmiddle)) cycle kloop
      if (.not.lmatchpair(nj,ntypj,nk,ntypk,nfor,nfspec2,nfptyp2,nfspec3,nfptyp3)) cycle kloop
!
      x32 = xc3 - xc2
      y32 = yc3 - yc2
      z32 = zc3 - zc2
!
!  Check r32 is OK
!
      r322 = x32*x32 + y32*y32 + z32*z32
      if (r322.lt.1d-12) cycle kloop
!
!
!  Molecularity check
!
      lbondedjk = .false.
      if (lmol.and.lanyneedmol) then
        lmolokjk = (nmj.eq.nmk.and.nmj.gt.0)
        if (lmolokjk) then
          ind = indmk - indmj
          lmolokjk = (ind.eq.0)
          if (.not.lmolokjk) then
            call mindtoijk(indmk,kxx,kyy,kzz)
            call mindtoijk(indmj,jxx,jyy,jzz)
            kxx = kxx - jxx
            kyy = kyy - jyy
            kzz = kzz - jzz
            call samemol(lmolokjk,nmj,kxx,kyy,kzz,0_i4,0_i4,0_i4)
          endif 
        endif
        if (lmolokjk.and.lanybtyp) then
          if (imode.eq.1) then
            if (lregion1j.and.lregion1k) then
              icm = 1
              lbondedjk = .false.
              do while (icm.le.nbondsdef(j).and..not.lbondedij)
                imm = nbondeddef(icm,j)
                lbondedjk = (imm.eq.k)
                icm = icm + 1
              enddo
            else
              if (npsj.gt.0.and.npsk.gt.0) then
                call mindtoijk(indmk,kxx,kyy,kzz)
                call mindtoijk(indmj,jxx,jyy,jzz)
                kxx = kxx - jxx
                kyy = kyy - jyy
                kzz = kzz - jzz
                indmmk = nmolind(npsk)
                indmmj = nmolind(npsj)
                call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                kxx = kxx + kxx2 - jxx2
                kyy = kyy + kyy2 - jyy2
                kzz = kzz + kzz2 - jzz2
                call bonded(lbondedjk,l2bondsjk,nbtypejk,nbtypejk2,npsj,npsk,kxx,kyy,kzz)
                lsamemoljk = (lbondedjk.or.l2bondsjk)
              else
                lbondedjk = .false.
                lsamemoljk = .false.
              endif
            endif
          else
            if (npsj.gt.0.and.npsk.gt.0) then
              call mindtoijk(indmk,kxx,kyy,kzz)
              call mindtoijk(indmj,jxx,jyy,jzz)
              kxx = kxx - jxx
              kyy = kyy - jyy
              kzz = kzz - jzz
              indmmk = nmolind(npsk)
              indmmj = nmolind(npsj)
              call mindtoijk(indmmk,kxx2,kyy2,kzz2)
              call mindtoijk(indmmj,jxx2,jyy2,jzz2)
              kxx = kxx + kxx2 - jxx2
              kyy = kyy + kyy2 - jyy2
              kzz = kzz + kzz2 - jzz2
              call bonded(lbondedjk,l2bondsjk,nbtypejk,nbtypejk2,npsj,npsk,kxx,kyy,kzz)
              lsamemoljk = (lbondedjk.or.l2bondsjk)
            else
              lbondedjk = .false.
              lsamemoljk = .false.
            endif
          endif
        endif
      else
        lmolokjk = .false.
      endif
!  
!  Distance checking
!     
      if (r322.gt.tr2max.and.(.not.lanybtyp.or..not.lbondedjk)) cycle kloop
!
!  Set counter for number of valid i/l end atom combinations
!
      niltor = 0
!***********************************
!  Loop over four-body potentials  *
!***********************************
      pots: do nn = 1,nfornonoop
        n = nptrnfornonoop(nn)
        nfortype = nforty(n)
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
        tr4 = for4(n)**2
        ltsyme_exact = lexactmatch(nt1,ntyp1,nt4,ntyp4)
        lbtyp = (mmfexc(n).eq.1)
        lintra_only = (lfintra(n).and..not.lfinter(n))
        linter_only = (lfinter(n).and..not.lfintra(n))
        lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Reset lmolok to initial state for j-k pair for each potential
!
        lmolok = lmolokjk
!************************************
!  Validate potential for j-k pair  *
!************************************
!
!  Check whether j and k are allowed for n
!
        ljmatch2 = lmatch(nj,ntypj,nt2,ntyp2,.true.)
        ljmatch3 = lmatch(nj,ntypj,nt3,ntyp3,.true.)
        lkmatch2 = lmatch(nk,ntypk,nt2,ntyp2,.true.)
        lkmatch3 = lmatch(nk,ntypk,nt3,ntyp3,.true.)
!
!  Check whether j-k or k-j orders are OK for 2-3
!
        ljkmatch = (ljmatch2.and.lkmatch3)
        lkjmatch = (ljmatch3.and.lkmatch2)
!
!  If no pair of matches can be found then cycle
!
        if (.not.ljkmatch.and..not.lkjmatch) cycle pots
        lswitchil = .false.
        if (.not.ljkmatch) then
!
!  If j-k doesn't match, but k-j does then swap terms
!
          ntmp = nt2
          nt2 = nt3
          nt3 = ntmp
          ntmp = ntyp2
          ntyp2 = ntyp3
          ntyp3 = ntmp
          rtmp = tr1
          tr1 = tr3
          tr3 = rtmp
          if (.not.ltsyme_exact) then
            ntmp = nt1
            nt1 = nt4
            nt4 = ntmp
            ntmp = ntyp1
            ntyp1 = ntyp4
            ntyp4 = ntmp
          endif
          lswitchil = .true.
        endif
!
!  Set flag indicating whether middle atoms could be matched either way round
!
        lmeither = (ljkmatch.and.lkjmatch)
!
!  Distance checking for j-k
!
        if (r322.gt.tr2.and.(.not.lbtyp.or..not.lbondedjk)) cycle pots
!       
!  Check for intra and but not in same molecule
!       
        if (lintra_only.and..not.lmolok) cycle pots
        if (lbtyp.and..not.lmolok) cycle pots
!
!  Molecule checking
!
        if (lmolok) then
          if (ndim.eq.0) then
            if (linter_only) cycle pots
            if (lbtyp) then
              if (.not.lbondedjk) cycle pots
!               
!  Check central bond type for correct order
!                 
              if (n4botype(1,n).gt.0) then
                if (n4botype(1,n).ne.nbtypejk) cycle pots
              endif
              if (n4botype(2,n).ne.nbtypejk2) cycle pots
            endif
          else
            if (lbtyp) then
              if (.not.lbondedjk) cycle pots
!               
!  Check central bond type for correct order
!                 
              if (n4botype(1,n).gt.0) then
                if (n4botype(1,n).ne.nbtypejk) cycle pots
              endif
              if (n4botype(2,n).ne.nbtypejk2) cycle pots
            endif
            if (lintra_only.and..not.lsamemoljk) cycle pots
            if (linter_only.and.lsamemoljk) cycle pots
          endif
        endif
!*****************************
!  Loop over end site 1 / i  *
!*****************************
        iloop: do i = 1,ntot
!  
!  Prevent atoms i and k being the same atom
!       
          if (i.eq.k) cycle iloop
!       
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
                indmi = ndefind(i)
              endif
              if (nreldef(i).gt.0) then
                npsi = npsite(nreldef(i))
              else
                npsi = 0
              endif
            else
              ni = natp(i)
              ntypi = ntypep(i)
              xc1 = xperf(i)
              yc1 = yperf(i)
              zc1 = zperf(i)
              oci = occp(i)
              if (lmol.and.lneedmol) then
                nmi = ndefmolp(i)
                indmi = ndefindp(i)
              endif
              npsi = npsite(i)
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
              indmi = nmir2a(ii)
            endif
            npsi = nps(ii)
          endif
!
!  Check whether i matches either of types 1 and 4
!
          limatch1 = lmatch(ni,ntypi,nt1,ntyp1,.true.)
          limatch4 = lmatch(ni,ntypi,nt4,ntyp4,.true.)
!
!  Is i allowed for type 1, or type 4 if the middle atoms can be switched?
!
          liok = (limatch1.or.(limatch4.and.lmeither))
          if (.not.liok) cycle iloop
!
          lswitchjk = .false.
          if (.not.limatch1.and.(limatch4.and.lmeither)) then
!
!  Switch round order of torsional atoms
!
            ntmp = nt1
            nt1 = nt4
            nt4 = ntmp
            ntmp = ntyp1
            ntyp1 = ntyp4
            ntyp4 = ntmp
            rtmp = tr1
            tr1 = tr3
            tr3 = rtmp
            lswitchjk = .true.
          endif
!
!  Molecule handling
!
          lbondedij = .false.
          if (lmol.and.lneedmol) then
            lmolok = (nmj.eq.nmi.and.nmj.gt.0)
            if (lmolok) then
              ind = indmi - indmj
              lmolok = (ind.eq.0)
              if (.not.lmolok) then
                call mindtoijk(indmi,ixx,iyy,izz)
                call mindtoijk(indmj,jxx,jyy,jzz)
                ixx = ixx - jxx
                iyy = iyy - jyy
                izz = izz - jzz
                call samemol(lmolok,nmj,ixx,iyy,izz,0_i4,0_i4,0_i4)
              endif
            endif
            if (lmolok.and.linter_only) cycle iloop
            if (.not.lmolok.and.lintra_only) cycle iloop
            if (lmolok.and.lbtyp) then
              if (imode.eq.1) then
                if (lregion1i.and.lregion1j) then
                  lbondedij = .false.
                  icm = 1
                  do while (icm.le.nbondsdef(i).and..not.lbondedij)
                    imm = nbondeddef(icm,i)
                    lbondedij = (imm.eq.j)
                    icm = icm + 1
                  enddo
                else
                  if (npsi.gt.0.and.npsj.gt.0) then
                    call mindtoijk(indmi,ixx,iyy,izz)
                    call mindtoijk(indmj,jxx,jyy,jzz)
                    ixx = ixx - jxx
                    iyy = iyy - jyy
                    izz = izz - jzz
                    indmmj = nmolind(npsj)
                    indmmi = nmolind(npsi)
                    call mindtoijk(indmmi,ixx2,iyy2,izz2)
                    call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                    ixx = ixx + ixx2 - jxx2
                    iyy = iyy + iyy2 - jyy2
                    izz = izz + izz2 - jzz2
                    call bonded(lbondedij,l2bondsij,nbtypeji,nbtypeji2,npsj,npsi,ixx,iyy,izz)
                  else
                    lbondedij = .false.
                  endif
                endif
              else
                if (npsi.gt.0.and.npsj.gt.0) then
                  call mindtoijk(indmi,ixx,iyy,izz)
                  call mindtoijk(indmj,jxx,jyy,jzz)
                  ixx = ixx - jxx
                  iyy = iyy - jyy
                  izz = izz - jzz
                  indmmi = nmolind(npsi)
                  indmmj = nmolind(npsj)
                  call mindtoijk(indmmi,ixx2,iyy2,izz2)
                  call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                  ixx = ixx + ixx2 - jxx2
                  iyy = iyy + iyy2 - jyy2
                  izz = izz + izz2 - jzz2
                  call bonded(lbondedij,l2bondsij,nbtypeji,nbtypeji2,npsj,npsi,ixx,iyy,izz)
                else
                  lbondedij = .false.
                endif
              endif
              if (.not.lbondedij) cycle iloop
            endif
          else
            lmolok = .false.
          endif
!
!  Check for intra and but not in same molecule
!
          if (lintra_only.and..not.lmolok) cycle iloop
          if (lbtyp.and..not.lmolok) cycle iloop
          x21 = xc2 - xc1
          y21 = yc2 - yc1
          z21 = zc2 - zc1
!
!  Check r21 is OK
!
          r212 = x21*x21 + y21*y21 + z21*z21
          if (r212.lt.1.0d-12) cycle iloop
          if (r212.gt.tr1.and.(.not.lbtyp.or..not.lbondedij)) cycle iloop
!
!  Check r31 is OK
!
          x31 = x32 + x21
          y31 = y32 + y21
          z31 = z32 + z21
          r312 = x31*x31 + y31*y31 + z31*z31
          if (r312.lt.1.0d-10) cycle iloop
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
          lloop: do l = 1,ntot
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
!  Prevent atoms j and l being the same atom
!           
            if (l.eq.j) cycle lloop
!  
!  Molecularity check
!           
            lbondedkl = .false.
            if (lmol.and.lneedmol) then
              lmolok = (nmi.eq.nml.and.nmi.gt.0)
              if (lmolok) then
                ind = indml - indmk
                lmolok = (ind.eq.0)
                if (.not.lmolok) then
                  call mindtoijk(indml,lxx,lyy,lzz)
                  call mindtoijk(indmk,kxx,kyy,kzz)
                  lxx = lxx - kxx
                  lyy = lyy - kyy
                  lzz = lzz - kzz
                  call samemol(lmolok,nmi,lxx,lyy,lzz,0_i4,0_i4,0_i4)
                endif
              endif
              if (lmolok.and.linter_only) cycle lloop
              if (.not.lmolok.and.lintra_only) cycle lloop
              if (lmolok.and.lbtyp) then
                if (imode.eq.1) then
                  if (lregion1k.and.lregion1l) then
                    icm = 1
                    lbondedkl = .false.
                    do while (icm.le.nbondsdef(k).and..not.lbondedkl)
                      imm = nbondeddef(icm,k)
                      lbondedkl = (imm.eq.l)
                      icm = icm + 1
                    enddo
                  else
                    if (npsk.gt.0.and.npsl.gt.0) then
                      call mindtoijk(indml,lxx,lyy,lzz)
                      call mindtoijk(indmk,kxx,kyy,kzz)
                      lxx = lxx - kxx
                      lyy = lyy - kyy
                      lzz = lzz - kzz
                      indmml = nmolind(npsl)
                      indmmk = nmolind(npsk)
                      call mindtoijk(indmml,lxx2,lyy2,lzz2)
                      call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                      lxx = lxx + lxx2 - kxx2
                      lyy = lyy + lyy2 - kyy2
                      lzz = lzz + lzz2 - kzz2
                      call bonded(lbondedkl,l2bondskl,nbtypekl,nbtypekl2,npsk,npsl,lxx,lyy,lzz)
                    else
                      lbondedkl = .false.
                    endif
                  endif
                else 
                  if (npsk.gt.0.and.npsl.gt.0) then
                    call mindtoijk(indml,lxx,lyy,lzz)
                    call mindtoijk(indmk,kxx,kyy,kzz)
                    lxx = lxx - kxx
                    lyy = lyy - kyy
                    lzz = lzz - kzz
                    indmml = nmolind(npsl)
                    indmmk = nmolind(npsk)
                    call mindtoijk(indmml,lxx2,lyy2,lzz2)
                    call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                    lxx = lxx + lxx2 - kxx2
                    lyy = lyy + lyy2 - kyy2
                    lzz = lzz + lzz2 - kzz2
                    call bonded(lbondedkl,l2bondskl,nbtypekl,nbtypekl2,npsk,npsl,lxx,lyy,lzz)
                  else
                    lbondedkl = .false.
                  endif
                endif
                if (.not.lbondedkl) cycle lloop
              endif
            else
              lmolok = .false.
            endif
            if (lbtyp.and..not.lmolok) cycle lloop
            x43 = xc4 - xc3
            y43 = yc4 - yc3
            z43 = zc4 - zc3
!
!  Check r43 is OK
!
            r432 = x43*x43 + y43*y43 + z43*z43
            if (r432.lt.1d-12) cycle lloop
            if (r432.gt.tr3.and.(.not.lbtyp.or..not.lbondedkl)) cycle lloop
!
!  Check r41 is OK
!
            x41 = x43 + x32 + x21
            y41 = y43 + y32 + y21
            z41 = z43 + z32 + z21
            r412 = x41*x41 + y41*y41 + z41*z41
            if (r412.lt.1.0d-12) cycle lloop
            if (r412.gt.tr4.and.tr4.gt.0.0_dp.and..not.lbtyp) cycle lloop
!
!  Check r42 is OK
!
            x42 = x32 + x43
            y42 = y32 + y43
            z42 = z32 + z43
            r422 = x42*x42 + y42*y42 + z42*z42
            if (r422.lt.1.0d-12) cycle lloop
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Finish calculating distances
!
            r21 = sqrt(r212)
            r31 = sqrt(r312)
            r32 = sqrt(r322)
            r41 = sqrt(r412)
            r42 = sqrt(r422)
            r43 = sqrt(r432)
!
!  Store information into iltor arrays
!
            niltor = niltor + 1
            if (niltor.gt.maxiltor) then
              maxiltor = niltor + 3
              call changemaxiltor
            endif
!
            nfortor(niltor) = n
            liltorswitch(niltor) = lswitchil
            ljktorswitch(niltor) = lswitchjk
!
            iltor(1,niltor) = i
            iltor(2,niltor) = l
            ilxtor(1,niltor) = ir
            ilxtor(2,niltor) = lr
!
            riltor(1,niltor) = r21
            riltor(2,niltor) = r31
            riltor(3,niltor) = r41
            riltor(4,niltor) = r42
            riltor(5,niltor) = r43
!
            xiltor(1,niltor) = x21
            yiltor(1,niltor) = y21
            ziltor(1,niltor) = z21
            xiltor(2,niltor) = x31
            yiltor(2,niltor) = y31
            ziltor(2,niltor) = z31
            xiltor(3,niltor) = x41
            yiltor(3,niltor) = y41
            ziltor(3,niltor) = z41
            xiltor(4,niltor) = x42
            yiltor(4,niltor) = y42
            ziltor(4,niltor) = z42
            xiltor(5,niltor) = x43
            yiltor(5,niltor) = y43
            ziltor(5,niltor) = z43
!
            oiltor(niltor) = oci*ocj*ock*ocl
            lopiltor(1,niltor) = lin1
            lopiltor(2,niltor) = lin4
            lsurfiltor(1,niltor) = lregion1i
            lsurfiltor(2,niltor) = lregion1l
!           
!  End of inner loops over atoms and cell vectors
!           
          enddo lloop
        enddo iloop
!
!  End loop over potentials
!
      enddo pots
!*******************************
!  Loop over i/l combinations  *
!*******************************
      do nil = 1,niltor
!
!  Return values to local variables
!
        n = nfortor(nil)
        lswitchil = liltorswitch(nil)
        lswitchjk = ljktorswitch(nil)
!
        i = iltor(1,nil)
        l = iltor(2,nil)
!
        ir = ilxtor(1,nil)
        lr = ilxtor(2,nil)
!
        r21 = riltor(1,nil)
        r31 = riltor(2,nil)
        r41 = riltor(3,nil)
        r42 = riltor(4,nil)
        r43 = riltor(5,nil)
!
        x21 = xiltor(1,nil)
        y21 = yiltor(1,nil)
        z21 = ziltor(1,nil)
        x31 = xiltor(2,nil)
        y31 = yiltor(2,nil)
        z31 = ziltor(2,nil)
        x41 = xiltor(3,nil)
        y41 = yiltor(3,nil)
        z41 = ziltor(3,nil)
        x42 = xiltor(4,nil)
        y42 = yiltor(4,nil)
        z42 = ziltor(4,nil)
        x43 = xiltor(5,nil)
        y43 = yiltor(5,nil)
        z43 = ziltor(5,nil)
!
        ofct = oiltor(nil)
        lin1 = lopiltor(1,nil)
        lin4 = lopiltor(2,nil)
        lregion1i = lsurfiltor(1,nil)
        lregion1l = lsurfiltor(2,nil)
!
!  Set terms for potentials
!
        rkforloc = fork(n)
!
!  If this is Dreiding mode then divide force constant by number of torsions
!
        if (lfdreiding(n)) then
          rkforloc = rkforloc/dble(niltor)
        endif
        npha = 0
        nfortype = nforty(n)
        if (nfortype.eq.1) then
          npha = npfor(n)
          if (npha.gt.0) then
            isgn = 1
          else
            isgn = - 1
          endif
          npha = abs(npha)
          phi0 = forpoly(1,n)*degtorad
        elseif (nfortype.eq.4.or.nfortype.eq.6.or.nfortype.eq.7) then
          npha = npfor(n)
          if (npha.gt.0) then
            isgn = 1
          else
            isgn = - 1
          endif
          npha = abs(npha)
          if (nfortype.eq.6) then
            phi0 = forpoly(1,n)*degtorad
          else
            phi0 = forpoly(1,n)
          endif
          if (nfortype.eq.6.or.nfortype.eq.7) then
            fpoly(2:4) = forpoly(2:4,n)
          endif
        elseif (nfortype.eq.8.or.nfortype.eq.9) then
          npha = npfor(n)
          if (npha.gt.0) then
            isgn = 1
          else
            isgn = - 1
          endif
          npha = abs(npha)
          if (nfortype.eq.8) then
            phi0 = forpoly(1,n)*degtorad
          else
            phi0 = forpoly(1,n)
          endif
          fpoly(2) = forpoly(2,n)
          fpoly(3) = for1(n)
          fpoly(4) = for2(n)
          fpoly(5) = for3(n)
        elseif (nfortype.eq.2) then
          npha = npfor(n)
        elseif (nfortype.eq.5) then
          phi0 = forpoly(1,n)*degtorad
        elseif (nfortype.eq.10.or.nfortype.eq.17) then
          fpoly(1) = forpoly(1,n)*degtorad
          fpoly(2) = forpoly(2,n)*degtorad
        elseif (nfortype.eq.13) then
          npha = abs(npfor(n))
          phi0 = forpoly(1,n)*degtorad
        endif
        rn = dble(npha)
!
!  Switch terms if necessary
!
        if (lswitchil) then
          if (nfortype.eq.8.or.nfortype.eq.9) then
            rtmp = fpoly(3)
            fpoly(3) = fpoly(5)
            fpoly(5) = rtmp
          elseif (nfortype.eq.6.or.nfortype.eq.7) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(4)
            fpoly(4) = rtmp
          elseif (nfortype.eq.10.or.nfortype.eq.17) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(1)
            fpoly(1) = rtmp
          endif
        endif
        if (lswitchjk) then
          if (nfortype.eq.8.or.nfortype.eq.9) then
            rtmp = fpoly(3)
            fpoly(3) = fpoly(5)
            fpoly(5) = rtmp
          elseif (nfortype.eq.6.or.nfortype.eq.7) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(4)
            fpoly(4) = rtmp
          elseif (nfortype.eq.10.or.nfortype.eq.17) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(1)
            fpoly(1) = rtmp
          endif
        endif
!
!  Scaling of terms
!
        rko = rkforloc*ofct
        phi0o = phi0
        if (nfortype.eq.2) then
          do kk = 1,npha
            fpoly(kk) = forpoly(kk,n)*ofct
          enddo
        elseif (nfortype.eq.4.or.nfortype.eq.7.or.nfortype.eq.9) then
          phi0o = phi0*ofct
        endif
!
!  Call subroutine to calculate energy and derivatives
!
        call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d, &
                      rko,rn,phi0o,isgn,fpoly,lgrad1,lgrad2,.false.)
        efor = efor + eterm
!         
!  Check that at least one species is in the asymmetric unit
!  for symmetry adapted run otherwise we can skip gradients.
!          
        if (ldsl.and..not.lin1.and..not.lin2.and..not.lin3.and..not.lin4) cycle
!**************************
!  Torsional derivatives  *
!**************************
        if (lgrad2) then
          if (lregion1i) then
            n1x = 3*(i - 1) + 1
            if (lin1) then
              n1xr = 3*(ir - 1) + 1
            endif
          else
            n1x = noffset + 1
          endif
          if (lregion1j) then
            n2x = 3*(j - 1) + 1
            if (lin2) then
              n2xr = 3*(jr - 1) + 1
            endif
          else
            n2x = noffset + 1
          endif
          if (lregion1k) then
            n3x = 3*(k - 1) + 1
            if (lin3) then
              n3xr = 3*(kr - 1) + 1
            endif
          else
            n3x = noffset + 1
          endif
          if (lregion1l) then
            n4x = 3*(l - 1) + 1
            if (lin4) then
              n4xr = 3*(lr - 1) + 1
            endif
          else
            n4x = noffset + 1
          endif
        endif
!*************************
!  Internal derivatives  *
!*************************
        if (lgrad1) then
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
              derv2(n21,n31) = derv2(n21,n31) - e1d(4)
              derv2(n21,n11) = derv2(n21,n11) - e1d(1)
              derv2(n31,n41) = derv2(n31,n41) - e1d(6)
              derv2(n31,n11) = derv2(n31,n11) - e1d(2)
              derv2(n21,n41) = derv2(n21,n41) - e1d(5)
              derv2(n11,n41) = derv2(n11,n41) - e1d(3)
!             
              derv2(n31,n21) = derv2(n31,n21) - e1d(4)
              derv2(n11,n21) = derv2(n11,n21) - e1d(1)
              derv2(n41,n31) = derv2(n41,n31) - e1d(6)
              derv2(n11,n31) = derv2(n11,n31) - e1d(2)
              derv2(n41,n21) = derv2(n41,n21) - e1d(5)
              derv2(n41,n11) = derv2(n41,n11) - e1d(3)
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
!  Add terms to total second derivative matrix
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
      enddo
    enddo kloop
  enddo jloop
!
!  End of outer loops
!
5 continue
!****************************
!  Out of plane potentials  *
!****************************
  if (noofp.gt.0) call fouroop12(efor,lgrad1,lgrad2,imode,xderv,yderv,zderv)
  if (lgrad1) then
    if (ldsl) then
!
!  Symmetry reduce gradients
!
      do i = 1,ndasym
        ii = ndsptr(i)
        neq = ndeqv(i)
        xdrv(i) = xdrv(i) + neq*xderv(ii)
        ydrv(i) = ydrv(i) + neq*yderv(ii)
        zdrv(i) = zdrv(i) + neq*zderv(ii)
      enddo     
    else        
!               
!  Add to main gradient vectors
!               
      do i = 1,nr1
        xdrv(i) = xdrv(i) + xderv(i)
        ydrv(i) = ydrv(i) + yderv(i)
        zdrv(i) = zdrv(i) + zderv(i)
      enddo     
    endif     
  endif
!
!  Free local memory
!
  deallocate(zderv,stat=status)
  if (status/=0) call deallocate_error('four12','zderv')
  deallocate(yderv,stat=status)
  if (status/=0) call deallocate_error('four12','yderv')
  deallocate(xderv,stat=status)
  if (status/=0) call deallocate_error('four12','xderv')
  deallocate(nptrnfornonoop,stat=status)
  if (status/=0) call deallocate_error('four12','nptrnfornonoop')
  deallocate(ntypmiddle,stat=status)
  if (status/=0) call deallocate_error('four12','ntypmiddle')
  deallocate(natmiddle,stat=status)
  if (status/=0) call deallocate_error('four12','natmiddle')
!
!  Timing
!
  time2 = cputime()
  tfour = tfour + time2 - time1
!
  return
  end
