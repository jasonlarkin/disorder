  subroutine fouroop(eoop,esregion12,esregion2,eattach,lgrad1,lgrad2,xderv,yderv,zderv)
!
!  Subroutine for four-body energy from out of plane potentials
!
!  Strategy - sift by potential first, then cutoffs
!
!   3/97 Created from four.f
!   8/98 Derivative structure changed to match new fourbody form
!   7/00 Error in rprod(6,2) corrected
!   2/01 Modifications for general dimensionality added
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol 
!   7/02 K4 added
!  10/02 Second derivatives modified to correct freezing bug
!  10/02 Surface energy corrections added
!  11/02 Wildcard atom types added
!   6/03 Bug for frozen second derivatives corrected
!   6/04 Sign of virial corrected
!  10/05 Modified for inversion form of out of plane potential
!   6/06 Inversion squared potential added
!   1/07 Wildcard handling in lmatch calls corrected
!   2/07 Bonding types added
!   5/07 QM/MM schemes added
!  10/07 Angle-angle cross potential added
!  12/07 Unused variables removed
!   4/08 Minimum cutoff added for out of plane potentials
!   5/08 UFFoop potential added
!   5/08 only3 check added as an option
!   7/08 New type checking algorithm introduced
!   7/08 Handling of xangle potential added w.r.t. to type swapping
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Option to output energy terms added
!   4/10 Code modified to increase speed for bonded potentials by only searching over
!        bonded atoms
!  12/10 Setting of derv3 now wrapped with checks as to whether relevant atom is being
!        optimised or not.
!  11/11 Region-region energy contributions stored
!  11/11 Out of plane site energy divided on a per bond basis
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
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
  use configurations, only : lsliceatom, nregions, nregionno, nregiontype, QMMMmode
  use control,        only : lseok, latomicstress
  use current
  use derivatives
  use energies,       only : eregion2region
  use four
  use iochannels,     only : ioout
  use mdlogic
  use molecule
  use optimisation
  use parallel,       only : ioproc
  use symmetry
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout)                      :: xderv(*)
  real(dp), intent(inout)                      :: yderv(*)
  real(dp), intent(inout)                      :: zderv(*)
  real(dp), intent(inout)                      :: eoop
  real(dp), intent(inout)                      :: esregion12
  real(dp), intent(inout)                      :: esregion2
  real(dp), intent(inout)                      :: eattach
  logical,  intent(in)                         :: lgrad1
  logical,  intent(in)                         :: lgrad2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: isgn
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4), dimension(:), allocatable       :: ioptptr
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixk
  integer(i4)                                  :: iyk
  integer(i4)                                  :: izk
  integer(i4)                                  :: ixl
  integer(i4)                                  :: iyl
  integer(i4)                                  :: izl
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjmax
  integer(i4)                                  :: jloop
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kb(6,6)
  integer(i4)                                  :: ki
  integer(i4)                                  :: kj
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kmax
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: l4
  integer(i4)                                  :: lj
  integer(i4)                                  :: lk
  integer(i4)                                  :: ll
  integer(i4)                                  :: lu
  integer(i4)                                  :: llmax
  integer(i4)                                  :: lmax
  integer(i4)                                  :: lx
  integer(i4)                                  :: ly
  integer(i4)                                  :: lz
  integer(i4)                                  :: n
  integer(i4)                                  :: n112
  integer(i4)                                  :: n113
  integer(i4)                                  :: n114
  integer(i4)                                  :: n1x
  integer(i4)                                  :: n1y
  integer(i4)                                  :: n1z
  integer(i4)                                  :: n1x2
  integer(i4)                                  :: n1x3
  integer(i4)                                  :: n1x4
  integer(i4)                                  :: n211
  integer(i4)                                  :: n213
  integer(i4)                                  :: n214
  integer(i4)                                  :: n221
  integer(i4)                                  :: n2x
  integer(i4)                                  :: n2y
  integer(i4)                                  :: n2z
  integer(i4)                                  :: n2x1
  integer(i4)                                  :: n2x3
  integer(i4)                                  :: n2x4
  integer(i4)                                  :: n311
  integer(i4)                                  :: n312
  integer(i4)                                  :: n314
  integer(i4)                                  :: n321
  integer(i4)                                  :: n322
  integer(i4)                                  :: n3x
  integer(i4)                                  :: n3y
  integer(i4)                                  :: n3z
  integer(i4)                                  :: n3x1
  integer(i4)                                  :: n3x2
  integer(i4)                                  :: n3x4
  integer(i4)                                  :: n411
  integer(i4)                                  :: n412
  integer(i4)                                  :: n413
  integer(i4)                                  :: n421
  integer(i4)                                  :: n422
  integer(i4)                                  :: n423
  integer(i4)                                  :: n4x
  integer(i4)                                  :: n4y
  integer(i4)                                  :: n4z
  integer(i4)                                  :: n4x1
  integer(i4)                                  :: n4x2
  integer(i4)                                  :: n4x3
  integer(i4)                                  :: n3vec(3,4)
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntp2
  integer(i4)                                  :: ntp3
  integer(i4)                                  :: ntp4
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: nunique
  integer(i4), dimension(:), allocatable       :: nuniqueptr
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: lattach
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: ljbond
  logical                                      :: lmatch
  logical                                      :: lmatch2
  logical                                      :: lmatch3
  logical                                      :: lmatchanyof2
  logical                                      :: lmatchanyof3
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lopk
  logical                                      :: lopl
  logical                                      :: lreg12
  logical                                      :: lreg2qtet
  logical                                      :: lsamemol
  logical                                      :: lsg1
  logical                                      :: lslicei
  logical                                      :: lslicej
  logical                                      :: lslicek
  logical                                      :: lslicel
  logical                                      :: lunique
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: eterm
  real(dp)                                     :: eterm3rd
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
  real(dp)                                     :: rprod(6,6)
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: t12
  real(dp)                                     :: t13
  real(dp)                                     :: t14
  real(dp)                                     :: t23
  real(dp)                                     :: t24
  real(dp)                                     :: t34
  real(dp)                                     :: temp(6)
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
  real(dp)                                     :: x21t
  real(dp)                                     :: y21t
  real(dp)                                     :: z21t
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x31t
  real(dp)                                     :: y31t
  real(dp)                                     :: z31t
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x41t
  real(dp)                                     :: y41t
  real(dp)                                     :: z41t
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
  real(dp)                                     :: xc2t
  real(dp)                                     :: yc2t
  real(dp)                                     :: zc2t
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17,18,5,10,14,17,19,20,6,11,15,18,20,21/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
!
  lsg1 = (lstr.and.lgrad1)
!
!  Allocate local memory
!
  allocate(nuniqueptr(numat),stat=status)
  if (status/=0) call outofmemory('fouroop','nuniqueptr')
  allocate(ioptptr(numat),stat=status)
  if (status/=0) call outofmemory('fouroop','ioptptr')
!
!  Initialisation
!
  lu = 0
  do i = 1,numat
    if (lopf(nrelat(i)).or..not.lfreeze) then
      lu = lu + 1
      ioptptr(i) = lu
    else
      ioptptr(i) = 0
    endif
  enddo
!
!  Openning banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  OOP  : Atom No. 1  Atom No. 2  Atom No. 3  Atom No. 4  OutOfPlane energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
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
    rkfor = fork(n)
    rkfor4 = forpoly(1,n)
    lintra_only = (lfintra(n).and..not.lfinter(n))
    linter_only = (lfinter(n).and..not.lfintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!********************************
!  Loop over middle site 1 / i  *
!********************************
    ix = - 2
    iy = - 1
    iz =   0
    liloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
      oci = occuf(i)
      lopi = (lopf(nrelat(i)).or..not.lfreeze)
      lslicei = lsliceatom(nsft + nrelat(i))
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
      endif
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle liloop
!
!  Only 3 bonds check
!
      if (lbtyp.and.lonly3oop(n).and.nbonds(i).ne.3) cycle liloop
!
!  QM/MM handling : i is a QM atom and potential is of bonded type => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle liloop
      endif
!
!  Set loop range for other atoms
!
      if (lbtyp) then
        ljbond = .true.
        if (nbonds(i).gt.0) then
          nunique = 1
          nuniqueptr(1) = nbonded(1,i)
          do jloop = 2,nbonds(i)
            lunique = .true.
            do lu = 1,nunique
              if (nbonded(jloop,i).eq.nuniqueptr(lu)) lunique = .false.
            enddo
            if (lunique) then
              nunique = nunique + 1
              nuniqueptr(nunique) = nbonded(jloop,i)
            endif
          enddo
          jloop = nunique
        else
          jloop = 0
        endif
      else
        ljbond = .false.
        jloop = numat
      endif
!
!  Skip if jloop is zero
!
      if (jloop.eq.0) cycle liloop
!
!  i has been accepted
!
      xc1 = xclat(i)
      yc1 = yclat(i)
      zc1 = zclat(i)
!
!  Molecule handling
!
      if (lmol.and.lneedmol) then
        nmi = natmol(i)
        if (ndim.gt.0) then
          indm = nmolind(i)
          call mindtoijk(indm,ixi,iyi,izi)
        endif
      endif
!***********************************
!  Loop over first end site 2 / j  *
!***********************************
      ljloop: do lj = 1,jloop
        if (ljbond) then
          j = nuniqueptr(lj)
        else
          j = lj
        endif
        nj = nat(j)
        ntypj = nftype(j)
!
!  Check j is allowed for n
!
        lmatch3 = lmatchanyof3(nj,ntypj,ntp2,nt2,ntyp2,tr1,tr1min,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
        if (.not.lmatch3) cycle ljloop
!
!  Set properties for atom j
!
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
        ocj = occuf(j)
        lopj = (lopf(nrelat(j)).or..not.lfreeze)
        lslicej = lsliceatom(nsft + nrelat(j))
        if (lopj) then
          jx = 3*(ioptptr(j) - 1) + 1
          jy = jx + 1
          jz = jx + 2
        endif
!
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          if (ndim.gt.0) then
            indmj = nmolind(j)
            call mindtoijk(indmj,ixj,iyj,izj)
            ixj = ixj - ixi
            iyj = iyj - iyi
            izj = izj - izi
          endif
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Check for intra and but not in same molecule
!
        if (lintra_only.and..not.lmolok) cycle ljloop
        if (lbtyp.and..not.lmolok) cycle ljloop
        xc2t = xclat(j)
        yc2t = yclat(j)
        zc2t = zclat(j)
        x21t = xc2t - xc1
        y21t = yc2t - yc1
        z21t = zc2t - zc1
!
!  Check r21 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,iimax
          r212 = (xvec1cell(ii)+x21t)**2 + (yvec1cell(ii)+y21t)**2 + (zvec1cell(ii)+z21t)**2
          if (r212.lt.1d-12) cycle iiloop
!
!  Molecule checking
!
          lbonded = .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle iiloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle iiloop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imaxl,jmaxl,kmaxl)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
                lsamemol = (lbonded.or.l2bonds)
              else
                lsamemol = .false.
              endif
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ixx,iyy,izz,ixj,iyj,izj)
              endif
              if (lintra_only.and..not.lsamemol) cycle iiloop
              if (linter_only.and.lsamemol) cycle iiloop
            endif
          endif
!
!  Distance checking
!
          if ((r212.gt.tr1.or.r212.lt.tr1min).and.(.not.lbtyp.or..not.lbonded)) cycle iiloop
          r21 = sqrt(r212)
          x21 = x21t + xvec1cell(ii)
          y21 = y21t + yvec1cell(ii)
          z21 = z21t + zvec1cell(ii)
!
          if (ndim.eq.0) then
            kmax = lj - 1
          else
            kmax = lj
          endif
!************************************
!  Loop over second end site 3 / k  *
!************************************
          lkloop: do lk = 1,kmax
            if (ljbond) then
              k = nuniqueptr(lk)
            else
              k = lk
            endif
            nk = nat(k)
            ntypk = nftype(k)
!
!  Check k is allowed for n
!
            lmatch2 = lmatchanyof2(nk,ntypk,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
            if (.not.lmatch2) cycle lkloop
!
!  Set properties for atom k
!
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
            ock = occuf(k)
            lopk = (lopf(nrelat(k)).or..not.lfreeze)
            lslicek = lsliceatom(nsft + nrelat(k))
            if (lopk) then
              kx = 3*(ioptptr(k) - 1) + 1
              ky = kx + 1
              kz = kx + 2
            endif
!
            if (lmol.and.lneedmol) then
!
!  Molecule handling
!
              nmk = natmol(k)
              if (ndim.gt.0) then
                indmk = nmolind(k)
                call mindtoijk(indmk,ixk,iyk,izk)
                ixk = ixk - ixi
                iyk = iyk - iyi
                izk = izk - izi
              endif
              lmolok = (nmi.eq.nmk.and.nmi.gt.0)
            else
              lmolok = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok) cycle lkloop
            if (lbtyp.and..not.lmolok) cycle lkloop
            xc3t = xclat(k)
            yc3t = yclat(k)
            zc3t = zclat(k)
            x31t = xc3t - xc1
            y31t = yc3t - yc1
            z31t = zc3t - zc1
!
            if (j.eq.k) then
              jjmax = ii - 1
            else
              jjmax = iimax
            endif
!
!  Check r31 is OK
!  Loop over cell vectors
!
            jjloop: do jj = 1,jjmax
              r312 = (xvec1cell(jj)+x31t)**2 + (yvec1cell(jj)+y31t)**2 + (zvec1cell(jj)+z31t)**2
              if (r312.lt.1d-12) cycle jjloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.(jj.eq.iimid)) cycle jjloop
!
!  Prevent atoms j and k being the same atom
!
              if (k.eq.j.and.jj.eq.ii) cycle jjloop
!
!  Molecule checking
!
              lbonded = .false.
              if (lmolok) then
                if (ndim.eq.0) then
                  if (linter_only) cycle jjloop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle jjloop
                  endif
                else
                  call lintoijk(jxx,jyy,jzz,jj,imaxl,jmaxl,kmaxl)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,jxx,jyy,jzz)
                    if (.not.lbonded) cycle jjloop
                    lsamemol = (lbonded.or.l2bonds)
                  else
                    lsamemol = .false.
                  endif
                  if (.not.lsamemol) then
                    call samemol(lsamemol,nmi,jxx,jyy,jzz,ixk,iyk,izk)
                  endif
                  if (lintra_only.and..not.lsamemol) cycle jjloop
                  if (linter_only.and.lsamemol) cycle jjloop
                endif
              endif
!
!  Distance checking
!
              if ((r312.gt.tr2.or.r312.lt.tr2min).and.(.not.lbtyp.or..not.lbonded)) cycle jjloop
              r31 = sqrt(r312)
              x31 = x31t + xvec1cell(jj)
              y31 = y31t + yvec1cell(jj)
              z31 = z31t + zvec1cell(jj)
!
              if (ndim.eq.0) then
                lmax = lk - 1
              else
                lmax = lk 
              endif
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
              l4loop: do l4 = 1,lmax
                if (ljbond) then
                  l = nuniqueptr(l4)
                else
                  l = l4
                endif
                nl = nat(l)
                ntypl = nftype(l)
!
!  Check l is allowed for n
!
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle l4loop
!
!  Set properties for atom l
!
                nregionl = nregionno(nsft+nrelat(l))
                nregiontypl = nregiontype(nregionl,ncf)
                ocl = occuf(l)
                lopl = (lopf(nrelat(l)).or..not.lfreeze)
                if (lopl) then
                  lx = 3*(ioptptr(l) - 1) + 1
                  ly = lx + 1
                  lz = lx + 2
                endif
!
!  If lfreeze=.true. and no atoms have any variables
!  then skip this four body term
!
                if (.not.lopi.and..not.lopj.and..not.lopk.and..not.lopl) cycle l4loop
!     
!  Set region 2 quartet flag
!     
                lreg12    = .false.
                lreg2qtet = .false.
                if (lseok.and.nregions(ncf).gt.1) then
                  lreg2qtet = (nregioni.gt.1.and.nregionj.gt.1.and.nregionk.gt.1.and.nregionl.gt.1)
                  if (.not.lreg2qtet) lreg12 = (nregioni.gt.1.or.nregionj.gt.1.or.nregionk.gt.1.or.nregionl.gt.1)
                endif
                lslicel = lsliceatom(nsft + nrelat(l))
                lattach = .true.
                if (lslicei.and.lslicej.and.lslicek.and.lslicel) lattach = .false.
                if (.not.lslicei.and..not.lslicej.and..not.lslicek.and..not.lslicel) lattach = .false.
!
!  QM/MM handling : i, j, k & l are all QM atoms => exclude
!
                if (QMMMmode(ncf).gt.0) then
                  if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1) cycle l4loop
                endif
!
                if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                  nml = natmol(l)
                  if (ndim.gt.0) then
                    indml = nmolind(l)
                    call mindtoijk(indml,ixl,iyl,izl)
                    ixl = ixl - ixi
                    iyl = iyl - iyi
                    izl = izl - izi
                  endif
                  lmolok = (nmi.eq.nml.and.nmi.gt.0)
                else
                  lmolok = .false.
                endif
!
!  Check for intra and but not in same molecule
!
                if (lintra_only.and..not.lmolok) cycle l4loop
                if (lbtyp.and..not.lmolok) cycle l4loop
                xc4t = xclat(l)
                yc4t = yclat(l)
                zc4t = zclat(l)
                x41t = xc4t - xc1
                y41t = yc4t - yc1
                z41t = zc4t - zc1
!
                if (k.eq.l) then
                  llmax = jj - 1
                else
                  llmax = iimax
                endif
!
!  Check r41 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,llmax
                  r412 = (xvec1cell(ll)+x41t)**2 + (yvec1cell(ll)+y41t)**2 + (zvec1cell(ll)+z41t)**2
                  if (r412.lt.1d-12) cycle llloop
!
!  Prevent atoms i and l being the same atom
!
                  if (l.eq.i.and.ll.eq.iimid) cycle llloop
!
!  Prevent atoms j and l being the same atom
!
                  if (l.eq.j.and.ll.eq.ii) cycle llloop
!
!  Prevent atoms k and l being the same atom
!
                  if (l.eq.k.and.ll.eq.jj) cycle llloop
!
!  Molecule checking
!
                  lbonded = .false.
                  if (lmolok) then
                    if (ndim.eq.0) then
                      if (linter_only) cycle llloop
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,0_i4,0_i4,0_i4)
                        if (.not.lbonded) cycle llloop
                      endif
                    else
                      call lintoijk(kxx,kyy,kzz,ll,imaxl,jmaxl,kmaxl)
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,kxx,kyy,kzz)
                        if (.not.lbonded) cycle llloop
                        lsamemol = (lbonded.or.l2bonds)
                      else
                        lsamemol = .false.
                      endif
                      if (.not.lsamemol) then
                        call samemol(lsamemol,nmi,kxx,kyy,kzz,ixl,iyl,izl)
                      endif
                      if (lintra_only.and..not.lsamemol) cycle llloop
                      if (linter_only.and.lsamemol) cycle llloop
                    endif
                  endif
!
!  Distance checking
!
                  if ((r412.gt.tr3.or.r412.lt.tr3min).and.(.not.lbtyp.or..not.lbonded)) cycle llloop
!
                  x41 = x41t + xvec1cell(ll)
                  y41 = y41t + yvec1cell(ll)
                  z41 = z41t + zvec1cell(ll)
!
!  Calculate other vectors needed
!
                  x32 = x31 - x21
                  y32 = y31 - y21
                  z32 = z31 - z21
                  r322 = x32*x32 + y32*y32 + z32*z32
                  r32 = sqrt(r322)
                  x43 = x41 - x31
                  y43 = y41 - y31
                  z43 = z41 - z31
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Calculate remaining distances
!
                  r41 = sqrt(r412)
                  x42 = x43 + x32
                  y42 = y43 + y32
                  z42 = z43 + z32
                  r422 = x42*x42 + y42*y42 + z42*z42
                  r432 = x43*x43 + y43*y43 + z43*z43
                  r42 = sqrt(r422)
                  r43 = sqrt(r432)
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
!
                  call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko,rn,phi0,isgn,fpoly, &
                    lgrad1,lgrad2,.false.)
                  if (lreg2qtet) then
                    esregion2 = esregion2 + eterm
                  elseif (lreg12) then
                    esregion12 = esregion12 + eterm
                  else
                    eoop = eoop + eterm
                  endif
                  if (lattach) eattach = eattach + eterm
!
                  eterm3rd = eterm/3.0_dp
                  eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm3rd
                  eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + eterm3rd
                  eregion2region(nregionl,nregioni) = eregion2region(nregionl,nregioni) + eterm3rd
!
!  Output energy contribution
!
                  if (lPrintFour) then
                    write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm
                  endif
!*****************************
!  Out of plane derivatives  *
!*****************************
!
!  Set up strain products
!
                  if (lsg1) then
                    call fourstrterms(ndim,rprod,x21,y21,z21,x31,y31,z31,x41,y41,z41,x32,y32,z32, &
                      x42,y42,z42,x43,y43,z43)
                  endif
                  if (lgrad2) then
                    n1x = ix
                    n1y = iy
                    n1z = iz
                    n2x = jx
                    n2y = jy
                    n2z = jz
                    n3x = kx
                    n3y = ky
                    n3z = kz
                    n4x = lx
                    n4y = ly
                    n4z = lz
                    if (lopi.and.lopj.and.lopk.and.lopl) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopi.and.lopj.and.lopk) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = ix
                      n4x2 = jx
                      n4x3 = kx
                    elseif (lopi.and.lopj.and.lopl) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = ix
                      n3x2 = jx
                      n3x4 = lx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopi.and.lopk.and.lopl) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = ix
                      n2x3 = kx
                      n2x4 = lx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopj.and.lopk.and.lopl) then
                      n1x2 = jx
                      n1x3 = kx
                      n1x4 = lx
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopi.and.lopj) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = ix
                      n3x2 = jx
                      n4x1 = ix
                      n4x2 = jx
                    elseif (lopi.and.lopk) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = ix
                      n2x3 = kx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = ix
                      n4x3 = kx
                    elseif (lopi.and.lopl) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = ix
                      n2x4 = lx
                      n3x1 = ix
                      n3x4 = lx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopj.and.lopk) then
                      n1x2 = jx
                      n1x3 = kx
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x2 = jx
                      n4x3 = kx
                    elseif (lopj.and.lopl) then
                      n1x2 = jx
                      n1x4 = lx
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x2 = jx
                      n3x4 = lx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopk.and.lopl) then
                      n1x3 = kx
                      n1x4 = lx
                      n2x3 = kx
                      n2x4 = lx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    elseif (lopi) then
                      n1x2 = ix
                      n1x3 = ix
                      n1x4 = ix
                      n2x1 = ix
                      n3x1 = ix
                      n4x1 = ix
                    elseif (lopj) then
                      n1x2 = jx
                      n2x1 = jx
                      n2x3 = jx
                      n2x4 = jx
                      n3x2 = jx
                      n4x2 = jx
                    elseif (lopk) then
                      n1x3 = kx
                      n2x3 = kx
                      n3x1 = kx
                      n3x2 = kx
                      n3x4 = kx
                      n4x3 = kx
                    elseif (lopl) then
                      n1x4 = lx
                      n2x4 = lx
                      n3x4 = lx
                      n4x1 = lx
                      n4x2 = lx
                      n4x3 = lx
                    endif
                  endif
!***********************
!  Strain derivatives  *
!***********************
                  if (lsg1) then
!
!  First strain derivatives
!
                    rstrdloc(1:nstrains) = 0.0_dp
                    do kl = 1,nstrains
                      rstrdloc(kl) = rstrdloc(kl) + e1d(1)*rprod(kl,1)
                      rstrdloc(kl) = rstrdloc(kl) + e1d(2)*rprod(kl,2)
                      rstrdloc(kl) = rstrdloc(kl) + e1d(3)*rprod(kl,3)
                      rstrdloc(kl) = rstrdloc(kl) + e1d(4)*rprod(kl,4)
                      rstrdloc(kl) = rstrdloc(kl) + e1d(5)*rprod(kl,5)
                      rstrdloc(kl) = rstrdloc(kl) + e1d(6)*rprod(kl,6)
                      rstrd(kl) = rstrd(kl) + rstrdloc(kl)
                    enddo
                    if (latomicstress) then
                      do kl = 1,nstrains
                        atomicstress(kl,i) = atomicstress(kl,i) + 0.25_dp*rstrdloc(kl)
                        atomicstress(kl,j) = atomicstress(kl,j) + 0.25_dp*rstrdloc(kl)
                        atomicstress(kl,k) = atomicstress(kl,k) + 0.25_dp*rstrdloc(kl)
                        atomicstress(kl,l) = atomicstress(kl,l) + 0.25_dp*rstrdloc(kl)
                      enddo
                    endif
!
!  Second strain derivatives
!
!  Strain only
!
                    if (lgrad2.and.lstr) then
                      do kk = 1,nstrains
                        do kl = 1,6
                          temp(kl) = e2d(kb(1,kl))*rprod(kk,1)
                          temp(kl) = temp(kl) + e2d(kb(2,kl))*rprod(kk,2)
                          temp(kl) = temp(kl) + e2d(kb(3,kl))*rprod(kk,3)
                          temp(kl) = temp(kl) + e2d(kb(4,kl))*rprod(kk,4)
                          temp(kl) = temp(kl) + e2d(kb(5,kl))*rprod(kk,5)
                          temp(kl) = temp(kl) + e2d(kb(6,kl))*rprod(kk,6)
                        enddo
!
!  Strain-strain second derivatives
!
                        do kl = 1,nstrains
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,1)*temp(1)
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,2)*temp(2)
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,3)*temp(3)
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,4)*temp(4)
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,5)*temp(5)
                          sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,6)*temp(6)
                        enddo
!
!  Mixed derivatives
!
                        if (lopi) then
                          derv3(n1x,kk) = derv3(n1x,kk) - x21*temp(1) - x31*temp(2) - x41*temp(3)
                          derv3(n1y,kk) = derv3(n1y,kk) - y21*temp(1) - y31*temp(2) - y41*temp(3)
                          derv3(n1z,kk) = derv3(n1z,kk) - z21*temp(1) - z31*temp(2) - z41*temp(3)
                        endif
                        if (lopj) then
                          derv3(n2x,kk) = derv3(n2x,kk) - x32*temp(4) + x21*temp(1) - x42*temp(5)
                          derv3(n2y,kk) = derv3(n2y,kk) - y32*temp(4) + y21*temp(1) - y42*temp(5)
                          derv3(n2z,kk) = derv3(n2z,kk) - z32*temp(4) + z21*temp(1) - z42*temp(5)
                        endif
                        if (lopk) then
                          derv3(n3x,kk) = derv3(n3x,kk) + x32*temp(4) - x43*temp(6) + x31*temp(2)
                          derv3(n3y,kk) = derv3(n3y,kk) + y32*temp(4) - y43*temp(6) + y31*temp(2)
                          derv3(n3z,kk) = derv3(n3z,kk) + z32*temp(4) - z43*temp(6) + z31*temp(2)
                        endif
                        if (lopl) then
                          derv3(n4x,kk) = derv3(n4x,kk) + x43*temp(6) + x42*temp(5) + x41*temp(3)
                          derv3(n4y,kk) = derv3(n4y,kk) + y43*temp(6) + y42*temp(5) + y41*temp(3)
                          derv3(n4z,kk) = derv3(n4z,kk) + z43*temp(6) + z42*temp(5) + z41*temp(3)
                        endif
                      enddo
                    endif
                  endif
!*************************
!  Internal derivatives  *
!*************************
                  if (lgrad1) then
                    xderv(i) = xderv(i) - x21*e1d(1) - x31*e1d(2) - x41*e1d(3)
                    yderv(i) = yderv(i) - y21*e1d(1) - y31*e1d(2) - y41*e1d(3)
                    zderv(i) = zderv(i) - z21*e1d(1) - z31*e1d(2) - z41*e1d(3)
                    xderv(j) = xderv(j) - x32*e1d(4) + x21*e1d(1) - x42*e1d(5)
                    yderv(j) = yderv(j) - y32*e1d(4) + y21*e1d(1) - y42*e1d(5)
                    zderv(j) = zderv(j) - z32*e1d(4) + z21*e1d(1) - z42*e1d(5)
                    xderv(k) = xderv(k) + x32*e1d(4) - x43*e1d(6) + x31*e1d(2)
                    yderv(k) = yderv(k) + y32*e1d(4) - y43*e1d(6) + y31*e1d(2)
                    zderv(k) = zderv(k) + z32*e1d(4) - z43*e1d(6) + z31*e1d(2)
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
                      n112 = n1x2 -1 + kk
                      n113 = n1x3 -1 + kk
                      n114 = n1x4 -1 + kk
                      n211 = n2x1 -1 + kk
                      n213 = n2x3 -1 + kk
                      n214 = n2x4 -1 + kk
                      n311 = n3x1 -1 + kk
                      n312 = n3x2 -1 + kk
                      n314 = n3x4 -1 + kk
                      n411 = n4x1 -1 + kk
                      n412 = n4x2 -1 + kk
                      n413 = n4x3 -1 + kk
!
!  First term
!
                      if (lopi.and.lopj) then
                        derv2(n112,n211) = derv2(n112,n211) - e1d(1)
                        derv2(n211,n112) = derv2(n211,n112) - e1d(1)
                      elseif (lopi.or.lopj) then
                        derv2(n112,n211) = derv2(n112,n211) - e1d(1)
                      endif
                      if (lopi.and.lopk) then
                        derv2(n113,n311) = derv2(n113,n311) - e1d(2)
                        derv2(n311,n113) = derv2(n311,n113) - e1d(2)
                      elseif (lopi.or.lopk) then
                        derv2(n113,n311) = derv2(n113,n311) - e1d(2)
                      endif
                      if (lopi.and.lopl) then
                        derv2(n114,n411) = derv2(n114,n411) - e1d(3)
                        derv2(n411,n114) = derv2(n411,n114) - e1d(3)
                      elseif (lopi.or.lopl) then
                        derv2(n114,n411) = derv2(n114,n411) - e1d(3)
                      endif
                      if (lopj.and.lopk) then
                        derv2(n213,n312) = derv2(n213,n312) - e1d(4)
                        derv2(n312,n213) = derv2(n312,n213) - e1d(4)
                      elseif (lopj.or.lopk) then
                        derv2(n213,n312) = derv2(n213,n312) - e1d(4)
                      endif
                      if (lopj.and.lopl) then
                        derv2(n214,n412) = derv2(n214,n412) - e1d(5)
                        derv2(n412,n214) = derv2(n412,n214) - e1d(5)
                      elseif (lopj.or.lopl) then
                        derv2(n214,n412) = derv2(n214,n412) - e1d(5)
                      endif
                      if (lopk.and.lopl) then
                        derv2(n314,n413) = derv2(n314,n413) - e1d(6)
                        derv2(n413,n314) = derv2(n413,n314) - e1d(6)
                      elseif (lopk.or.lopl) then
                        derv2(n314,n413) = derv2(n314,n413) - e1d(6)
                      endif
!
!  Loop over second coordinate
!
                      do kl = 1,3
                        n221 = n2x1 - 1 + kl
                        n321 = n3x1 - 1 + kl
                        n322 = n3x2 - 1 + kl
                        n421 = n4x1 - 1 + kl
                        n422 = n4x2 - 1 + kl
                        n423 = n4x3 - 1 + kl
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
                        if (lopi.and.lopj) then
                          derv2(n112,n221) = derv2(n112,n221) + t12
                          derv2(n221,n112) = derv2(n221,n112) + t12
                        elseif (lopj) then
                          derv2(n112,n221) = derv2(n112,n221) + t12
                        elseif (lopi) then
                          derv2(n221,n112) = derv2(n221,n112) + t12
                        endif
                        if (lopi.and.lopk) then
                          derv2(n113,n321) = derv2(n113,n321) + t13
                          derv2(n321,n113) = derv2(n321,n113) + t13
                        elseif (lopk) then
                          derv2(n113,n321) = derv2(n113,n321) + t13
                        elseif (lopi) then
                          derv2(n321,n113) = derv2(n321,n113) + t13
                        endif
                        if (lopi.and.lopl) then
                          derv2(n114,n421) = derv2(n114,n421) + t14
                          derv2(n421,n114) = derv2(n421,n114) + t14
                        elseif (lopl) then
                          derv2(n114,n421) = derv2(n114,n421) + t14
                        elseif (lopi) then
                          derv2(n421,n114) = derv2(n421,n114) + t14
                        endif
                        if (lopj.and.lopk) then
                          derv2(n213,n322) = derv2(n213,n322) + t23
                          derv2(n322,n213) = derv2(n322,n213) + t23
                        elseif (lopk) then
                          derv2(n213,n322) = derv2(n213,n322) + t23
                        elseif (lopj) then
                          derv2(n322,n213) = derv2(n322,n213) + t23
                        endif
                        if (lopj.and.lopl) then
                          derv2(n214,n422) = derv2(n214,n422) + t24
                          derv2(n422,n214) = derv2(n422,n214) + t24
                        elseif (lopl) then
                          derv2(n214,n422) = derv2(n214,n422) + t24
                        elseif (lopj) then
                          derv2(n422,n214) = derv2(n422,n214) + t24
                        endif
                        if (lopk.and.lopl) then
                          derv2(n314,n423) = derv2(n314,n423) + t34
                          derv2(n423,n314) = derv2(n423,n314) + t34
                        elseif (lopl) then
                          derv2(n314,n423) = derv2(n314,n423) + t34
                        elseif (lopk) then
                          derv2(n423,n314) = derv2(n423,n314) + t34
                        endif
                      enddo
                    enddo
                  endif
!
!  End of inner loops over atoms and cell vectors
!
                enddo llloop
              enddo l4loop
            enddo jjloop
          enddo lkloop
        enddo iiloop
      enddo ljloop
    enddo liloop
!
!  End of outer loops
!
  enddo pots
!
!  Closing banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Free local memory
!
  deallocate(ioptptr,stat=status)
  if (status/=0) call deallocate_error('fouroop','ioptptr')
  deallocate(nuniqueptr,stat=status)
  if (status/=0) call deallocate_error('fouroop','nuniqueptr')
!
!  All tidying up of derivatives is handled by four so we can just return here
!
  return
  end
