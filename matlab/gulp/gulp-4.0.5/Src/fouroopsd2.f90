  subroutine fouroopsd2(eoop,lgrad1,lgrad2)
!
!  Subroutine for four-body energy from out of plane potentials
!  Symmetry adapted version
!
!  Strategy - sift by potential first, then cutoffs
!
!   3/99 Re-name from fouroopsd.f
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
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
!   7/08 New type checking algorithm introduced
!   7/08 Handling of xangle potential added w.r.t. to type swapping
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Option to output energy terms added
!   4/10 Code modified to increase speed for bonded potentials by only searching over
!        bonded atoms
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
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
  use current
  use derivatives
  use four
  use iochannels,     only : ioout
  use molecule
  use optimisation
  use parallel,       only : ioproc
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
  real(dp),    intent(inout)                   :: eoop
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
  integer(i4), dimension(:), allocatable       :: ioptaptr
  integer(i4), dimension(:), allocatable       :: ioptptr
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixf
  integer(i4)                                  :: iyf
  integer(i4)                                  :: izf
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
  integer(i4)                                  :: jxf
  integer(i4)                                  :: jyf
  integer(i4)                                  :: jzf
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
  integer(i4)                                  :: kxf
  integer(i4)                                  :: kyf
  integer(i4)                                  :: kzf
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
  integer(i4)                                  :: lxf
  integer(i4)                                  :: lyf
  integer(i4)                                  :: lzf
  integer(i4)                                  :: n
  integer(i4)                                  :: n1x
  integer(i4)                                  :: n1y
  integer(i4)                                  :: n1z
  integer(i4)                                  :: n11a
  integer(i4)                                  :: n21a
  integer(i4)                                  :: n31a
  integer(i4)                                  :: n41a
  integer(i4)                                  :: n1xa
  integer(i4)                                  :: n2xa
  integer(i4)                                  :: n3xa
  integer(i4)                                  :: n4xa
  integer(i4)                                  :: n12
  integer(i4)                                  :: n22
  integer(i4)                                  :: n11
  integer(i4)                                  :: n21
  integer(i4)                                  :: n31
  integer(i4)                                  :: n41
  integer(i4)                                  :: n2x
  integer(i4)                                  :: n2y
  integer(i4)                                  :: n2z
  integer(i4)                                  :: n32
  integer(i4)                                  :: n3x
  integer(i4)                                  :: n3y
  integer(i4)                                  :: n3z
  integer(i4)                                  :: n42
  integer(i4)                                  :: n4x
  integer(i4)                                  :: n4y
  integer(i4)                                  :: n4z
  integer(i4)                                  :: n3vec(3,4)
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
  integer(i4)                                  :: nreli
  integer(i4)                                  :: nrelj
  integer(i4)                                  :: nrelk
  integer(i4)                                  :: nrell
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
  logical                                      :: lasu1
  logical                                      :: lasu2
  logical                                      :: lasu3
  logical                                      :: lasu4
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: lintra_only
  logical                                      :: linter_only
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
  logical                                      :: lsamemol
  logical                                      :: lsg1
  logical                                      :: lunique
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
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
!
  lsg1 = (lstr.and.lgrad1)
!
!  Allocate local memory
!
  allocate(nuniqueptr(numat),stat=status)
  if (status/=0) call outofmemory('fouroopmd','nuniqueptr')
  allocate(ioptptr(numat),stat=status)
  if (status/=0) call outofmemory('fouroopsd2','ioptptr')
  allocate(ioptaptr(nasym),stat=status)
  if (status/=0) call outofmemory('fouroopsd2','ioptaptr')
!
!  Initialisation
!
  lu = 0
  do i = 1,nasym
    if (lopf(i).or..not.lfreeze) then
      lu = lu + 1
      ioptaptr(i) = lu
    else
      ioptaptr(i) = 0
    endif
  enddo
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
!
!  Vectors are already set in four before call
!
  if (ndim.eq.3) then
    iimax = 27
    iimid = 14
  else
    iimax = 1
    iimid = 1
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
    ix = -2
    iy = -1
    iz =  0
    ixf = -2
    iyf = -1
    izf =  0
    liloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      oci = occuf(i)
      nreli = nrelat(i)
      lasu1 = (nrel2(nreli).eq.i)
!
!  For energy only call only need terms where lasu1 is true
!
      if (.not.lgrad1.and..not.lasu1) cycle liloop
      neqi = neqv(nreli)
      lopi = (lopf(nrelat(i)).or..not.lfreeze)
      if (lopi) then
        if (lasu1) then
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
        endif
        ixf = ixf + 3
        iyf = iyf + 3
        izf = izf + 3
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
        ocj = occuf(j)
        nrelj = nrelat(j)
        lasu2 = (nrel2(nrelj).eq.j)
        neqj = neqv(nrelj)
        lopj = (lopf(nrelat(j)).or..not.lfreeze)
        if (lopj) then
          if (lasu2) then
            jx = 3*(ioptaptr(nrelj) - 1) + 1
          endif
          jxf = 3*(ioptptr(j) - 1) + 1
          jyf = jxf + 1
          jzf = jxf + 2
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
!
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
          if (r212.lt.1d-10) cycle iiloop
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
          kmax = lj
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
            ock = occuf(k)
            lopk = (lopf(nrelat(k)).or..not.lfreeze)
            nrelk = nrelat(k)
            neqk = neqv(nrelk)
            lasu3 = (nrel2(nrelk).eq.k)
            if (lopk) then
              if (lasu3) then
                kx = 3*(ioptaptr(nrelk) - 1) + 1
              endif
              kxf = 3*(ioptptr(k) - 1) + 1
              kyf = kxf + 1
              kzf = kxf + 2
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
              if (r312.lt.1d-10) cycle jjloop
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
              lmax = lk
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
                ocl = occuf(l)
                lopl = (lopf(nrelat(l)).or..not.lfreeze)
                nrell = nrelat(l)
                neql = neqv(nrell)
                lasu4 = (nrel2(nrell).eq.l)
!
!  If no atom is in the asymmetric unit then skip
!
                if (.not.lasu1.and..not.lasu2.and..not.lasu3.and..not.lasu4) cycle l4loop
!
                if (lopl) then
                  if (lasu4) then
                    lx = 3*(ioptaptr(nrell) - 1) + 1
                  endif
                  lxf = 3*(ioptptr(l) - 1) + 1
                  lyf = lxf + 1
                  lzf = lxf + 2
                endif
!
!  If lfreeze = .true. and no atoms have any variables
!  then skip this four body term
!
                if (.not.lopi.and..not.lopj.and..not.lopk.and..not.lopl) cycle l4loop
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
                  if (r412.lt.1d-10) cycle llloop
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
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Finish calculating distances
!
                  x41 = x41t + xvec1cell(ll)
                  y41 = y41t + yvec1cell(ll)
                  z41 = z41t + zvec1cell(ll)
                  r412 = x41*x41 + y41*y41 + z41*z41
                  r41 = sqrt(r412)
                  x32 = x31-x21
                  y32 = y31-y21
                  z32 = z31-z21
                  r322 = x32*x32 + y32*y32 + z32*z32
                  r32 = sqrt(r322)
                  x43 = x41-x31
                  y43 = y41-y31
                  z43 = z41-z31
                  r432 = x43*x43 + y43*y43 + z43*z43
                  r43 = sqrt(r432)
                  x42 = x43+x32
                  y42 = y43+y32
                  z42 = z43+z32
                  r422 = x42*x42 + y42*y42 + z42*z42
                  r42 = sqrt(r422)
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
                  call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko,rn, &
                    phi0,isgn,fpoly,lgrad1,lgrad2,.false.)
!************************
!  Energy contribution  *
!************************
                  if (lasu1) then
                    eoop = eoop + eterm*dble(neqi)
!
!  Output energy contribution
!
                    if (lPrintFour) then
                      write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm*dble(neqi)
                    endif
                  endif
!*****************************
!  Out of plane derivatives  *
!*****************************
                  if (lsg1) then
!
!  Set up strain products
!
                    rprod(1,1) = x21*x21
                    rprod(1,2) = x31*x31
                    rprod(1,3) = x41*x41
                    rprod(1,4) = x32*x32
                    rprod(1,5) = x42*x42
                    rprod(1,6) = x43*x43
                    rprod(2,1) = y21*y21
                    rprod(2,2) = y31*y31
                    rprod(2,3) = y41*y41
                    rprod(2,4) = y32*y32
                    rprod(2,5) = y42*y42
                    rprod(2,6) = y43*y43
                    rprod(3,1) = z21*z21
                    rprod(3,2) = z31*z31
                    rprod(3,3) = z41*z41
                    rprod(3,4) = z32*z32
                    rprod(3,5) = z42*z42
                    rprod(3,6) = z43*z43
                    rprod(4,1) = y21*z21
                    rprod(4,2) = y31*z31
                    rprod(4,3) = y41*z41
                    rprod(4,4) = y32*z32
                    rprod(4,5) = y42*z42
                    rprod(4,6) = y43*z43
                    rprod(5,1) = x21*z21
                    rprod(5,2) = x31*z31
                    rprod(5,3) = x41*z41
                    rprod(5,4) = x32*z32
                    rprod(5,5) = x42*z42
                    rprod(5,6) = x43*z43
                    rprod(6,1) = x21*y21
                    rprod(6,2) = x31*y31
                    rprod(6,3) = x41*y41
                    rprod(6,4) = x32*y32
                    rprod(6,5) = x42*y42
                    rprod(6,6) = x43*y43
                  endif
                  if (lgrad2) then
                    n1x = ixf
                    n1y = iyf
                    n1z = izf
                    n2x = jxf
                    n2y = jyf
                    n2z = jzf
                    n3x = kxf
                    n3y = kyf
                    n3z = kzf
                    n4x = lxf
                    n4y = lyf
                    n4z = lzf
                    n1xa = ix
                    n2xa = jx
                    n3xa = kx
                    n4xa = lx
                  endif
!***********************
!  Strain derivatives  *
!***********************
                  if (lsg1) then
!
!  First strain derivatives
!
                    if (lasu1) then
                      do kl = 1,6
                        rstrd(kl) = rstrd(kl) + e1d(1)*rprod(kl,1)*dble(neqi)
                        rstrd(kl) = rstrd(kl) + e1d(2)*rprod(kl,2)*dble(neqi)
                        rstrd(kl) = rstrd(kl) + e1d(3)*rprod(kl,3)*dble(neqi)
                        rstrd(kl) = rstrd(kl) + e1d(4)*rprod(kl,4)*dble(neqi)
                        rstrd(kl) = rstrd(kl) + e1d(5)*rprod(kl,5)*dble(neqi)
                        rstrd(kl) = rstrd(kl) + e1d(6)*rprod(kl,6)*dble(neqi)
                      enddo
                    endif
!
!  Second strain derivatives
!
!  Strain only
!
                    if (lgrad2.and.lstr) then
                      do kk = 1,6
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
                        if (lasu1) then
                          do kl = 1,6
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,1)*temp(1)*dble(neqi)
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,2)*temp(2)*dble(neqi)
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,3)*temp(3)*dble(neqi)
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,4)*temp(4)*dble(neqi)
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,5)*temp(5)*dble(neqi)
                            sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,6)*temp(6)*dble(neqi)
                          enddo
!
!  Mixed derivatives
!
                          derv3(n1x,kk) = derv3(n1x,kk) - (x21*temp(1)+x31*temp(2)+x41*temp(3))*dble(neqi)
                          derv3(n1y,kk) = derv3(n1y,kk) - (y21*temp(1)+y31*temp(2)+y41*temp(3))*dble(neqi)
                          derv3(n1z,kk) = derv3(n1z,kk) - (z21*temp(1)+z31*temp(2)+z41*temp(3))*dble(neqi)
                        endif
                        if (lasu2) then
                          derv3(n2x,kk) = derv3(n2x,kk) - (x32*temp(4)-x21*temp(1)+x42*temp(5))*dble(neqj)
                          derv3(n2y,kk) = derv3(n2y,kk) - (y32*temp(4)-y21*temp(1)+y42*temp(5))*dble(neqj)
                          derv3(n2z,kk) = derv3(n2z,kk) - (z32*temp(4)-z21*temp(1)+z42*temp(5))*dble(neqj)
                        endif
                        if (lasu3) then
                          derv3(n3x,kk) = derv3(n3x,kk) + (x32*temp(4)-x43*temp(6)+x31*temp(2))*dble(neqk)
                          derv3(n3y,kk) = derv3(n3y,kk) + (y32*temp(4)-y43*temp(6)+y31*temp(2))*dble(neqk)
                          derv3(n3z,kk) = derv3(n3z,kk) + (z32*temp(4)-z43*temp(6)+z31*temp(2))*dble(neqk)
                        endif
                        if (lasu4) then
                          derv3(n4x,kk) = derv3(n4x,kk) + (x43*temp(6)+x42*temp(5)+x41*temp(3))*dble(neql)
                          derv3(n4y,kk) = derv3(n4y,kk) + (y43*temp(6)+y42*temp(5)+y41*temp(3))*dble(neql)
                          derv3(n4z,kk) = derv3(n4z,kk) + (z43*temp(6)+z42*temp(5)+z41*temp(3))*dble(neql)
                        endif
                      enddo
                    endif
                  endif
!*************************
!  Internal derivatives  *
!*************************
                  if (lgrad1) then
                    if (lasu1) then
                      xdrv(nreli) = xdrv(nreli) - (x21*e1d(1)+x31*e1d(2)+x41*e1d(3))*dble(neqi)
                      ydrv(nreli) = ydrv(nreli) - (y21*e1d(1)+y31*e1d(2)+y41*e1d(3))*dble(neqi)
                      zdrv(nreli) = zdrv(nreli) - (z21*e1d(1)+z31*e1d(2)+z41*e1d(3))*dble(neqi)
                    endif
                    if (lasu2) then
                      xdrv(nrelj) = xdrv(nrelj) - (x32*e1d(4)-x21*e1d(1)+x42*e1d(5))*dble(neqj)
                      ydrv(nrelj) = ydrv(nrelj) - (y32*e1d(4)-y21*e1d(1)+y42*e1d(5))*dble(neqj)
                      zdrv(nrelj) = zdrv(nrelj) - (z32*e1d(4)-z21*e1d(1)+z42*e1d(5))*dble(neqj)
                    endif
                    if (lasu3) then
                      xdrv(nrelk) = xdrv(nrelk) + (x32*e1d(4)-x43*e1d(6)+x31*e1d(2))*dble(neqk)
                      ydrv(nrelk) = ydrv(nrelk) + (y32*e1d(4)-y43*e1d(6)+y31*e1d(2))*dble(neqk)
                      zdrv(nrelk) = zdrv(nrelk) + (z32*e1d(4)-z43*e1d(6)+z31*e1d(2))*dble(neqk)
                    endif
                    if (lasu4) then
                      xdrv(nrell) = xdrv(nrell) + (x43*e1d(6)+x42*e1d(5)+x41*e1d(3))*dble(neql)
                      ydrv(nrell) = ydrv(nrell) + (y43*e1d(6)+y42*e1d(5)+y41*e1d(3))*dble(neql)
                      zdrv(nrell) = zdrv(nrell) + (z43*e1d(6)+z42*e1d(5)+z41*e1d(3))*dble(neql)
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
                      n11a = n1xa - 1 + kk
                      n21a = n2xa - 1 + kk
                      n31a = n3xa - 1 + kk
                      n41a = n4xa - 1 + kk
!
!  First term
!
                      if (lasu1.and.lopi) then
                        if (lopj) then
                          derv2(n21,n11a) = derv2(n21,n11a) - e1d(1)*dble(neqi)
                        else
                          derv2(n11,n11a) = derv2(n11,n11a) - e1d(1)*dble(neqi)
                        endif
                        if (lopk) then
                          derv2(n31,n11a) = derv2(n31,n11a) - e1d(2)*dble(neqi)
                        else
                          derv2(n11,n11a) = derv2(n11,n11a) - e1d(2)*dble(neqi)
                        endif
                        if (lopl) then
                          derv2(n41,n11a) = derv2(n41,n11a) - e1d(3)*dble(neqi)
                        else
                          derv2(n11,n11a) = derv2(n11,n11a) - e1d(3)*dble(neqi)
                        endif
                      endif
                      if (lasu2.and.lopj) then
                        if (lopi) then
                          derv2(n11,n21a) = derv2(n11,n21a) - e1d(1)*dble(neqj)
                        else
                          derv2(n21,n21a) = derv2(n21,n21a) - e1d(1)*dble(neqj)
                        endif
                        if (lopk) then
                          derv2(n31,n21a) = derv2(n31,n21a) - e1d(4)*dble(neqj)
                        else
                          derv2(n21,n21a) = derv2(n21,n21a) - e1d(4)*dble(neqj)
                        endif
                        if (lopl) then
                          derv2(n41,n21a) = derv2(n41,n21a) - e1d(5)*dble(neqj)
                        else
                          derv2(n21,n21a) = derv2(n21,n21a) - e1d(5)*dble(neqj)
                        endif
                      endif
                      if (lasu3.and.lopk) then
                        if (lopi) then
                          derv2(n11,n31a) = derv2(n11,n31a) - e1d(2)*dble(neqk)
                        else
                          derv2(n31,n31a) = derv2(n31,n31a) - e1d(2)*dble(neqk)
                        endif
                        if (lopj) then
                          derv2(n21,n31a) = derv2(n21,n31a) - e1d(4)*dble(neqk)
                        else
                          derv2(n31,n31a) = derv2(n31,n31a) - e1d(4)*dble(neqk)
                        endif
                        if (lopl) then
                          derv2(n41,n31a) = derv2(n41,n31a) - e1d(6)*dble(neqk)
                        else
                          derv2(n31,n31a) = derv2(n31,n31a) - e1d(6)*dble(neqk)
                        endif
                      endif
                      if (lasu4.and.lopl) then
                        if (lopi) then
                          derv2(n11,n41a) = derv2(n11,n41a) - e1d(3)*dble(neql)
                        else
                          derv2(n41,n41a) = derv2(n41,n41a) - e1d(3)*dble(neql)
                        endif
                        if (lopj) then
                          derv2(n21,n41a) = derv2(n21,n41a) - e1d(5)*dble(neql)
                        else 
                          derv2(n41,n41a) = derv2(n41,n41a) - e1d(5)*dble(neql)
                        endif
                        if (lopk) then
                          derv2(n31,n41a) = derv2(n31,n41a) - e1d(6)*dble(neql)
                        else
                          derv2(n41,n41a) = derv2(n41,n41a) - e1d(6)*dble(neql)
                        endif
                      endif
!
!  Loop over second coordinate
!
                      do kl = 1,3
                        n12 = n1x - 1 + kl
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
                        if (lasu1.and.lopi) then
                          if (lopj) then
                            derv2(n22,n11a) = derv2(n22,n11a) + t12*dble(neqi)
                          else
                            derv2(n12,n11a) = derv2(n12,n11a) + t12*dble(neqi)
                          endif
                          if (lopk) then
                            derv2(n32,n11a) = derv2(n32,n11a) + t13*dble(neqi)
                          else
                            derv2(n12,n11a) = derv2(n12,n11a) + t13*dble(neqi)
                          endif
                          if (lopl) then
                            derv2(n42,n11a) = derv2(n42,n11a) + t14*dble(neqi)
                          else
                            derv2(n12,n11a) = derv2(n12,n11a) + t14*dble(neqi)
                          endif
                        endif
                        if (lasu2.and.lopj) then
                          if (lopi) then
                            derv2(n11,n21a) = derv2(n11,n21a) + t12*dble(neqj)
                          else
                            derv2(n21,n21a) = derv2(n21,n21a) + t12*dble(neqj)
                          endif
                          if (lopk) then
                            derv2(n32,n21a) = derv2(n32,n21a) + t23*dble(neqj)
                          else
                            derv2(n22,n21a) = derv2(n22,n21a) + t23*dble(neqj)
                          endif
                          if (lopl) then
                            derv2(n42,n21a) = derv2(n42,n21a) + t24*dble(neqj)
                          else
                            derv2(n22,n21a) = derv2(n22,n21a) + t24*dble(neqj)
                          endif
                        endif
                        if (lasu3.and.lopk) then
                          if (lopi) then
                            derv2(n11,n31a) = derv2(n11,n31a) + t13*dble(neqk)
                          else
                            derv2(n31,n31a) = derv2(n31,n31a) + t13*dble(neqk)
                          endif 
                          if (lopj) then
                            derv2(n22,n31a) = derv2(n22,n31a) + t23*dble(neqk)
                          else
                            derv2(n32,n31a) = derv2(n32,n31a) + t23*dble(neqk)
                          endif
                          if (lopl) then
                            derv2(n42,n31a) = derv2(n42,n31a) + t34*dble(neqk)
                          else
                            derv2(n32,n31a) = derv2(n32,n31a) + t34*dble(neqk)
                          endif
                        endif
                        if (lasu4.and.lopl) then
                          if (lopi) then
                            derv2(n11,n41a) = derv2(n11,n41a) + t14*dble(neql)
                          else
                            derv2(n41,n41a) = derv2(n41,n41a) + t14*dble(neql)
                          endif 
                          if (lopj) then
                            derv2(n22,n41a) = derv2(n22,n41a) + t24*dble(neql)
                          else
                            derv2(n42,n41a) = derv2(n42,n41a) + t24*dble(neql)
                          endif
                          if (lopk) then
                            derv2(n32,n41a) = derv2(n32,n41a) + t34*dble(neql)
                          else
                            derv2(n42,n41a) = derv2(n42,n41a) + t34*dble(neql)
                          endif
                        endif
!
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
  deallocate(ioptaptr,stat=status)
  if (status/=0) call deallocate_error('fouroopsd2','ioptaptr')
  deallocate(ioptptr,stat=status)
  if (status/=0) call deallocate_error('fouroopsd2','ioptptr')
  deallocate(nuniqueptr,stat=status)
  if (status/=0) call deallocate_error('fouroopsd2','nuniqueptr')
!
!  All tidying up of derivatives is handled by four so we can just return here
!
  return
  end
