  subroutine sixnos(esix,esregion12,esregion2,eattach,lgrad1,lgrad2)
!
!  Subroutine for six-body potentials
!
!   1/06 Created based on fouroop.f
!   7/06 Second derivatives completed
!   2/07 Bonding types and test added
!   5/07 QM/MM schemes added
!  12/07 Unused variables removed
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
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
  use mdlogic
  use molecule
  use numbers,        only : sixth
  use optimisation
  use six
  use symmetry
  use times,          only : tsix
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout)                      :: esix
  real(dp), intent(inout)                      :: esregion12
  real(dp), intent(inout)                      :: esregion2
  real(dp), intent(inout)                      :: eattach
  logical,  intent(in)                         :: lgrad1
  logical,  intent(in)                         :: lgrad2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ia
  integer(i4)                                  :: ialp
  integer(i4)                                  :: ia1
  integer(i4)                                  :: ia2
  integer(i4)                                  :: ib
  integer(i4)                                  :: ibet
  integer(i4)                                  :: ib1
  integer(i4)                                  :: ib4
  integer(i4)                                  :: ii
  integer(i4)                                  :: iiimax
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: indmm
  integer(i4)                                  :: indmn
  integer(i4)                                  :: indvec
  integer(i4)                                  :: isatom(6)
  integer(i4)                                  :: isxyz(3,6)
  integer(i4)                                  :: ivec
  integer(i4)                                  :: iveca(2,15)
  integer(i4)                                  :: ixm
  integer(i4)                                  :: ixn
  integer(i4)                                  :: iym
  integer(i4)                                  :: iyn
  integer(i4)                                  :: izm
  integer(i4)                                  :: izn
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
  integer(i4)                                  :: ja
  integer(i4)                                  :: ja1
  integer(i4)                                  :: ja3
  integer(i4)                                  :: ja4
  integer(i4)                                  :: jb
  integer(i4)                                  :: jb2
  integer(i4)                                  :: jb3
  integer(i4)                                  :: jj
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jmin
  integer(i4)                                  :: jvec
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kb3(15,15)
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: liimax
  integer(i4)                                  :: ll
  integer(i4)                                  :: lmax
  integer(i4)                                  :: lmin
  integer(i4)                                  :: m
  integer(i4)                                  :: mm
  integer(i4)                                  :: mn
  integer(i4)                                  :: mxx
  integer(i4)                                  :: myy
  integer(i4)                                  :: mzz
  integer(i4)                                  :: n
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypejm
  integer(i4)                                  :: nbtypejn
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: nbtypejm2
  integer(i4)                                  :: nbtypejn2
  integer(i4)                                  :: neq
  integer(i4)                                  :: nff
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: nsixtype
  integer(i4)                                  :: ni
  integer(i4)                                  :: niimax
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nm 
  integer(i4)                                  :: nn 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nmm 
  integer(i4)                                  :: nmn 
  integer(i4)                                  :: nmax
  integer(i4)                                  :: nmin
  integer(i4)                                  :: np
  integer(i4)                                  :: nr
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregionm
  integer(i4)                                  :: nregionn
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nregiontypm
  integer(i4)                                  :: nregiontypn
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: nt5
  integer(i4)                                  :: nt6
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntyp5
  integer(i4)                                  :: ntyp6
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: ntypm
  integer(i4)                                  :: ntypn
  integer(i4)                                  :: nxx
  integer(i4)                                  :: nyy
  integer(i4)                                  :: nzz
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: lattach
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopis(6)
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lopk
  logical                                      :: lopl
  logical                                      :: lopm
  logical                                      :: lopn
  logical                                      :: lopone
  logical                                      :: lreg12
  logical                                      :: lreg2stet
  logical                                      :: lsamemol
  logical                                      :: lsg1
  logical                                      :: lslicei
  logical                                      :: lslicej
  logical                                      :: lslicek
  logical                                      :: lslicel
  logical                                      :: lslicem
  logical                                      :: lslicen
  logical                                      :: ltsym12
  logical                                      :: ltsym34
  logical                                      :: ltsym56
  real(dp)                                     :: cputime
  real(dp)                                     :: d2trm
  real(dp)                                     :: e1d(15)
  real(dp)                                     :: e2d(120)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: eterm
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ocm 
  real(dp)                                     :: ocn 
  real(dp)                                     :: ofct 
  real(dp)                                     :: r212
  real(dp)                                     :: r312
  real(dp)                                     :: r412
  real(dp)                                     :: r522
  real(dp)                                     :: r622
  real(dp)                                     :: rksix
  real(dp)                                     :: rko
  real(dp)                                     :: rprod(6,15)
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: temp(15)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr4
  real(dp)                                     :: tr5
  real(dp)                                     :: ttr2
  real(dp)                                     :: ttr3
  real(dp)                                     :: ttr4
  real(dp)                                     :: ttr5
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
  real(dp)                                     :: x52
  real(dp)                                     :: y52
  real(dp)                                     :: z52
  real(dp)                                     :: x52t
  real(dp)                                     :: y52t
  real(dp)                                     :: z52t
  real(dp)                                     :: x62
  real(dp)                                     :: y62
  real(dp)                                     :: z62
  real(dp)                                     :: x62t
  real(dp)                                     :: y62t
  real(dp)                                     :: z62t
  real(dp)                                     :: xc2t
  real(dp)                                     :: yc2t
  real(dp)                                     :: zc2t
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
  real(dp)                                     :: xc5t
  real(dp)                                     :: yc5t
  real(dp)                                     :: zc5t
  real(dp)                                     :: xc6t
  real(dp)                                     :: yc6t
  real(dp)                                     :: zc6t
  real(dp)                                     :: sdist(15)
  real(dp)                                     :: svec(3,6,6)
  real(dp)                                     :: sxyz(3,6)
  real(dp), dimension(:), allocatable          :: xderv
  real(dp), dimension(:), allocatable          :: yderv
  real(dp), dimension(:), allocatable          :: zderv
!
  time1 = cputime()
  lsg1 = (lstr.and.lgrad1)
!     
!  Allocate local memory                           
!
  allocate(nptr(numat),stat=status)
  if (status/=0) call outofmemory('sixnos','nptr')
  allocate(xderv(numat),stat=status)                       
  if (status/=0) call outofmemory('sixnos','xderv')
  allocate(yderv(numat),stat=status)                       
  if (status/=0) call outofmemory('sixnos','yderv')
  allocate(zderv(numat),stat=status)
  if (status/=0) call outofmemory('sixnos','zderv')
!
!  Set up pointer
!
  if (lfreeze) then
    nff = 0
    do i = 1,numat
      if (lopf(nrelat(i))) then
        nff = nff + 1
        nptr(i) = nff
      else
        nptr(i) = 0
      endif
    enddo
  else
    do i = 1,numat
      nptr(i) = i
    enddo
  endif
!
!  Set up vector to atom mapping
!
  indvec = 0
  do ivec = 1,5
    do jvec = ivec+1,6
      indvec = indvec + 1
      iveca(1,indvec) = jvec
      iveca(2,indvec) = ivec
    enddo
  enddo
!
!  Initialise kb3
!
  if (lgrad2) then
    indvec = 0
    do ivec = 1,15
      do jvec = ivec,15
        indvec = indvec + 1
        kb3(ivec,jvec) = indvec
        kb3(jvec,ivec) = indvec
      enddo
    enddo
  endif
!
!  Zero derivatives
!
  do i = 1,numat
    xderv(i) = 0.0_dp
    yderv(i) = 0.0_dp
    zderv(i) = 0.0_dp
  enddo
!*************************
!  Loop over potentials  *
!*************************
  do np = 1,nsix
    nsixtype = nsixty(np)
    tr1 = six1(np)**2
    ttr2 = six2(np)**2
    ttr3 = six3(np)**2
    ttr4 = six4(np)**2
    ttr5 = six5(np)**2
    ltsym12 = (lmatch(nsspec1(np),nsptyp1(np),nsspec2(np),nsptyp2(np),.true.).or. &
               lmatch(nsspec2(np),nsptyp2(np),nsspec1(np),nsptyp1(np),.true.))
    lbtyp = (mmsexc(np).eq.1)
    rksix = sixk(np)
    lintra_only = (lsintra(np).and..not.lsinter(np))
    linter_only = (lsinter(np).and..not.lsintra(np))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!********************************
!  Loop over middle site 1 / i  *
!********************************
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
      oci = occuf(i)
      lopi = (lopf(nrelat(i)).or..not.lfreeze)
      lslicei = lsliceatom(nsft + nrelat(i))
!
!  Check i is allowed for n
!
      if (lmatch(ni,ntypi,nsspec1(np),nsptyp1(np),.true.)) then
        nt2 = nsspec2(np)
        ntyp2 = nsptyp2(np)
        nt3 = nsspec3(np)
        nt4 = nsspec4(np)
        nt5 = nsspec5(np)
        nt6 = nsspec6(np)
        ntyp3 = nsptyp3(np)
        ntyp4 = nsptyp4(np)
        ntyp5 = nsptyp5(np)
        ntyp6 = nsptyp6(np)
        tr2 = ttr2
        tr3 = ttr3
        tr4 = ttr4
        tr5 = ttr5
      elseif (lmatch(ni,ntypi,nsspec2(np),nsptyp2(np),.true.)) then
        nt2 = nsspec1(np)
        ntyp2 = nsptyp1(np)
        nt3 = nsspec5(np)
        nt4 = nsspec6(np)
        nt5 = nsspec3(np)
        nt6 = nsspec4(np)
        ntyp3 = nsptyp5(np)
        ntyp4 = nsptyp6(np)
        ntyp5 = nsptyp3(np)
        ntyp6 = nsptyp4(np)
        tr2 = ttr4
        tr3 = ttr5
        tr4 = ttr2
        tr5 = ttr3
      else
        cycle iloop
      endif
!
!  Set flags as to whether potentials are symmetric at the two ends
!
      ltsym34 = (lmatch(nt3,ntyp3,nt4,ntyp4,.true.).or.lmatch(nt4,ntyp4,nt3,ntyp3,.true.))
      ltsym56 = (lmatch(nt5,ntyp5,nt6,ntyp6,.true.).or.lmatch(nt6,ntyp6,nt5,ntyp5,.true.))
!
!  i has been accepted
!
      isatom(1) = i
      isxyz(1,1) = 3*(nptr(i) - 1) + 1
      isxyz(2,1) = 3*(nptr(i) - 1) + 2
      isxyz(3,1) = 3*(nptr(i) - 1) + 3
      lopis(1)  = lopi
      sxyz(1,1) = xclat(i)
      sxyz(2,1) = yclat(i)
      sxyz(3,1) = zclat(i)
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
!
!  Set j looping indices
!
      if (ltsym12) then
        jmin = 1
        jmax = i
      else
        jmin = 1
        jmax = numat
      endif
!********************************
!  Loop over middle site 2 / j  *
!********************************
      jloop: do j = jmin,jmax
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
        ocj = occuf(j)
        lopj = (lopf(nrelat(j)).or..not.lfreeze)
        lslicej = lsliceatom(nsft + nrelat(j))
!
!  Check j is allowed for n
!
        if (.not.lmatch(nj,ntypj,nt2,ntyp2,.true.)) cycle jloop
!
!  QM/MM handling : i & j are both QM atoms and potential is of bonded type => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.lbtyp) cycle jloop
        endif
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
        if (lintra_only.and..not.lmolok) cycle jloop
        if (lbtyp.and..not.lmolok) cycle jloop
        xc2t = xclat(j)
        yc2t = yclat(j)
        zc2t = zclat(j)
        x21t = xc2t - sxyz(1,1)
        y21t = yc2t - sxyz(2,1)
        z21t = zc2t - sxyz(3,1)
        if (ltsym12.and.i.eq.j) then
          iiimax = iimid - 1
        else
          iiimax = iimax
        endif
!
!  Check r21 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,iiimax
          x21 = x21t + xvec1cell(ii)
          y21 = y21t + yvec1cell(ii)
          z21 = z21t + zvec1cell(ii)
          r212 = x21*x21 + y21*y21 + z21*z21
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
!
!  Check central bond type for correct order
!
                if (n6botype(1,np).gt.0) then
                  if (n6botype(1,np).ne.nbtypeij) cycle iiloop
                endif
                if (n6botype(2,np).gt.0) then
                  if (n6botype(2,np).ne.nbtypeij2) cycle iiloop
                endif
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imaxl,jmaxl,kmaxl)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
!
!  Check central bond type for correct order
!
                if (n6botype(1,np).gt.0) then
                  if (n6botype(1,np).ne.nbtypeij) cycle iiloop
                endif
                if (n6botype(2,np).gt.0) then
                  if (n6botype(2,np).ne.nbtypeij2) cycle iiloop
                endif
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
          if (r212.gt.tr1.and.(.not.lbtyp.or..not.lbonded)) cycle iiloop
!
!  j has been accepted
!
          isatom(2) = j
          isxyz(1,2) = 3*(nptr(j) - 1) + 1
          isxyz(2,2) = 3*(nptr(j) - 1) + 2
          isxyz(3,2) = 3*(nptr(j) - 1) + 3
          lopis(2)  = lopj
          sxyz(1,2) = x21 + sxyz(1,1)
          sxyz(2,2) = y21 + sxyz(2,1)
          sxyz(3,2) = z21 + sxyz(3,1)
!*****************************************
!  Loop over first end site bonded to i  *
!*****************************************
          kloop: do k = 1,numat
            nk = nat(k)
            ntypk = nftype(k)
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
            ock = occuf(k)
            lopk = (lopf(nrelat(k)).or..not.lfreeze)
            lslicek = lsliceatom(nsft + nrelat(k))
!
!  Check k is allowed for n
!
            if (.not.lmatch(nk,ntypk,nt3,ntyp3,.true.)) cycle kloop
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
            if (lintra_only.and..not.lmolok) cycle kloop
            if (lbtyp.and..not.lmolok) cycle kloop
            xc3t = xclat(k)
            yc3t = yclat(k)
            zc3t = zclat(k)
            x31t = xc3t - sxyz(1,1)
            y31t = yc3t - sxyz(2,1)
            z31t = zc3t - sxyz(3,1)
!
!  Check r31 is OK
!  Loop over cell vectors
!
            jjloop: do jj = 1,iimax
              x31 = x31t + xvec1cell(jj)
              y31 = y31t + yvec1cell(jj)
              z31 = z31t + zvec1cell(jj)
              r312 = x31*x31 + y31*y31 + z31*z31
              if (r312.lt.1d-12) cycle jjloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.jj.eq.iimid) cycle jjloop
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
              if (r312.gt.tr2.and.(.not.lbtyp.or..not.lbonded)) cycle jjloop
!
!  k has been accepted
!
              isatom(3) = k
              isxyz(1,3) = 3*(nptr(k) - 1) + 1
              isxyz(2,3) = 3*(nptr(k) - 1) + 2
              isxyz(3,3) = 3*(nptr(k) - 1) + 3
              lopis(3)  = lopk
              sxyz(1,3) = x31 + sxyz(1,1)
              sxyz(2,3) = y31 + sxyz(2,1)
              sxyz(3,3) = z31 + sxyz(3,1)
!************************************
!  Loop over second end site for i  *
!************************************
!
!  Set l looping indices
!
              if (ltsym34) then
                lmin = 1
                lmax = k
              else
                lmin = 1
                lmax = numat
              endif
              lloop: do l = lmin,lmax
                nl = nat(l)
                ntypl = nftype(l)
                nregionl = nregionno(nsft+nrelat(l))
                nregiontypl = nregiontype(nregionl,ncf)
                ocl = occuf(l)
                lopl = (lopf(nrelat(l)).or..not.lfreeze)
!
!  Check l is allowed for n
!
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  Molecule handling
!
                if (lmol.and.lneedmol) then
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
                if (lintra_only.and..not.lmolok) cycle lloop
                if (lbtyp.and..not.lmolok) cycle lloop
                xc4t = xclat(l)
                yc4t = yclat(l)
                zc4t = zclat(l)
                x41t = xc4t - sxyz(1,1)
                y41t = yc4t - sxyz(2,1)
                z41t = zc4t - sxyz(3,1)
                if (ltsym34.and.k.eq.l) then
                  liimax = iimid - 1
                else
                  liimax = iimax
                endif
!
!  Check r41 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,liimax
                  x41 = x41t + xvec1cell(ll)
                  y41 = y41t + yvec1cell(ll)
                  z41 = z41t + zvec1cell(ll)
                  r412 = x41*x41 + y41*y41 + z41*z41
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
                  if (r412.gt.tr3.and.(.not.lbtyp.or..not.lbonded)) cycle llloop
!
!  l has been accepted
!
                  isatom(4) = l
                  isxyz(1,4) = 3*(nptr(l) - 1) + 1
                  isxyz(2,4) = 3*(nptr(l) - 1) + 2
                  isxyz(3,4) = 3*(nptr(l) - 1) + 3
                  lopis(4)  = lopl
                  sxyz(1,4) = x41 + sxyz(1,1)
                  sxyz(2,4) = y41 + sxyz(2,1)
                  sxyz(3,4) = z41 + sxyz(3,1)
!*****************************************
!  Loop over first end site bonded to j  *
!*****************************************
                  mloop: do m = 1,numat
                    nm = nat(m)
                    ntypm = nftype(m)
                    nregionm = nregionno(nsft+nrelat(m))
                    nregiontypm = nregiontype(nregionm,ncf)
                    ocm = occuf(m)
                    lopm = (lopf(nrelat(m)).or..not.lfreeze)
                    lslicem = lsliceatom(nsft + nrelat(m))
!
!  Check m is allowed for n
!
                    if (.not.lmatch(nm,ntypm,nt5,ntyp5,.true.)) cycle mloop
                    if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                      nmm = natmol(m)
                      if (ndim.gt.0) then
                        indmm = nmolind(m)
                        call mindtoijk(indmm,ixm,iym,izm)
                        ixm = ixm - ixj
                        iym = iym - iyj
                        izm = izm - izj
                      endif
                      lmolok = (nmi.eq.nmm.and.nmi.gt.0)
                    else
                      lmolok = .false.
                    endif
!
!  Check for intra and but not in same molecule
!
                    if (lintra_only.and..not.lmolok) cycle mloop
                    if (lbtyp.and..not.lmolok) cycle mloop
                    xc5t = xclat(m)
                    yc5t = yclat(m)
                    zc5t = zclat(m)
                    x52t = xc5t - sxyz(1,2)
                    y52t = yc5t - sxyz(2,2)
                    z52t = zc5t - sxyz(3,2)
!
!  Check r52 is OK
!  Loop over cell vectors
!
                    mmloop: do mm = 1,iimax
                      x52 = x52t + xvec1cell(mm)
                      y52 = y52t + yvec1cell(mm)
                      z52 = z52t + zvec1cell(mm)
                      r522 = x52*x52 + y52*y52 + z52*z52
                      if (r522.lt.1d-12) cycle mmloop
!
!  Prevent atoms i and m being the same atom
!
                      if (m.eq.i.and.mm.eq.iimid) cycle mmloop
!
!  Prevent atoms j and m being the same atom
!
                      if (m.eq.j.and.mm.eq.ii) cycle mmloop
!
!  Molecule checking
!
                      lbonded = .false.
                      if (lmolok) then
                        if (ndim.eq.0) then
                          if (linter_only) cycle mmloop
                          if (lbtyp) then
                            call bonded(lbonded,l2bonds,nbtypejm,nbtypejm2,j,m,0_i4,0_i4,0_i4)
                            if (.not.lbonded) cycle mmloop
                          endif
                        else
                          call lintoijk(mxx,myy,mzz,mm,imaxl,jmaxl,kmaxl)
                          if (lbtyp) then
                            call bonded(lbonded,l2bonds,nbtypejm,nbtypejm2,j,m,mxx-ixx,myy-iyy,mzz-izz)
                            if (.not.lbonded) cycle mmloop
                            lsamemol = (lbonded.or.l2bonds)
                          else
                            lsamemol = .false.
                          endif
                          if (.not.lsamemol) then
                            call samemol(lsamemol,nmi,jxx,jyy,jzz,ixm,iym,izm)
                          endif
                          if (lintra_only.and..not.lsamemol) cycle mmloop
                          if (linter_only.and.lsamemol) cycle mmloop
                        endif
                      endif
!
!  Distance checking
!
                      if (r522.gt.tr4.and.(.not.lbtyp.or..not.lbonded)) cycle mmloop
!
!  m has been accepted
!
                      isatom(5) = m
                      isxyz(1,5) = 3*(nptr(m) - 1) + 1
                      isxyz(2,5) = 3*(nptr(m) - 1) + 2
                      isxyz(3,5) = 3*(nptr(m) - 1) + 3
                      lopis(5)  = lopm
                      sxyz(1,5) = x52 + sxyz(1,2)
                      sxyz(2,5) = y52 + sxyz(2,2)
                      sxyz(3,5) = z52 + sxyz(3,2)
!************************************
!  Loop over second end site for j  *
!************************************
!
!  Set n looping indices
!
                      if (ltsym56) then
                        nmin = 1
                        nmax = m
                      else
                        nmin = 1
                        nmax = numat
                      endif
                      nloop: do n = nmin,nmax
                        nn = nat(n)
                        ntypn = nftype(n)
                        nregionn = nregionno(nsft+nrelat(n))
                        nregiontypn = nregiontype(nregionn,ncf)
                        ocn = occuf(n)
                        lopn = (lopf(nrelat(n)).or..not.lfreeze)
                        lslicen = lsliceatom(nsft + nrelat(n))
!
!  Check n is allowed for n
!
                        if (.not.lmatch(nn,ntypn,nt6,ntyp6,.true.)) cycle nloop
!
!  If lfreeze=.true. and no atoms have any variables then skip this four body term
!
                        if (.not.lopi.and..not.lopj.and..not.lopk.and..not.lopl.and..not.lopm.and..not.lopn) cycle nloop
!
!  QM/MM handling : i, j, k, l, m and n are all QM atoms => exclude
!
                        if (QMMMmode(ncf).gt.0) then
                          if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1.and. &
                              nregiontypm.eq.1.and.nregiontypn.eq.1) cycle nloop
                        endif
!
!  Molecule handling
!
                        if (lmol.and.lneedmol) then
                          nmn = natmol(n)
                          if (ndim.gt.0) then
                            indmn = nmolind(n)
                            call mindtoijk(indmn,ixn,iyn,izn)
                            ixn = ixn - ixj
                            iyn = iyn - iyj
                            izn = izn - izj
                          endif
                          lmolok = (nmi.eq.nmn.and.nmi.gt.0)
                        else
                          lmolok = .false.
                        endif
!
!  Check for intra and but not in same molecule
!
                        if (lintra_only.and..not.lmolok) cycle nloop
                        if (lbtyp.and..not.lmolok) cycle nloop
                        xc6t = xclat(n)
                        yc6t = yclat(n)
                        zc6t = zclat(n)
                        x62t = xc6t - sxyz(1,2)
                        y62t = yc6t - sxyz(2,2)
                        z62t = zc6t - sxyz(3,2)
                        if (ltsym56.and.m.eq.n) then
                          niimax = iimid - 1
                        else
                          niimax = iimax
                        endif
!
!  Check r62 is OK
!  Loop over cell vectors
!
                        mnloop: do mn = 1,niimax
                          x62 = x62t + xvec1cell(mn)
                          y62 = y62t + yvec1cell(mn)
                          z62 = z62t + zvec1cell(mn)
                          r622 = x62*x62 + y62*y62 + z62*z62
                          if (r622.lt.1d-12) cycle mnloop
!
!  Prevent atoms i and n being the same atom
!
                          if (n.eq.i.and.mn.eq.iimid) cycle mnloop
!
!  Prevent atoms j and n being the same atom
!
                          if (n.eq.j.and.mn.eq.ii) cycle mnloop
!
!  Prevent atoms m and n being the same atom
!
                          if (n.eq.m.and.mn.eq.mm) cycle mnloop
!
!  Molecule checking
!
                          lbonded = .false.
                          if (lmolok) then
                            if (ndim.eq.0) then
                              if (linter_only) cycle mnloop
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypejn,nbtypejn2,j,n,0_i4,0_i4,0_i4)
                                if (.not.lbonded) cycle mnloop
                              endif
                            else
                              call lintoijk(nxx,nyy,nzz,mn,imaxl,jmaxl,kmaxl)
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypejn,nbtypejn2,j,n,nxx-ixx,nyy-iyy,nzz-izz)
                                if (.not.lbonded) cycle mnloop
                                lsamemol = (lbonded.or.l2bonds)
                              else
                                lsamemol = .false.
                              endif
                              if (.not.lsamemol) then
                                call samemol(lsamemol,nmi,jxx,jyy,jzz,ixn,iyn,izn)
                              endif
                              if (lintra_only.and..not.lsamemol) cycle mnloop
                              if (linter_only.and.lsamemol) cycle mnloop
                            endif
                          endif
!
!  Distance checking
!
                          if (r622.gt.tr5.and.(.not.lbtyp.or..not.lbonded)) cycle mnloop
!
!  n has been accepted
!
                          isatom(6) = n
                          isxyz(1,6) = 3*(nptr(n) - 1) + 1
                          isxyz(2,6) = 3*(nptr(n) - 1) + 2
                          isxyz(3,6) = 3*(nptr(n) - 1) + 3
                          lopis(6)  = lopn
                          sxyz(1,6) = x62 + sxyz(1,2)
                          sxyz(2,6) = y62 + sxyz(2,2)
                          sxyz(3,6) = z62 + sxyz(3,2)
!     
!  Set region 2 sextet flag
!     
                          lreg12    = .false.
                          lreg2stet = .false.
                          if (lseok.and.nregions(ncf).gt.1) then
                            lreg2stet = (nregioni.gt.1.and.nregionj.gt.1.and.nregionk.gt.1.and.nregionl.gt.1 &
                                         .and.nregionm.gt.1.and.nregionn.gt.1)
                            if (.not.lreg2stet) lreg12 = (nregioni.gt.1.or.nregionj.gt.1.or.nregionk.gt.1.or. &
                                                          nregionl.gt.1.or.nregionl.gt.1.or.nregionn.gt.1)
                          endif
                          lslicel = lsliceatom(nsft + nrelat(l))
                          lattach = .true.
                          if (lslicei.and.lslicej.and.lslicek.and.lslicel.and.lslicem.and.lslicen) lattach = .false.
                          if (.not.lslicei.and..not.lslicej.and..not.lslicek.and..not.lslicel.and..not.lslicem &
                              .and..not.lslicen) lattach = .false.
!********************************
!  Valid six-body term located  *
!********************************
!
!  Calculate vectors and remaining distances
!
                          do ivec = 1,6
                            do jvec = 1,6
                              svec(1,jvec,ivec) = sxyz(1,ivec) - sxyz(1,jvec)
                              svec(2,jvec,ivec) = sxyz(2,ivec) - sxyz(2,jvec)
                              svec(3,jvec,ivec) = sxyz(3,ivec) - sxyz(3,jvec)
                            enddo
                          enddo
                          indvec = 0
                          do ivec = 1,5
                            do jvec = ivec+1,6
                              indvec = indvec + 1
                              sdist(indvec) = svec(1,jvec,ivec)**2 + svec(2,jvec,ivec)**2 + svec(3,jvec,ivec)**2
                              sdist(indvec) = sqrt(sdist(indvec))
                            enddo
                          enddo
!
                          ofct = oci*ocj*ock*ocl*ocm*ocn
                          rko = rksix*ofct
                          call sixbody(np,nsixtype,sdist,eterm,e1d,e2d,e3d,rko,lgrad1,lgrad2,.false.)
                          if (lreg2stet) then
                            esregion2 = esregion2 + eterm
                          elseif (lreg12) then
                            esregion12 = esregion12 + eterm
                          else
                            esix = esix + eterm
                          endif
!
!  Assign inter-region energy to i-j pair only - sixbody terms shouldn't cross regions anyway
!
                          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm
!
                          if (lattach) eattach = eattach + eterm
!***********************************
!  Cross out of plane derivatives  *
!***********************************
!
!  Set up strain products
!
                          if (lsg1) then
                            call sixstrterms(ndim,rprod,svec)
                          endif
!***********************
!  Strain derivatives  *
!***********************
                          if (lsg1) then
!
!  First strain derivatives
!
                            rstrdloc(1:nstrains) = 0.0_dp
                            do ivec = 1,15
                              do kl = 1,nstrains
                                rstrdloc(kl) = rstrdloc(kl) + e1d(ivec)*rprod(kl,ivec)
                              enddo
                            enddo
                            do kl = 1,nstrains
                              rstrd(kl) = rstrd(kl) + rstrdloc(kl)
                            enddo
                            if (latomicstress) then
                              do ivec = 1,6
                                do kl = 1,nstrains
                                  atomicstress(kl,isatom(ivec)) = atomicstress(kl,isatom(ivec)) + sixth*rstrdloc(kl)
                                enddo
                              enddo
                            endif
!                       
!  Second strain derivatives
!  
!  Strain only
!  
                            if (lgrad2) then
                              do kk = 1,nstrains
                                do ivec = 1,15
                                  temp(ivec) = 0.0_dp
                                  do jvec = 1,15
                                    temp(ivec) = temp(ivec) + e2d(kb3(jvec,ivec))*rprod(kk,jvec)
                                  enddo
                                enddo
!
!  Strain-strain second derivatives
!
                                do ivec = 1,15
                                  do kl = 1,nstrains
                                    sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,ivec)*temp(ivec)
                                  enddo
                                enddo
!
!  Mixed derivatives
!
                                indvec = 0
                                do ivec = 1,5
                                  do jvec = ivec+1,6
                                    indvec = indvec + 1
                                    ja = iveca(1,indvec)
                                    ia = iveca(2,indvec)
                                    if (lopis(ia)) then
                                      derv3(isxyz(1,ia),kk) = derv3(isxyz(1,ia),kk) + svec(1,jvec,ivec)*temp(indvec)
                                      derv3(isxyz(2,ia),kk) = derv3(isxyz(2,ia),kk) + svec(2,jvec,ivec)*temp(indvec)
                                      derv3(isxyz(3,ia),kk) = derv3(isxyz(3,ia),kk) + svec(3,jvec,ivec)*temp(indvec)
                                    endif
                                    if (lopis(ja)) then
                                      derv3(isxyz(1,ja),kk) = derv3(isxyz(1,ja),kk) - svec(1,jvec,ivec)*temp(indvec)
                                      derv3(isxyz(2,ja),kk) = derv3(isxyz(2,ja),kk) - svec(2,jvec,ivec)*temp(indvec)
                                      derv3(isxyz(3,ja),kk) = derv3(isxyz(3,ja),kk) - svec(3,jvec,ivec)*temp(indvec)
                                    endif
                                  enddo
                                enddo
                              enddo
                            endif
                          endif
!*************************
!  Internal derivatives  *
!*************************
                          if (lgrad1) then
                            indvec = 0
                            do ivec = 1,5
                              do jvec = ivec+1,6
                                indvec = indvec + 1
                                xderv(isatom(ivec)) = xderv(isatom(ivec)) + svec(1,jvec,ivec)*e1d(indvec)
                                yderv(isatom(ivec)) = yderv(isatom(ivec)) + svec(2,jvec,ivec)*e1d(indvec)
                                zderv(isatom(ivec)) = zderv(isatom(ivec)) + svec(3,jvec,ivec)*e1d(indvec)
                                xderv(isatom(jvec)) = xderv(isatom(jvec)) + svec(1,ivec,jvec)*e1d(indvec)
                                yderv(isatom(jvec)) = yderv(isatom(jvec)) + svec(2,ivec,jvec)*e1d(indvec)
                                zderv(isatom(jvec)) = zderv(isatom(jvec)) + svec(3,ivec,jvec)*e1d(indvec)
                              enddo
                            enddo
                          endif
                          if (lgrad2) then
                            indvec = 0
                            do ivec = 1,15
                              ja = iveca(1,ivec)
                              ia = iveca(2,ivec)
                              if (lopis(ia).and.lopis(ja)) then
                                ia1 = ia
                                ja1 = ja
                                lopone = .false.
                              elseif (lopis(ia)) then
                                ia1 = ia
                                ja1 = ia
                                lopone = .true.
                              elseif (lopis(ja)) then
                                ia1 = ja
                                ja1 = ja
                                lopone = .true.
                              endif
                              if (lopis(ia).or.lopis(ja).and.ia.ne.ja) then
                                derv2(isxyz(1,ja1),isxyz(1,ia1)) = derv2(isxyz(1,ja1),isxyz(1,ia1)) - e1d(ivec)
                                derv2(isxyz(2,ja1),isxyz(2,ia1)) = derv2(isxyz(2,ja1),isxyz(2,ia1)) - e1d(ivec)
                                derv2(isxyz(3,ja1),isxyz(3,ia1)) = derv2(isxyz(3,ja1),isxyz(3,ia1)) - e1d(ivec)
                                if (.not.lopone) then
                                  derv2(isxyz(1,ia1),isxyz(1,ja1)) = derv2(isxyz(1,ia1),isxyz(1,ja1)) - e1d(ivec)
                                  derv2(isxyz(2,ia1),isxyz(2,ja1)) = derv2(isxyz(2,ia1),isxyz(2,ja1)) - e1d(ivec)
                                  derv2(isxyz(3,ia1),isxyz(3,ja1)) = derv2(isxyz(3,ia1),isxyz(3,ja1)) - e1d(ivec)
                                endif
                              endif
                              do jvec = ivec,15
                                indvec = indvec + 1 
                                jb = iveca(1,jvec)
                                ib = iveca(2,jvec)
                                if (lopis(ia).and.lopis(ib)) then
                                  ia1 = ia
                                  ib1 = ib
                                elseif (lopis(ia)) then
                                  ia1 = ia
                                  ib1 = ia
                                elseif (lopis(ib)) then
                                  ia1 = ib
                                  ib1 = ib
                                endif
                                if (lopis(ia).and.lopis(jb)) then
                                  ia2 = ia
                                  jb2 = jb
                                elseif (lopis(ia)) then
                                  ia2 = ia
                                  jb2 = ia
                                elseif (lopis(jb)) then
                                  ia2 = jb
                                  jb2 = jb
                                endif
                                if (lopis(ja).and.lopis(jb)) then
                                  ja3 = ja
                                  jb3 = jb
                                elseif (lopis(ja)) then
                                  ja3 = ja
                                  jb3 = ja
                                elseif (lopis(jb)) then
                                  ja3 = jb
                                  jb3 = jb
                                endif
                                if (lopis(ja).and.lopis(ib)) then
                                  ja4 = ja
                                  ib4 = ib
                                elseif (lopis(ja)) then
                                  ja4 = ja
                                  ib4 = ja
                                elseif (lopis(ib)) then
                                  ja4 = ib
                                  ib4 = ib
                                endif
                                do ialp = 1,3
                                  do ibet = 1,3
                                    d2trm = svec(ialp,ja,ia)*svec(ibet,jb,ib)*e2d(indvec)
                                    if (lopis(ia).or.lopis(ib).and.ia.ne.ib) then
                                      if ((ia1.eq.ib1.and.ia.gt.ib).or.ia1.ne.ib1) then
                                        derv2(isxyz(ibet,ib1),isxyz(ialp,ia1)) = derv2(isxyz(ibet,ib1),isxyz(ialp,ia1)) + d2trm
                                      endif
                                    endif
                                    if (lopis(ia).or.lopis(jb).and.ia.ne.jb) then
                                      if ((ia2.eq.jb2.and.ia.gt.jb).or.ia2.ne.jb2) then
                                        derv2(isxyz(ibet,jb2),isxyz(ialp,ia2)) = derv2(isxyz(ibet,jb2),isxyz(ialp,ia2)) - d2trm
                                      endif
                                    endif
                                    if (lopis(ja).or.lopis(jb).and.ja.ne.jb) then
                                      if ((ja3.eq.jb3.and.ja.gt.jb).or.ja3.ne.jb3) then
                                        derv2(isxyz(ibet,jb3),isxyz(ialp,ja3)) = derv2(isxyz(ibet,jb3),isxyz(ialp,ja3)) + d2trm
                                      endif
                                    endif
                                    if (lopis(ja).or.lopis(ib).and.ja.ne.ib) then
                                      if ((ja4.eq.ib4.and.ja.gt.ib).or.ja4.ne.ib4) then
                                        derv2(isxyz(ibet,ib4),isxyz(ialp,ja4)) = derv2(isxyz(ibet,ib4),isxyz(ialp,ja4)) - d2trm
                                      endif
                                    endif
!
                                    if (ivec.ne.jvec) then
                                      if (lopis(ia).or.lopis(ib).and.ia.ne.ib) then
                                        if ((ib1.eq.ia1.and.ib.gt.ia).or.ib1.ne.ia1) then
                                          derv2(isxyz(ialp,ia1),isxyz(ibet,ib1)) = derv2(isxyz(ialp,ia1),isxyz(ibet,ib1))+d2trm
                                        endif
                                      endif
                                      if (lopis(ia).or.lopis(jb).and.ia.ne.jb) then
                                        if ((jb2.eq.ia2.and.jb.gt.ia).or.jb2.ne.ia2) then
                                          derv2(isxyz(ialp,ia2),isxyz(ibet,jb2)) = derv2(isxyz(ialp,ia2),isxyz(ibet,jb2))-d2trm
                                        endif
                                      endif
                                      if (lopis(ja).or.lopis(jb).and.ja.ne.jb) then
                                        if ((jb3.eq.ja3.and.jb.gt.ja).or.jb3.ne.ja3) then
                                          derv2(isxyz(ialp,ja3),isxyz(ibet,jb3)) = derv2(isxyz(ialp,ja3),isxyz(ibet,jb3))+d2trm
                                        endif
                                      endif
                                      if (lopis(ja).or.lopis(ib).and.ja.ne.ib) then
                                        if ((ib4.eq.ja4.and.ib.gt.ja).or.ib4.ne.ja4) then
                                          derv2(isxyz(ialp,ja4),isxyz(ibet,ib4)) = derv2(isxyz(ialp,ja4),isxyz(ibet,ib4))-d2trm
                                        endif
                                      endif
                                    endif
                                  enddo
                                enddo
                              enddo
                            enddo
                          endif
!
!  End of inner loops over atoms and cell vectors
!
                        enddo mnloop
                      enddo nloop
                    enddo mmloop
                  enddo mloop
                enddo llloop
              enddo lloop
            enddo jjloop
          enddo kloop
        enddo iiloop
      enddo jloop
    enddo iloop
!
!  End of outer loops
!
  enddo
!
!  If symmetry adapted derivatives have been calculated elsewhere
!  then add derivatives of related atoms
!
  if (lgrad1) then
    if (lsymderv.and.(.not.lgrad2)) then
      do i = 1,nasym
        nr = nrel2(i)
        neq = neqv(i)
        xdrv(i) = xdrv(i) + neq*xderv(nr)
        ydrv(i) = ydrv(i) + neq*yderv(nr)
        zdrv(i) = zdrv(i) + neq*zderv(nr)
!
        nregioni = nregionno(nsft+i)
        xregdrv(nregioni) = xregdrv(nregioni) + neq*xderv(nr)
        yregdrv(nregioni) = yregdrv(nregioni) + neq*yderv(nr)
        zregdrv(nregioni) = zregdrv(nregioni) + neq*zderv(nr)
      enddo
    else
      do i = 1,numat
        xdrv(i) = xdrv(i) + xderv(i)
        ydrv(i) = ydrv(i) + yderv(i)
        zdrv(i) = zdrv(i) + zderv(i)
!
        nregioni = nregionno(nsft+nrelat(i))
        xregdrv(nregioni) = xregdrv(nregioni) + xderv(i)
        yregdrv(nregioni) = yregdrv(nregioni) + yderv(i)
        zregdrv(nregioni) = zregdrv(nregioni) + zderv(i)
      enddo
    endif
  endif
!
!  Free local memory
!
  deallocate(zderv,stat=status)
  if (status/=0) call deallocate_error('sixnos','zderv')
  deallocate(yderv,stat=status)
  if (status/=0) call deallocate_error('sixnos','yderv')
  deallocate(xderv,stat=status)
  if (status/=0) call deallocate_error('sixnos','xderv')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('sixnos','nptr')
!
!  Timing
!
  time2 = cputime()
  tsix = tsix + time2 - time1
!
  return
  end
