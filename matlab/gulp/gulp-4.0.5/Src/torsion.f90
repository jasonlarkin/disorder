  subroutine torsion
!
!  Calculates torsion angles and prints them out
!  for valid four-body terms
!
!   1/95 Intra/inter-molecular specification added
!   2/95 Bonded specification added for four-body terms
!   3/95 Periodic molecule corrections added
!   3/97 Modified for out of plane potentials
!  12/00 Set of looping vectors removed as should be done
!        by call to rtlist
!  12/00 Reference to "14" replaced by iimid for generality
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol
!  10/01 Format of output changed to accomodate larger atom nos
!  11/02 Wildcard atom types added
!   5/04 Handling of lattice vectors generalised
!   9/03 Spatial decomposition option modifications made
!   9/03 imaxl, jmaxl & kmaxl renamed to avoid conflict with global symbols
!  10/05 Modified to handle inversion potential
!   6/06 Inversion squared potential added
!   1/07 lmatch settings changed to handle wildcards
!   2/07 Bonding types and test added
!   4/07 REPLACED BY NEW VERSION: Uses old fournos algorithm 
!        since this is more consistent with new torsion searching
!        rules and is more reliable.
!   4/07 Bond type checking added for ndim > 0
!   5/07 QMMM schemes added
!  10/07 Angle-angle cross potential added
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  10/07 Handling of n4botype(2,n) modified
!  12/07 Unused variables removed
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 New logic for matching species introduced to handle case of the same element
!        being the middle atom, but one with a specific type and the other without.
!  11/08 ixl etc defined relative to ixj etc rather than ixk to correct error
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, November 2008
!
  use configurations, only : nregionno, nregiontype, QMMMmode
  use constants
  use current
  use element
  use four
  use iochannels
  use molecule
  use spatial,   only : lspatialok, xinbox, yinbox, zinbox
  use symmetry
  use times
  implicit none
!
!  Local variables
!
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  character(len=80)                            :: string
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: imax
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
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
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kmax
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: ll
  integer(i4),                            save :: maxvector = 27
  integer(i4)                                  :: n
  integer(i4)                                  :: nbtypeji
  integer(i4)                                  :: nbtypejk
  integer(i4)                                  :: nbtypekl
  integer(i4)                                  :: nbtypeji2
  integer(i4)                                  :: nbtypejk2
  integer(i4)                                  :: nbtypekl2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmid
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: noofp
  integer(i4)                                  :: nphi
  integer(i4)                                  :: nphitot
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
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: nvector
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: lexactmatch
  logical                                      :: liok
  logical                                      :: limatch1
  logical                                      :: limatch4
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: ljmatch2
  logical                                      :: ljmatch3
  logical                                      :: ljkmatch
  logical                                      :: lkjmatch
  logical                                      :: lkmatch2
  logical                                      :: lkmatch3
  logical                                      :: lmatch
  logical                                      :: lmeither
  logical                                      :: lmolloc
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lsamemol
  logical                                      :: ltsyme_exact
  real(dp)                                     :: c12
  real(dp)                                     :: c34
  real(dp)                                     :: cputime
  real(dp)                                     :: cos2
  real(dp)                                     :: cos3
  real(dp)                                     :: cosphi
  real(dp)                                     :: cut
  real(dp)                                     :: phi
  real(dp)                                     :: r21
  real(dp)                                     :: r31
  real(dp)                                     :: r32
  real(dp)                                     :: r41
  real(dp)                                     :: r42
  real(dp)                                     :: r43
  real(dp)                                     :: rtmp
  real(dp)                                     :: rvp21
  real(dp)                                     :: rvp34
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr4
  real(dp)                                     :: vp21x
  real(dp)                                     :: vp21y
  real(dp)                                     :: vp21z
  real(dp)                                     :: vp34x
  real(dp)                                     :: vp34y
  real(dp)                                     :: vp34z
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x21t
  real(dp)                                     :: y21t
  real(dp)                                     :: z21t
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x32t
  real(dp)                                     :: y32t
  real(dp)                                     :: z32t
  real(dp)                                     :: x42
  real(dp)                                     :: y42
  real(dp)                                     :: z42
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
  real(dp)                                     :: x43t
  real(dp)                                     :: y43t
  real(dp)                                     :: z43t
  real(dp)                                     :: xc1t
  real(dp)                                     :: yc1t
  real(dp)                                     :: zc1t
  real(dp)                                     :: xc2
  real(dp)                                     :: yc2
  real(dp)                                     :: zc2
  real(dp)                                     :: xc3
  real(dp)                                     :: yc3
  real(dp)                                     :: zc3
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
  real(dp), dimension(:), allocatable          :: xvec
  real(dp), dimension(:), allocatable          :: yvec
  real(dp), dimension(:), allocatable          :: zvec
!
  time1 = cputime()
  lmolloc = (nmol.gt.0)
!
!  Allocate local memory
!
  if (ndim.gt.0) then
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','zvec')
  endif
!
!  Define error string
!
  string = 'angle in torsion has become 180 degrees'
!
!  Check how many four-body potentials are of out of plane type
!
  noofp = 0
  do n = 1,nfor
    if (loutofplane(n)) then
      noofp = noofp + 1
    endif
  enddo
!   
!  If there are no standard four-body potentials - don't bother
!  printing the banner!
!   
  if (noofp.ne.nfor) then
    write(ioout,'(/,''  Analysis of four-body terms for primitive cell:'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Potential   Atom 1     Atom 2     Atom 3     Atom 4         Phi (degrees)'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif 
!   
!  Zero total number of torsions
!
  nphitot = 0
!*************************
!  Loop over potentials  *
!*************************
  pots: do n = 1,nfor
    nphi = 0
    if (loutofplane(n)) cycle pots
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
!  Create lattice vectors
!
    if (ndim.gt.0) then
      cut = for1(n) + for2(n) + for3(n)
      if (for4(n).gt.0.0_dp) cut = for4(n)
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      if (nvector.gt.maxvector) then
!
!  Too many vectors
!
        deallocate(zvec,stat=status)
        if (status/=0) call deallocate_error('fournos','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('fournos','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('fournos','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('fournos','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('fournos','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('fournos','zvec')
        call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      endif
    else
      nvector = 1
      nmid = 1
      xvec(1) = 0.0_dp
      yvec(1) = 0.0_dp
      zvec(1) = 0.0_dp
    endif
!********************************
!  Loop over middle site 2 / j  *
!********************************
    jloop: do j = 1,numat
      nj = nat(j)
      ntypj = nftype(j)
      nregionj = nregionno(nsft+nrelat(j))       
      nregiontypj = nregiontype(nregionj,ncf)
!
!  Check whether j is allowed for n
!
      ljmatch2 = lmatch(nj,ntypj,nt2,ntyp2,.true.)
      ljmatch3 = lmatch(nj,ntypj,nt3,ntyp3,.true.)
!
!  If no matches is found then cycle
!
      if (.not.ljmatch2.and..not.ljmatch3) cycle jloop
!
!  j has been accepted
!
      if (lspatialok) then
        xc2 = xinbox(j)
        yc2 = yinbox(j)
        zc2 = zinbox(j)
      else
        xc2 = xclat(j)
        yc2 = yclat(j)
        zc2 = zclat(j)
      endif
!
!  Molecule handling
!
      if (lmolloc.and.lneedmol) then
        nmj = natmol(j)
        if (ndim.gt.0) then
          indmj = nmolind(j)
          call mindtoijk(indmj,ixj,iyj,izj)
        endif
      endif
      call label(nj,ntypj,lab2)
!***************************************
!  Loop over second middle site 3 / k  *
!***************************************
      kloop: do k = j,numat
        nk = nat(k)
        ntypk = nftype(k)
        nregionk = nregionno(nsft+nrelat(k))
        nregiontypk = nregiontype(nregionk,ncf)
!
!  Check whether j and k are allowed for n
!
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
        if (.not.ljkmatch.and..not.lkjmatch) cycle kloop
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
        endif
!
!  Set flag indicating whether middle atoms could be matched either way round
!
        lmeither = (ljkmatch.and.lkjmatch)
!
!  QM/MM handling : j & k are both QM atoms and potential is of bonded type => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypj.eq.1.and.nregiontypk.eq.1.and.lbtyp) cycle kloop
        endif
        if (lmolloc.and.lneedmol) then
!
!  Molecule handling
!
          nmk = natmol(k)
          if (ndim.gt.0) then
            indmk = nmolind(k)
            call mindtoijk(indmk,ixk,iyk,izk)
            ixk = ixk - ixj
            iyk = iyk - iyj
            izk = izk - izj
          endif
          lmolok = (nmj.eq.nmk.and.nmj.gt.0)
        else
          lmolok = .false.
        endif
!
!  Check for intra and but not in same molecule
!
        if (lintra_only.and..not.lmolok) cycle kloop
        if (lbtyp.and..not.lmolok) cycle kloop
        if (lspatialok) then
          xc3t = xinbox(k)
          yc3t = yinbox(k)
          zc3t = zinbox(k)
        else
          xc3t = xclat(k)
          yc3t = yclat(k)
          zc3t = zclat(k)
        endif
        x32t = xc3t - xc2
        y32t = yc3t - yc2
        z32t = zc3t - zc2
        call label(nk,ntypk,lab3)
!
!  Check r32 is OK
!  Loop over cell vectors
!
        jjloop: do jj = 1,nvector
          r32 = (xvec(jj)+x32t)**2 + (yvec(jj)+y32t)**2 + (zvec(jj)+z32t)**2
          if (r32.lt.1d-12) cycle jjloop
          if (k.eq.j.and.jj.eq.nmid) cycle jjloop
!
!  Molecule checking
!
          lbonded = .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle jjloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,j,k,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle jjloop
!
!  Check central bond type for correct order
!
                if (n4botype(1,n).gt.0) then
                  if (n4botype(1,n).ne.nbtypejk) cycle jjloop
                endif
                if (n4botype(2,n).ne.nbtypejk2) cycle jjloop
              endif
            else
              call lintoijk(jxx,jyy,jzz,jj,imax,jmax,kmax)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,j,k,jxx,jyy,jzz)
                if (.not.lbonded) cycle jjloop
!
!  Check central bond type for correct order
!
                if (n4botype(1,n).gt.0) then
                  if (n4botype(1,n).ne.nbtypejk) cycle jjloop
                endif
                if (n4botype(2,n).ne.nbtypejk2) cycle jjloop
                lsamemol = (lbonded.or.l2bonds)
              else
                lsamemol = .false.
              endif
              if (.not.lsamemol) then
                call samemol(lsamemol,nmj,jxx,jyy,jzz,ixk,iyk,izk)
              endif
              if (lintra_only.and..not.lsamemol) cycle jjloop
              if (linter_only.and.lsamemol) cycle jjloop
            endif
          endif
!
!  Distance checking
!
          if (r32.gt.tr2.and.(.not.lbtyp.or..not.lbonded)) cycle jjloop
          r32 = sqrt(r32)
          xc3 = xc3t + xvec(jj)
          yc3 = yc3t + yvec(jj)
          zc3 = zc3t + zvec(jj)
          x32 = x32t + xvec(jj)
          y32 = y32t + yvec(jj)
          z32 = z32t + zvec(jj)
!*****************************
!  Loop over end site 1 / i  *
!*****************************
          iloop: do i = 1,numat
            ni = nat(i)
            ntypi = nftype(i)
            nregioni = nregionno(nsft+nrelat(i))
            nregiontypi = nregiontype(nregioni,ncf)
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
            endif
!
!  Molecule handling
!
            if (lmolloc.and.lneedmol) then
              nmi = natmol(i)
              if (ndim.gt.0) then
                indmi = nmolind(i)
                call mindtoijk(indmi,ixi,iyi,izi)
                ixi = ixi - ixj
                iyi = iyi - iyj
                izi = izi - izj
              endif
              lmolok = (nmj.eq.nmi.and.nmj.gt.0)
            else
              lmolok = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok) cycle iloop
            if (lbtyp.and..not.lmolok) cycle iloop
!
            if (lspatialok) then
              xc1t = xinbox(i)
              yc1t = yinbox(i)
              zc1t = zinbox(i)
            else
              xc1t = xclat(i)
              yc1t = yclat(i)
              zc1t = zclat(i)
            endif
            x21t = xc2 - xc1t
            y21t = yc2 - yc1t
            z21t = zc2 - zc1t
            call label(ni,ntypi,lab1)
!
!  Check r21 is OK
!  Loop over cell vectors
!
            iiloop: do ii = 1,nvector
              r21 = (-xvec(ii) + x21t)**2 + (-yvec(ii) + y21t)**2 + (-zvec(ii) + z21t)**2
              if (r21.lt.1d-12) cycle iiloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.ii.eq.jj) cycle iiloop
!
!  Molecule checking
!
              lbonded = .false.
              if (lmolok) then
                if (ndim.eq.0) then
                  if (linter_only) cycle iiloop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeji,nbtypeji2,j,i,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle iiloop
                  endif
                else
                  call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeji,nbtypeji2,j,i,ixx,iyy,izz)
                    if (.not.lbonded) cycle iiloop
                    lsamemol = (lbonded.or.l2bonds)
                  else
                    lsamemol = .false.
                  endif
                  if (.not.lsamemol) then
                    call samemol(lsamemol,nmj,ixx,iyy,izz,ixi,iyi,izi)
                  endif
                  if (lintra_only.and..not.lsamemol) cycle iiloop
                  if (linter_only.and.lsamemol) cycle iiloop
                endif
              endif
!
!  Distance checking
!
              if (r21.gt.tr1.and.(.not.lbtyp.or..not.lbonded)) cycle iiloop
              x21 = x21t - xvec(ii)
              y21 = y21t - yvec(ii)
              z21 = z21t - zvec(ii)
!
!  Check r31 is OK
!
              x31 = x32 + x21
              y31 = y32 + y21
              z31 = z32 + z21
              r31 = x31*x31 + y31*y31 + z31*z31
              if (r31.lt.1.0d-8) cycle iiloop
!
              r21 = sqrt(r21)
              r31 = sqrt(r31)
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
              lloop: do l = 1,numat
                nl = nat(l)
                ntypl = nftype(l)
                nregionl = nregionno(nsft+nrelat(l))
                nregiontypl = nregiontype(nregionl,ncf)
!
!  Check l is allowed for n
!
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  QM/MM handling : i, j, k & l are all QM atoms => exclude
!               
                if (QMMMmode(ncf).gt.0) then
                  if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1) cycle lloop
                endif
!
                if (lmolloc.and.lneedmol) then
!
!  Molecule handling
!
                  nml = natmol(l)
                  if (ndim.gt.0) then
                    indml = nmolind(l)
                    call mindtoijk(indml,ixl,iyl,izl)
                    ixl = ixl - ixj 
                    iyl = iyl - iyj 
                    izl = izl - izj 
                  endif
                  lmolok = (nmj.eq.nml.and.nmj.gt.0)
                else
                  lmolok = .false.
                endif
!
!  Check for intra and but not in same molecule
!
                if (lintra_only.and..not.lmolok) cycle lloop
                if (lbtyp.and..not.lmolok) cycle lloop
                if (lspatialok) then
                  xc4t = xinbox(l)
                  yc4t = yinbox(l)
                  zc4t = zinbox(l)
                else
                  xc4t = xclat(l)
                  yc4t = yclat(l)
                  zc4t = zclat(l)
                endif
                x43t = xc4t - xc3
                y43t = yc4t - yc3
                z43t = zc4t - zc3
                call label(nl,ntypl,lab4)
!
!  Check r43 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,nvector
                  r43 = (xvec(ll)+x43t)**2 + (yvec(ll)+y43t)**2 + (zvec(ll)+z43t)**2
                  if (r43.lt.1d-12) cycle llloop
!
!  Molecule checking
!
                  lbonded = .false.
                  if (lmolok) then
                    if (ndim.eq.0) then
                      if (linter_only) cycle llloop
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypekl,nbtypekl2,k,l,0_i4,0_i4,0_i4)
                        if (.not.lbonded) cycle llloop
                      endif
                    else
                      call lintoijk(kxx,kyy,kzz,ll,imax,jmax,kmax)
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypekl,nbtypekl2,k,l,kxx-jxx,kyy-jyy,kzz-jzz)
                        if (.not.lbonded) cycle llloop
                        lsamemol = (lbonded.or.l2bonds)
                      else
                        lsamemol = .false.
                      endif
                      if (.not.lsamemol) then
                        call samemol(lsamemol,nmj,kxx,kyy,kzz,ixl,iyl,izl)
                      endif
                      if (lintra_only.and..not.lsamemol) cycle llloop
                      if (linter_only.and.lsamemol) cycle llloop
                    endif
                  endif
!
!  Distance checking
!
                  if (r43.gt.tr3.and.(.not.lbtyp.or..not.lbonded)) cycle llloop
                  r43 = sqrt(r43)
                  x43 = x43t + xvec(ll)
                  y43 = y43t + yvec(ll)
                  z43 = z43t + zvec(ll)
!
!  Check r41 is OK
!
                  x41 = x43 + x32 + x21
                  y41 = y43 + y32 + y21
                  z41 = z43 + z32 + z21
                  r41 = x41*x41 + y41*y41 + z41*z41
                  if (r41.gt.tr4.and.tr4.gt.0.0_dp.and..not.lbtyp) cycle llloop
                  if (r41.lt.1.0d-8) cycle llloop
!
!  Check r42 is OK
!
                  x42 = x32 + x43
                  y42 = y32 + y43
                  z42 = z32 + z43
                  r42 = x42*x42 + y42*y42 + z42*z42
                  if (r42.lt.1.0d-8) cycle llloop
!*********************************
!  Valid four-body term located  *
!*********************************
!                   
!  Generate perpendicular end vectors
!                   
                  c12 = (x21*x32+y21*y32+z21*z32)
                  cos2 = - c12/(r21*r32)
                  c12 = c12/(r32*r32)
                  if (cos2.le.-0.99999999) then
                    call outerror(string,0_i4)
                    call stopnow('torsion')
                  endif
                  if (cos2.ge.0.99999999) then
                    call outerror(string,0_i4)
                    call stopnow('torsion')
                  endif
                  vp21x = c12*x32 - x21
                  vp21y = c12*y32 - y21
                  vp21z = c12*z32 - z21
                  rvp21 = (vp21x*vp21x) + (vp21y*vp21y) + (vp21z*vp21z)
                  rvp21 = sqrt(rvp21)
!
                  c34 = (x43*x32+y43*y32+z43*z32)
                  cos3 = - c34/(r43*r32)
                  c34 = c34/(r32*r32)
                  if (cos3.le.-0.99999999) then 
                    call outerror(string,0_i4)
                    call stopnow('torsion')
                  endif
                  if (cos3.ge.0.99999999) then
                    call outerror(string,0_i4)
                    call stopnow('torsion')
                  endif
                  vp34x = x43 - c34*x32
                  vp34y = y43 - c34*y32
                  vp34z = z43 - c34*z32
                  rvp34 = (vp34x*vp34x) + (vp34y*vp34y) + (vp34z*vp34z)
                  rvp34 = sqrt(rvp34)
!
                  cosphi = (vp21x*vp34x+vp21y*vp34y+vp21z*vp34z)
                  cosphi = cosphi/(rvp21*rvp34)
                  if (cosphi.gt.1.0_dp) cosphi = 1.0_dp
                  if (cosphi.lt.-1.0_dp) cosphi = - 1.0_dp
                  phi = acos(cosphi)*radtodeg
!
!  Output
!
                  nphi = nphi + 1
                  if (nphi.eq.1) then
                    write(ioout,'(5x,i3,4x,4(1x,a5,1x,i4),7x,f10.6)') n,lab1,i,lab2,j,lab3,k,lab4,l,phi
                  else
                    write(ioout,'(12x,4(1x,a5,1x,i4),7x,f10.6)') lab1,i,lab2,j,lab3,k,lab4,l,phi
                  endif
!
!  End of inner loops over atoms and cell vectors
!
                enddo llloop
              enddo lloop
            enddo iiloop
          enddo iloop
        enddo jjloop
      enddo kloop
    enddo jloop
    if (nphi.eq.0) then
      write(ioout,'(5x,i3,9x,''No angles found'')')n
    endif
    nphitot = nphitot + nphi
    write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  End of outer loop over potentials
!
  enddo pots
  if (nphitot.gt.0) then
    write(ioout,'(''  Total number of torsions = '',i8)') nphitot
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif   
  write(ioout,'(/)')
!
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('fournos','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('fournos','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('fournos','xvec')
! 
!  If out of plane potentials are present call routine for these
! 
  if (noofp.gt.0) call outofp
!
!  Timing
!
  time2 = cputime()
  tfour = tfour + time2 - time1
!
  return
  end
