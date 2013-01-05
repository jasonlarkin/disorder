  subroutine angle
!
!  Outputs a table of valid three-body angles
!  Modified to allow for distance-dependant three-body terms.
!
!   1/95 Intra/intermolecular specification added
!   3/95 Periodic molecule corrections added
!   7/00 Apparent hiccup in nang initialisation fixed
!        and dynamic allocation of arrays added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   5/01 Minimum cut-offs added
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol 
!   4/02 Initialisation of number of angles corrected
!   9/03 Spatial decomposition modifications added
!   7/05 Deallocations cleaned
!   2/07 Bonding types added
!   3/07 Allocation style changed
!   5/07 QM/MM scheme added
!   5/07 Dreiding option added
!   6/07 Dreiding option bonded2donorJK check added
!   7/07 Checking of bond orders added
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!   6/08 Checking of bond numbers added
!   6/09 Module name changed from three to m_three
!   5/12 Format statements modified to allow for larger numbers
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
  use configurations, only : nregionno, nregiontype, QMMMmode
  use constants
  use current
  use element
  use iochannels
  use m_three
  use molecule
  use spatial,   only : lspatialok, xinbox, yinbox, zinbox
  use times
  implicit none
!
!  Local variables
!
  character(len=1)                             :: cstyp1
  character(len=1)                             :: cstyp2
  character(len=1)                             :: cstyp3
  character(len=1)                             :: cstyp4
  character(len=1)                             :: cstyp5
  character(len=1)                             :: cstyp6
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  character(len=5)                             :: lab5
  character(len=5)                             :: lab6
  integer(i4)                                  :: i
  integer(i4)                                  :: ii2
  integer(i4)                                  :: ii3
  integer(i4)                                  :: ii3min
  integer(i4)                                  :: imax
  integer(i4)                                  :: in3
  integer(i4)                                  :: ind
  integer(i4), dimension(:), allocatable       :: ind1
  integer(i4), dimension(:), allocatable       :: ind2
  integer(i4), dimension(:), allocatable       :: ind3
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixk
  integer(i4)                                  :: iyk
  integer(i4)                                  :: izk
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kmax
  integer(i4)                                  :: m
  integer(i4),                            save :: maxangle = 1000
  integer(i4),                            save :: maxvector = 100
  integer(i4)                                  :: n
  integer(i4)                                  :: n3ty
  integer(i4)                                  :: nang
  integer(i4)                                  :: nangtot
  integer(i4), dimension(:), allocatable       :: nat1
  integer(i4), dimension(:), allocatable       :: nat2
  integer(i4), dimension(:), allocatable       :: nat3
  integer(i4)                                  :: nbotyp11
  integer(i4)                                  :: nbotyp12
  integer(i4)                                  :: nbotyp21
  integer(i4)                                  :: nbotyp22
  integer(i4)                                  :: nbtyp11
  integer(i4)                                  :: nbtyp12
  integer(i4)                                  :: nbtyp21
  integer(i4)                                  :: nbtyp22
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nline
  integer(i4)                                  :: nline2
  integer(i4)                                  :: nliner
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmid
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmk
  integer(i4)                                  :: nreli
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nto
  integer(i4), dimension(:), allocatable       :: ntp1
  integer(i4), dimension(:), allocatable       :: ntp2
  integer(i4), dimension(:), allocatable       :: ntp3
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypo
  integer(i4)                                  :: nvector
  integer(i4)                                  :: status
  logical                                      :: bonded2donor
  logical                                      :: bonded2donorJK
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbondnoOK
  logical                                      :: lbondtypeOK
  logical                                      :: lbtyp
  logical                                      :: ldiff23bo
  logical                                      :: ldiff23cut
  logical                                      :: ldiff23typ
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: lmatch
  logical                                      :: lmolloc
  logical                                      :: lmolok
  logical                                      :: lmolok2
  logical                                      :: lneedmol
  logical                                      :: lsamemol
  logical                                      :: lsymijk
  real(dp),    dimension(:), allocatable       :: ang
  real(dp)                                     :: cell21
  real(dp)                                     :: cell22
  real(dp)                                     :: cell23
  real(dp)                                     :: cell31
  real(dp)                                     :: cell32
  real(dp)                                     :: cell33
  real(dp)                                     :: cputime
  real(dp)                                     :: cut
  real(dp)                                     :: dot
  real(dp)                                     :: r12
  real(dp)                                     :: r122
  real(dp)                                     :: r13
  real(dp)                                     :: r23
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr1m
  real(dp)                                     :: tr2
  real(dp)                                     :: tr2m
  real(dp)                                     :: tr3
  real(dp)                                     :: tr3m
  real(dp)                                     :: ttmp
  real(dp)                                     :: ttr1
  real(dp)                                     :: ttr1m
  real(dp)                                     :: ttr2
  real(dp)                                     :: ttr2m
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x23
  real(dp)                                     :: y23
  real(dp)                                     :: z23
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: xc1
  real(dp)                                     :: yc1
  real(dp)                                     :: zc1
  real(dp)                                     :: xt21
  real(dp)                                     :: yt21
  real(dp)                                     :: zt21
  real(dp)                                     :: xt31
  real(dp)                                     :: yt31
  real(dp)                                     :: zt31
  real(dp),    dimension(:), allocatable       :: xvec
  real(dp),    dimension(:), allocatable       :: yvec
  real(dp),    dimension(:), allocatable       :: zvec
!
  time1 = cputime()
  lmolloc = (nmol.gt.0)
  if (nasym.ne.numat) then
    write(ioout,'(/,''  Analysis of three-body terms for asymmetric unit :'',/)')
  else
    write(ioout,'(/,''  Analysis of three-body terms :'',/)')
  endif
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Potential   Atom 1  Atom 2  Atom 3   Angle   Atom 1  Atom 2  Atom 3   Angle  '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Allocate local memory
!
  allocate(xvec(maxvector),stat=status)
  if (status/=0) call outofmemory('angle','xvec')
  allocate(yvec(maxvector),stat=status)
  if (status/=0) call outofmemory('angle','yvec')
  allocate(zvec(maxvector),stat=status)
  if (status/=0) call outofmemory('angle','zvec')
1 allocate(ang(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ang')
  allocate(nat1(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','nat1')
  allocate(nat2(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','nat2')
  allocate(nat3(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','nat3')
  allocate(ntp1(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ntp1')
  allocate(ntp2(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ntp2')
  allocate(ntp3(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ntp3')
  allocate(ind1(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ind1')
  allocate(ind2(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ind2')
  allocate(ind3(maxangle),stat=status)
  if (status/=0) call outofmemory('angle','ind3')
!
!  Initialise total number of valid angles
!
  nangtot = 0
!
!  Loop over potentials
!
  pots: do n = 1,nthb
    n3ty = nthrty(n)
    lsymijk = (n3ty.eq.3.or.n3ty.eq.4.or.n3ty.eq.10)
    nt1 = ntspec1(n)
    nt2 = ntspec2(n)
    nt3 = ntspec3(n)
    ntyp1 = ntptyp1(n)
    ntyp2 = ntptyp2(n)
    ntyp3 = ntptyp3(n)
    nbtyp11 = n3botype(1,1,n)
    nbtyp12 = n3botype(2,1,n)
    nbtyp21 = n3botype(1,2,n)
    nbtyp22 = n3botype(2,2,n)
    lbtyp = (mmtexc(n).eq.1)
    tr1m = thr1min(n)**2
    tr2m = thr2min(n)**2
    tr3m = thr3min(n)**2
    tr1 = thr1(n)**2
    tr2 = thr2(n)**2
    tr3 = thr3(n)**2
!  
!  Check that atomic numbers match
!   
    ldiff23typ = (ntyp2.eq.ntyp3.and.nt2.eq.nt3)
!  
!  ..and the cutoffs..
!   
    ldiff23cut = (tr1.eq.tr2.and.tr1m.eq.tr2m)
!  
!  ..and the bond orders
!   
    ldiff23bo = (nbtyp11.eq.nbtyp21.and.nbtyp12.eq.nbtyp22)
!
    lintra_only = (ltintra(n).and..not.ltinter(n))
    linter_only = (ltinter(n).and..not.ltintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Initialise number of valid angles
!
    nang = 0
!
!  Create lattice vectors
!
    if (ndim.gt.0) then
      cut = tr1
      if (tr2.gt.cut) cut = tr2
      if (lsymijk.and.tr3.gt.cut) cut = tr3
      cut = sqrt(cut)
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      if (nvector.gt.maxvector) then
!
!  Too many vectors
!
        deallocate(zvec,stat=status)
        if (status/=0) call deallocate_error('angle','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('angle','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('angle','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('angle','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('angle','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('angle','zvec')
        call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      endif
    else
      nvector = 1
      xvec(1) = 0.0_dp
      yvec(1) = 0.0_dp
      zvec(1) = 0.0_dp
      imax = 0
      jmax = 0
      kmax = 0
    endif
!
!  Outer loop over sites
!
    iloop: do i = 1,nasym
      ni = iatn(i)
      ntypi = natype(i)
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      nreli = nrel2(i)
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  QM/MM handling
!     
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
! 
!  Dreiding option handling                 
! 
      if (ltdreiding(n)) then               
        if (.not.bonded2donor(i)) cycle iloop
      endif
!
      if (lspatialok) then
        xc1 = xinbox(i)
        yc1 = yinbox(i)
        zc1 = zinbox(i)
      else
        xc1 = xalat(i)
        yc1 = yalat(i)
        zc1 = zalat(i)
      endif
      if (lmolloc.and.lneedmol) then
!
!  Molecule handling
!
        nmi = natmol(nreli)
        indm = nmolind(nreli)
        izi = (indm/100) - 5
        ind = indm - 100*(izi+5)
        iyi = (ind/10) - 5
        ind = ind - 10*(iyi+5)
        ixi = ind - 5
      endif
!
!  Check number of bonds if necessary
!
      if (n3bondnono(1,n).gt.0) then
        lbondnoOK = .false.
        do in3 = 1,n3bondnono(1,n)
          if (nbonds(i).eq.n3bondno(in3,1,n)) lbondnoOK = .true.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
      if (n3bondnono(2,n).gt.0) then
        lbondnoOK = .true.
        do in3 = 1,n3bondnono(2,n)
          if (nbonds(i).eq.n3bondno(in3,2,n)) lbondnoOK = .false.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
!
!  Inner loop over first site
!
      jloop: do j = 1,numat
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
!
!  Check j is allowed for n
!
        if (lmatch(nj,ntypj,nt2,ntyp2,.false.)) then
          nto = nt3
          ntypo = ntyp3
          nbotyp11 = nbtyp11
          nbotyp12 = nbtyp12
          nbotyp21 = nbtyp21 
          nbotyp22 = nbtyp22
          ttr1 = tr1
          ttr2 = tr2
          ttr1m = tr1m
          ttr2m = tr2m
        elseif (lmatch(nj,ntypj,nt3,ntyp3,.true.)) then
          nto = nt2
          ntypo = ntyp2
          nbotyp11 = nbtyp21
          nbotyp12 = nbtyp22
          nbotyp21 = nbtyp11
          nbotyp22 = nbtyp12
          ttr1 = tr2
          ttr2 = tr1
          ttr1m = tr2m
          ttr2m = tr1m
        else
          cycle jloop
        endif
        if (lspatialok) then
          x21 = xinbox(j) - xc1
          y21 = yinbox(j) - yc1
          z21 = zinbox(j) - zc1
        else
          x21 = xclat(j) - xc1
          y21 = yclat(j) - yc1
          z21 = zclat(j) - zc1
        endif
        if (lmolloc.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          indmj = nmolind(j)
          izj = (indmj/100) - 5
          ind = indmj-100*(izj+5)
          iyj = (ind/10) - 5
          ind = ind - 10*(iyj+5)
          ixj = ind - 5
          ixj = ixj - ixi
          iyj = iyj - iyi
          izj = izj - izi
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Check for intra and but not in same molecule
!
        if (lintra_only.and..not.lmolok) cycle jloop
        if (lbtyp.and..not.lmolok) cycle jloop
!
!  Check r12 is OK
!  Loop over cell vectors
!
        ii2loop: do ii2 = 1,nvector
          cell21 = xvec(ii2)
          cell22 = yvec(ii2)
          cell23 = zvec(ii2)
          r122 = (cell21+x21)**2+(cell22+y21)**2+(cell23+z21)**2
          if (r122.lt.1.0d-12) cycle ii2loop
!
!  Molecule checking
!
          lbonded  =  .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle ii2loop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,nreli,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle ii2loop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii2,imax,jmax,kmax)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,nreli,j,ixx,iyy,izz)
                if (.not.lbonded) cycle ii2loop
                lsamemol = (lbonded.or.l2bonds)
              else
                lsamemol = .false.
              endif
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ixx,iyy,izz,ixj,iyj,izj)
              endif
              if (lintra_only.and..not.lsamemol) cycle ii2loop
              if (linter_only.and.lsamemol) cycle ii2loop
            endif
          endif
!
!  Distance checking
!
          if ((r122.gt.ttr1.or.r122.lt.ttr1m).and.(.not.lbtyp.or..not.lbonded)) then
            if (ldiff23typ.or..not.ldiff23cut) cycle ii2loop
            if (r122.lt.ttr2.and.r122.gt.ttr2m) then
              ttmp = ttr2
              ttr2 = ttr1
              ttr1 = ttmp
              ttmp = ttr2m
              ttr2m = ttr1m
              ttr1m = ttmp
            else
              cycle ii2loop
            endif
          endif
          r12 = sqrt(r122)
!
!  Inner loop over second site
!
          kloop: do k = j,numat
            nk = nat(k)
            ntypk = nftype(k)
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
!
!  Check k is allowed for n, and not equivalent to j
!
            if (.not.lmatch(nk,ntypk,nto,ntypo,.true.)) cycle kloop
!
!  QM/MM handling
!
            if (QMMMmode(ncf).gt.0) then
              if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1) cycle kloop
            endif
! 
!  Dreiding option handling                 
! 
            if (ltdreiding(n)) then               
              if (.not.bonded2donorJK(i,j,k)) cycle kloop
            endif
!
            if (lspatialok) then
              x31 = xinbox(k) - xc1
              y31 = yinbox(k) - yc1
              z31 = zinbox(k) - zc1
            else
              x31 = xclat(k) - xc1
              y31 = yclat(k) - yc1
              z31 = zclat(k) - zc1
            endif
            if (lmolloc.and.lneedmol) then
!
!  Molecule handling
!
              nmk = natmol(k)
              indmk = nmolind(k)
              izk = (indmk/100) - 5
              ind = indmk - 100*(izk+5)
              iyk = (ind/10) - 5
              ind = ind - 10*(iyk+5)
              ixk = ind - 5
              ixk = ixk - ixi
              iyk = iyk - iyi
              izk = izk - izi
              lmolok2 = (nmi.eq.nmk.and.nmi.gt.0)
            else
              lmolok2 = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok2) cycle kloop
            if (lbtyp.and..not.lmolok2) cycle kloop
!
!  Check r13 is OK
!  Loop over cell vectors
!
            if (j.eq.k) then
              ii3min = ii2 + 1
            else
              ii3min = 1
            endif
            ii3loop: do ii3 = ii3min,nvector
              cell31 = xvec(ii3)
              cell32 = yvec(ii3)
              cell33 = zvec(ii3)
              r13 = (cell31+x31)**2 + (cell32+y31)**2 + (cell33+z31)**2
              if (r13.lt.1.0d-12) cycle ii3loop
!
!  Molecule checking
!
              lbonded  =  .false.
              if (lmolok2) then
                if (ndim.eq.0) then
                  if (linter_only) cycle ii3loop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,nreli,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle ii3loop
                    if (lsymijk) then
                      call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,j,k,0_i4,0_i4,0_i4)
                      if (.not.lbonded) cycle ii3loop
                    endif
                  endif
                else
                  call lintoijk(jxx,jyy,jzz,ii3,imax,jmax,kmax)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,nreli,k,jxx,jyy,jzz)
                    if (.not.lbonded) cycle ii3loop
                    if (lsymijk) then
                      call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,j,k,jxx-ixx,jyy-iyy,jzz-izz)
                      if (.not.lbonded) cycle ii3loop
                    endif
                    lsamemol = (lbonded.or.l2bonds)
                  else
                    lsamemol = .false.
                  endif
                  if (.not.lsamemol) then
                    call samemol(lsamemol,nmi,jxx,jyy,jzz,ixk,iyk,izk)
                  endif
                  if (lintra_only.and..not.lsamemol) cycle ii3loop
                  if (linter_only.and.lsamemol) cycle ii3loop
                endif
              endif
!
!  Distance checking
!
!  Modification to handle case where species for 2 and 3 are the same
!  but cutoffs are different
!
              if ((r13.gt.ttr2.or.r13.lt.ttr2m).and.(.not.lbtyp.or..not.lbonded)) then
                if (ldiff23typ.or..not.ldiff23cut) cycle ii3loop
                if (r122.gt.ttr2.or.r13.gt.ttr1) cycle ii3loop
                if (r122.lt.ttr2m.or.r13.lt.ttr1m) cycle ii3loop
              endif
              if (lbtyp) then
!
!  Bond type checking
!
!  If we've made it this far then the atoms must be bonded. We just have
!  to check if the bond orders match.
!
                lbondtypeOK = .true.
!
!  Check i-j bond for correct order
!
                if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeij) lbondtypeOK = .false.
                if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeij2) lbondtypeOK = .false.
!
!  Check i-k bond for correct order
!
                if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeik) lbondtypeOK = .false.
                if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeik2) lbondtypeOK = .false.
!
                if (.not.lbondtypeOK) then
!
!  If bond types don't match, but atom types are symmetric then try other permutation
!
                  if (ldiff23typ.or..not.ldiff23bo) cycle ii3loop
!
!  Check i-j bond for correct order
!
                  if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeij) cycle ii3loop
                  if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeij2) cycle ii3loop
!
!  Check i-k bond for correct order
! 
                  if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeik) cycle ii3loop
                  if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeik2) cycle ii3loop
                endif
              endif
!
              r13 = sqrt(r13)
!
!  Check r23 is OK
!
              xt31 = x31 + cell31
              yt31 = y31 + cell32
              zt31 = z31 + cell33
              xt21 = x21 + cell21
              yt21 = y21 + cell22
              zt21 = z21 + cell23
              x23 = xt31 - xt21
              y23 = yt31 - yt21
              z23 = zt31 - zt21
              r23 = x23**2 + y23**2 + z23**2
              if (r23.gt.tr3.and..not.lbtyp) cycle ii3loop
              if (r23.lt.tr3m.or.r23.lt.1d-12) cycle ii3loop
!
!  Valid angle
!
              nang = nang + 1
              nangtot = nangtot + 1
!
!  Check array dimensions - if too small, increase maxangle and start again
!
              if (nang.gt.maxangle) then
!
!  Increase maxangle
!
                maxangle  =  2_i4*maxangle
!
!  Free local memory
!
                deallocate(ind3,stat=status)
                if (status/=0) call deallocate_error('angle','ind3')
                deallocate(ind2,stat=status)
                if (status/=0) call deallocate_error('angle','ind2')
                deallocate(ind1,stat=status)
                if (status/=0) call deallocate_error('angle','ind1')
                deallocate(ntp3,stat=status)
                if (status/=0) call deallocate_error('angle','ntp3')
                deallocate(ntp2,stat=status)
                if (status/=0) call deallocate_error('angle','ntp2')
                deallocate(ntp1,stat=status)
                if (status/=0) call deallocate_error('angle','ntp1')
                deallocate(nat3,stat=status)
                if (status/=0) call deallocate_error('angle','nat3')
                deallocate(nat2,stat=status)
                if (status/=0) call deallocate_error('angle','nat2')
                deallocate(nat1,stat=status)
                if (status/=0) call deallocate_error('angle','nat1')
                deallocate(ang,stat=status)
                if (status/=0) call deallocate_error('angle','ang')
!
!  Start again
!
                goto 1
              endif
              dot = xt21*xt31 + yt21*yt31 + zt21*zt31
              dot = dot/(r12*r13)
              if (abs(dot).gt.0.9999999_dp) dot = sign(1.0_dp,dot)
              ang(nang) = radtodeg*acos(dot)
              nat1(nang) = ni
              nat2(nang) = nj
              nat3(nang) = nk
              ntp1(nang) = ntypi
              ntp2(nang) = ntypj
              ntp3(nang) = ntypk
              ind1(nang) = i
              ind2(nang) = j
              ind3(nang) = k
!
!  End of inner loops
!
            enddo ii3loop
          enddo kloop
        enddo ii2loop
      enddo jloop
    enddo iloop
!
!  Write out angle analysis for three body potential n
!
    nline = (nang-1)/2 + 1
    nline2 = nline - 2
    if (nang.eq.0) then
      write(ioout,'(1x,i6,5x,3x,''No angles found'')') n
    elseif (nang.eq.1) then
      call label(nat1(1),ntp1(1),lab1)
      call label(nat2(1),ntp2(1),lab2)
      call label(nat3(1),ntp3(1),lab3)
      cstyp1 = 'c'
      cstyp2 = 'c'
      cstyp3 = 'c'
      if (nat1(1).gt.maxele) cstyp1 = 's'
      if (nat2(1).gt.maxele) cstyp2 = 's'
      if (nat3(1).gt.maxele) cstyp3 = 's'
      write(ioout,'(1x,i6,5x,1x,3(1x,a5,1x,a1),1x,f7.3)') n,lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,ang(1)
    else
      call label(nat1(1),ntp1(1),lab1)
      call label(nat2(1),ntp2(1),lab2)
      call label(nat3(1),ntp3(1),lab3)
      call label(nat1(2),ntp1(2),lab4)
      call label(nat2(2),ntp2(2),lab5)
      call label(nat3(2),ntp3(2),lab6)
      cstyp1 = 'c'
      cstyp2 = 'c'
      cstyp3 = 'c'
      cstyp4 = 'c'
      cstyp5 = 'c'
      cstyp6 = 'c'
      if (nat1(1).gt.maxele) cstyp1 = 's'
      if (nat2(1).gt.maxele) cstyp2 = 's'
      if (nat3(1).gt.maxele) cstyp3 = 's'
      if (nat1(2).gt.maxele) cstyp4 = 's'
      if (nat2(2).gt.maxele) cstyp5 = 's'
      if (nat3(2).gt.maxele) cstyp6 = 's'
      write(ioout,'(1x,i6,5x,2(1x,3(1x,a5,1x,a1),1x,f7.3))') &
        n,lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,ang(1),lab4,cstyp4,lab5,cstyp5,lab6,cstyp6,ang(2)
      if (nline2.ge.1) then
        ind = 0
        do m = 1,nline2
          ind = ind + 2
          call label(nat1(ind+1),ntp1(ind+1),lab1)
          call label(nat2(ind+1),ntp2(ind+1),lab2)
          call label(nat3(ind+1),ntp3(ind+1),lab3)
          call label(nat1(ind+2),ntp1(ind+2),lab4)
          call label(nat2(ind+2),ntp2(ind+2),lab5)
          call label(nat3(ind+2),ntp3(ind+2),lab6)
          cstyp1 = 'c'
          cstyp2 = 'c'
          cstyp3 = 'c'
          cstyp4 = 'c'
          cstyp5 = 'c'
          cstyp6 = 'c'
          if (nat1(ind+1).gt.maxele) cstyp1 = 's'
          if (nat2(ind+1).gt.maxele) cstyp2 = 's'
          if (nat3(ind+1).gt.maxele) cstyp3 = 's'
          if (nat1(ind+2).gt.maxele) cstyp4 = 's'
          if (nat2(ind+2).gt.maxele) cstyp5 = 's'
          if (nat3(ind+2).gt.maxele) cstyp6 = 's'
          write(ioout,'(12x,2(1x,3(1x,a5,1x,a1),1x,f7.3))') &
            lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,ang(ind+1),lab4,cstyp4,lab5,cstyp5,lab6,cstyp6,ang(ind+2)
        enddo
      endif
      if (nang.gt.2) then
        nliner = nang - (nline-1)*2
      else
        nliner = 0
      endif
      if (nliner.eq.1) then
        call label(nat1(nang),ntp1(nang),lab1)
        call label(nat2(nang),ntp2(nang),lab2)
        call label(nat3(nang),ntp3(nang),lab3)
        cstyp1 = 'c'
        cstyp2 = 'c'
        cstyp3 = 'c'
        if (nat1(nang).gt.maxele) cstyp1 = 's'
        if (nat2(nang).gt.maxele) cstyp2 = 's'
        if (nat3(nang).gt.maxele) cstyp3 = 's'
        write(ioout,'(12x,2(1x,3(1x,a5,1x,a1),1x,f7.3))') &
          lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,ang(nang)
      elseif (nliner.eq.2) then
        ind = nang - 2
        call label(nat1(ind+1),ntp1(ind+1),lab1)
        call label(nat2(ind+1),ntp2(ind+1),lab2)
        call label(nat3(ind+1),ntp3(ind+1),lab3)
        call label(nat1(ind+2),ntp1(ind+2),lab4)
        call label(nat2(ind+2),ntp2(ind+2),lab5)
        call label(nat3(ind+2),ntp3(ind+2),lab6)
        cstyp1 = 'c'
        cstyp2 = 'c'
        cstyp3 = 'c'
        cstyp4 = 'c'
        cstyp5 = 'c'
        cstyp6 = 'c'
        if (nat1(ind+1).gt.maxele) cstyp1 = 's'
        if (nat2(ind+1).gt.maxele) cstyp2 = 's'
        if (nat3(ind+1).gt.maxele) cstyp3 = 's'
        if (nat1(ind+2).gt.maxele) cstyp4 = 's'
        if (nat2(ind+2).gt.maxele) cstyp5 = 's'
        if (nat3(ind+2).gt.maxele) cstyp6 = 's'
        write(ioout,'(12x,2(1x,3(1x,a5,1x,a1),1x,f7.3))') lab1,cstyp1, &
            lab2,cstyp2,lab3,cstyp3,ang(ind+1),lab4,cstyp4,lab5,cstyp5,lab6,cstyp6,ang(ind+2)
      endif
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  End of outer loops
!
  enddo pots
  if (nangtot.gt.0) then
    write(ioout,'(''  Total number of angles = '',i8)') nangtot
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  write(ioout,'(/)')
!
!  Free local memory
!
  deallocate(ind3,stat=status)
  if (status/=0) call deallocate_error('angle','ind3')
  deallocate(ind2,stat=status)
  if (status/=0) call deallocate_error('angle','ind2')
  deallocate(ind1,stat=status)
  if (status/=0) call deallocate_error('angle','ind1')
  deallocate(ntp3,stat=status)
  if (status/=0) call deallocate_error('angle','ntp3')
  deallocate(ntp2,stat=status)
  if (status/=0) call deallocate_error('angle','ntp2')
  deallocate(ntp1,stat=status)
  if (status/=0) call deallocate_error('angle','ntp1')
  deallocate(nat3,stat=status)
  if (status/=0) call deallocate_error('angle','nat3')
  deallocate(nat2,stat=status)
  if (status/=0) call deallocate_error('angle','nat2')
  deallocate(nat1,stat=status)
  if (status/=0) call deallocate_error('angle','nat1')
  deallocate(ang,stat=status)
  if (status/=0) call deallocate_error('angle','ang')
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('angle','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('angle','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('angle','xvec')
!
!  Timing
!
  time2 = cputime()
  tthree = tthree + time2 - time1
!
  return
  end
