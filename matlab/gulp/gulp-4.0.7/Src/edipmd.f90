  subroutine EDIPmd(eedip,lgrad1)
!
!  Calculates the energy and derivatives for the EDIP force field.
!  MD version.
!
!  On entry : 
!
!  lgrad1          = if .true. calculate the first derivatives
!
!  On exit :
!
!  eedip           = the value of the energy contribution
!
!   9/10 Created from reaxffmd
!  10/10 Switched to using d1addc to allow for dot products in 
!        torsional term.
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  use configurations, only : nregionno, nregions, lsliceatom, nregiontype, QMMMmode
  use control,        only : keyword, lseok
  use constants,      only : radtodeg
  use current
  use EDIPdata
  use energies,       only : eattach, esregion12, esregion2
  use iochannels
  use neighbours
  use optimisation,   only : lfreeze, lopf
  use parallel,       only : ioproc, nprocs, procid
  use spatialbo
  use symmetry,       only : lstr
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)                       :: eedip
  logical,     intent(in)                        :: lgrad1
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: ijbase             ! Pointer to start of i-j-j_neighbour positions in d1i
  integer(i4)                                    :: indij
  integer(i4)                                    :: itmp
  integer(i4)                                    :: j
  integer(i4)                                    :: k
  integer(i4)                                    :: maxneigh1
  integer(i4)                                    :: maxneigh2          ! Maximum dimension of derivative arrays without torsions
  integer(i4)                                    :: maxneigh3          ! Maximum dimension of derivative arrays with torsions
  integer(i4)                                    :: mneigh
  integer(i4)                                    :: n
  integer(i4)                                    :: nati
  integer(i4)                                    :: njk
  integer(i4)                                    :: nzij
  integer(i4), dimension(:),   allocatable, save :: nzs
  integer(i4), dimension(:),   allocatable, save :: nzsptr
  integer(i4)                                    :: nzatom
  integer(i4), dimension(:),   allocatable, save :: nzatomRptr
  integer(i4)                                    :: ni
  integer(i4)                                    :: nivalid
  integer(i4)                                    :: nj
  integer(i4)                                    :: nk
  integer(i4)                                    :: nl
  integer(i4)                                    :: nm
  integer(i4)                                    :: nmin
  integer(i4)                                    :: nn
  integer(i4)                                    :: nn2
  integer(i4)                                    :: nptr
  integer(i4), dimension(:,:), allocatable, save :: neighno
  integer(i4), dimension(:,:), allocatable, save :: neighnoRptr
  integer(i4), dimension(:),   allocatable, save :: nfreeatom
  integer(i4), dimension(:),   allocatable, save :: nfreeqptr
  integer(i4), dimension(:),   allocatable, save :: nneigh
  integer(i4)                                    :: nneighi1
  integer(i4)                                    :: nneighi2           ! Maximum dimension of derivatives for i without torsions
  integer(i4)                                    :: nneighi3           ! Maximum dimension of derivatives for i with torsions
  integer(i4)                                    :: nregioni           ! Region number for i
  integer(i4)                                    :: nregionj           ! Region number for j
  integer(i4)                                    :: nregiontypi        ! Region type for i
  integer(i4)                                    :: nregiontypj        ! Region type for j
  integer(i4)                                    :: nspeci             ! Species number for i
  integer(i4)                                    :: nspecj             ! Species number for j
  integer(i4)                                    :: nspeck             ! Species number for k
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: numattodo
  integer(i4)                                    :: numattodo_main
  integer(i4), dimension(:),   allocatable, save :: numattodoptr
  integer(i4), dimension(:),   allocatable, save :: numattodoRptr
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: latomdone
  logical                                        :: lattach
  logical                                        :: lPnonzero
  logical                                        :: lZnonzero
  logical                                        :: lfound
  logical                                        :: lmaxneighok
  logical                                        :: lQMMMok
  logical                                        :: lreg2one
  logical                                        :: lreg2pair
  logical                                        :: lslicei
  logical                                        :: lslicej
  logical,     dimension(:),   allocatable, save :: lopanyneigh
  real(dp)                                       :: Ppair
  real(dp)                                       :: Zpair
  real(dp)                                       :: Cdih_ijk
  real(dp)                                       :: Cdih_ijkl
  real(dp)                                       :: Cdih_ijklm
  real(dp)                                       :: Crep3_ijkl
  real(dp)                                       :: Crep2_ijk
  real(dp)                                       :: Crep_ij
  real(dp)                                       :: cputime
  real(dp)                                       :: dotp0
  real(dp)                                       :: dotp1
  real(dp)                                       :: dotp2
  real(dp)                                       :: e2i
  real(dp)                                       :: e3i
  real(dp),    dimension(:,:), allocatable, save :: P                  ! Cut-off P function used in dihedral contributions
  real(dp),    dimension(:,:), allocatable, save :: Z                  ! Pairwise coordination contributions to Zsum
  real(dp),    dimension(:),   allocatable, save :: Zsum               ! Zsum is the coordination number before dihedral correction
  real(dp),    dimension(:,:), allocatable, save :: d1P                ! First derivative of P array
  real(dp),    dimension(:,:), allocatable, save :: d1Z                ! First derivative of Z where is Z is uncorrected
  real(dp),    dimension(:,:), allocatable, save :: d1i                ! First Cartesian derivatives for energy contribution of atom i
  real(dp),    dimension(:,:), allocatable, save :: d1si               ! First strain derivatives for energy contribution of atom i
  real(dp),    dimension(:,:), allocatable, save :: d1Zi               ! First Cartesian derivatives of Z where Z is corrected for atom i
  real(dp),    dimension(:,:), allocatable, save :: d1sZi              ! First strain derivatives of Z where Z is corrected for atom i
  real(dp)                                       :: dPpairdr
  real(dp)                                       :: dZpairdr
  real(dp)                                       :: deijdrij
  real(dp)                                       :: deijdZi
  real(dp)                                       :: de2idrij
  real(dp)                                       :: de2idZi
  real(dp)                                       :: de3idr(3)
  real(dp)                                       :: de3idZi
  real(dp)                                       :: dpi2idZi
  real(dp)                                       :: dpi3idZi
  real(dp)                                       :: dpi0jdZj
  real(dp)                                       :: dpi3jdZj
  real(dp)                                       :: eij
  real(dp)                                       :: expcut
  real(dp)                                       :: pi2i
  real(dp)                                       :: pi3i
  real(dp)                                       :: pi0j
  real(dp)                                       :: pi3j
  real(dp)                                       :: Pij
  real(dp)                                       :: Pik
  real(dp)                                       :: Pil
  real(dp)                                       :: Pjm
  real(dp)                                       :: rij
  real(dp)                                       :: rik
  real(dp)                                       :: ril
  real(dp)                                       :: rjm
  real(dp)                                       :: rrij
  real(dp)                                       :: rrik
  real(dp)                                       :: rril
  real(dp)                                       :: rrjm
  real(dp)                                       :: rtmp
  real(dp)                                       :: scale
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp),    dimension(:,:), allocatable, save :: rneigh
  real(dp),    dimension(:,:), allocatable, save :: xneigh
  real(dp),    dimension(:,:), allocatable, save :: yneigh
  real(dp),    dimension(:,:), allocatable, save :: zneigh
  real(dp)                                       :: xcross
  real(dp)                                       :: ycross
  real(dp)                                       :: zcross
  real(dp)                                       :: xdiff
  real(dp)                                       :: ydiff
  real(dp)                                       :: zdiff
  real(dp)                                       :: Zdih
  real(dp)                                       :: Zrep2
  real(dp)                                       :: Zrep3
  real(dp)                                       :: Zi
  real(dp)                                       :: Zitot
  real(dp)                                       :: Zj
  real(dp)                                       :: ztrm
  real(dp)                                       :: ztrm2
  real(dp)                                       :: xd
  real(dp)                                       :: yd
  real(dp)                                       :: zd
  real(dp)                                       :: xij
  real(dp)                                       :: yij
  real(dp)                                       :: zij
  real(dp)                                       :: xik
  real(dp)                                       :: yik
  real(dp)                                       :: zik
  real(dp)                                       :: xil
  real(dp)                                       :: yil
  real(dp)                                       :: zil
  real(dp)                                       :: xjk
  real(dp)                                       :: yjk
  real(dp)                                       :: zjk
  real(dp)                                       :: xjm
  real(dp)                                       :: yjm
  real(dp)                                       :: zjm
  real(dp)                                       :: Zmax
!
  t1 = cputime()
!
!  Initialise energy
!
  eedip = 0.0_dp
!
  allocate(nzatomRptr(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nzatomRptr')
! 
!  Set up a dummy pointer for derivative array calls
! 
  nzatom = 0
  do i = 1,numat
    nzatom = nzatom + 1
    nzatomRptr(i) = nzatom
  enddo
!
!  Allocate memory that does not depend on maxneigh
!
  allocate(numattodoptr(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','numattodoptr')
  allocate(numattodoRptr(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','numattodoRptr')
  allocate(nzs(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nzs')
  allocate(nzsptr(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nzsptr')
  allocate(latomdone(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','latomdone')
  allocate(lopanyneigh(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','lopanyneigh')
  allocate(nfreeatom(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nfreeatom')
  allocate(nfreeqptr(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nfreeqptr')
  allocate(nneigh(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','nneigh')
  allocate(Zsum(numat),stat=status)
  if (status/=0) call outofmemory('edipmd','Zsum')
!
!  Reinitialisation point should maxneigh be increased
!
100 continue
  lmaxneighok = .true.
  if (allocated(rneigh)) then
    deallocate(d1sZi,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1sZi')
    deallocate(d1Zi,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1Zi')
    deallocate(d1si,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1si')
    deallocate(d1i,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1i')
    deallocate(d1Z,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1Z')
    deallocate(d1P,stat=status)
    if (status/=0) call deallocate_error('edipmd','d1P')
    deallocate(Z,stat=status)
    if (status/=0) call deallocate_error('edipmd','Z')
    deallocate(P,stat=status)
    if (status/=0) call deallocate_error('edipmd','P')
    deallocate(zneigh,stat=status)
    if (status/=0) call deallocate_error('edipmd','zneigh')
    deallocate(yneigh,stat=status)
    if (status/=0) call deallocate_error('edipmd','yneigh')
    deallocate(xneigh,stat=status)
    if (status/=0) call deallocate_error('edipmd','xneigh')
    deallocate(rneigh,stat=status)
    if (status/=0) call deallocate_error('edipmd','rneigh')
    deallocate(neighnoRptr,stat=status)
    if (status/=0) call deallocate_error('edipmd','neighnoRptr')
    deallocate(neighno,stat=status)
    if (status/=0) call deallocate_error('edipmd','neighno')
  endif
!
!  Set parameter for pairwise storage memory
!
!  maxneigh2  is the size of first derivative arrays except d1i
!
  maxneigh1 = maxneigh*(maxneigh + 1)/2
  maxneigh2 = maxneigh + maxneigh1
  maxneigh3 = maxneigh2 + maxneigh*(maxneigh+1)*maxneigh
!
!  Allocate local memory
!
  allocate(neighno(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','neighno')
  allocate(neighnoRptr(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','neighnoRptr')
  allocate(rneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','rneigh')
  allocate(xneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','xneigh')
  allocate(yneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','yneigh')
  allocate(zneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','zneigh')
  allocate(P(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','P')
  allocate(Z(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('edipmd','Z')
!
  if (lgrad1) then
    allocate(d1P(maxneigh,numat),stat=status)
    if (status/=0) call outofmemory('edipmd','d1P')
    allocate(d1Z(maxneigh,numat),stat=status)
    if (status/=0) call outofmemory('edipmd','d1Z')
    allocate(d1i(3,maxneigh3),stat=status)
    if (status/=0) call outofmemory('edipmd','d1i')
    allocate(d1si(6,maxneigh3),stat=status)
    if (status/=0) call outofmemory('edipmd','d1si')
    allocate(d1Zi(3,maxneigh3),stat=status)
    if (status/=0) call outofmemory('edipmd','d1Zi')
    allocate(d1sZi(6,maxneigh3),stat=status)
    if (status/=0) call outofmemory('edipmd','d1sZi')
  else
    allocate(d1P(1,numat),stat=status)
    if (status/=0) call outofmemory('edipmd','d1P')
    allocate(d1Z(1,numat),stat=status)
    if (status/=0) call outofmemory('edipmd','d1Z')
    allocate(d1i(3,1),stat=status)
    if (status/=0) call outofmemory('edipmd','d1i')
    allocate(d1si(6,1),stat=status)
    if (status/=0) call outofmemory('edipmd','d1si')
    allocate(d1Zi(3,1),stat=status)
    if (status/=0) call outofmemory('edipmd','d1Zi')
    allocate(d1sZi(6,1),stat=status)
    if (status/=0) call outofmemory('edipmd','d1sZi')
  endif
!****************************
!  Find list of free atoms  *
!****************************
  if (lfreeze) then
    ii = 0
    do i = 1,numat
      if (lopf(nrelat(i))) then
        ii = ii + 1
        nfreeatom(i) = ii
      else
        nfreeatom(i) = 0
      endif
    enddo
  else
    do i = 1,numat
      nfreeatom(i) = i
    enddo
  endif
!*************************************
!  Find cut-off radii for all atoms  *
!*************************************
  nzsptr(1:numat) = 0
  do i = 1,numat
    nati = nat(i)
    ntypi = nftype(i)
!
!  Check repulsive terms and build atom pointers to species
!
    nzs(i) = 0
    do j = 1,nEDIPspec
      if (nati.eq.natEDIPspec(j).and.(ntypi.eq.ntypEDIPspec(j).or.ntypEDIPspec(j).eq.0)) then
        nzs(i) = nzs(i) + 1
        nzsptr(i) = j
      endif
    enddo
!
!  Check number of species for now
!
    if (nzs(i).gt.1) then
      call outerror('Multiple species per atom not yet allowed for in EDIP',0_i4)
      call stopnow('EDIPmd')
    endif
  enddo
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  call getEDIPneighbour(maxneigh,nzsptr,nneigh,neighno,rneigh, &
                        xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!**********************************************************************
!  If maxneigh has been exceeded, increase value and return to start  *
!**********************************************************************
  if (.not.lmaxneighok) then
    do i = 1,numat
      if (nneigh(i).gt.maxneigh) maxneigh = nneigh(i)
    enddo
    if (index(keyword,'verb').ne.0) then
      write(ioout,'(/,''  Increasing maxneigh to '',i6)') maxneigh
    endif
    goto 100
  endif
!
!  Set pointer to atoms that are needed on this node
!
  if (nprocs.gt.1) then
    numattodo = 0
    numattodoRptr(1:numat) = 0
    do i = 1+procid,numat,nprocs
      numattodo = numattodo + 1
      numattodoptr(numattodo) = i
      numattodoRptr(i) = numattodo
    enddo
!
!  Now include neighbours of atoms
!
    numattodo_main = numattodo
    do ni = 1,numattodo_main
      i = numattodoptr(ni)
      do nj = 1,nneigh(i)
        j = neighno(nj,i)
        if (numattodoRptr(j).eq.0) then
          numattodo = numattodo + 1
          numattodoptr(numattodo) = j
          numattodoRptr(j) = numattodo
        endif
      enddo
    enddo
  else
    numattodo = numat
    numattodo_main = numat
    do i = 1,numat
      numattodoptr(i) = i
      numattodoRptr(i) = i
    enddo
  endif
!*******************************
!  Sort neighbours into order  *
!******************************* 
  if (lspatialboOK) then
    do i = 1,numat
!               
!  Build pointer
!               
      do nn = 1,nneigh(i)
        nmin = numat + 1 
        do nn2 = nn,nneigh(i) 
          if (neighno(nn2,i).lt.nmin) then
            nmin = neighno(nn2,i)
            nptr = nn2  
          endif
        enddo       
!         
!  Sort quantities
!
        if (nptr.ne.nn) then
          itmp = neighno(nptr,i)
          neighno(nptr,i) = neighno(nn,i)
          neighno(nn,i)  = itmp
          rtmp = rneigh(nptr,i)
          rneigh(nptr,i) = rneigh(nn,i)
          rneigh(nn,i)  = rtmp
          rtmp = xneigh(nptr,i)
          xneigh(nptr,i) = xneigh(nn,i)
          xneigh(nn,i)  = rtmp
          rtmp = yneigh(nptr,i)
          yneigh(nptr,i) = yneigh(nn,i)
          yneigh(nn,i)  = rtmp
          rtmp = zneigh(nptr,i)
          zneigh(nptr,i) = zneigh(nn,i)
          zneigh(nn,i)  = rtmp
        endif  
      enddo         
    enddo
  endif
!*********************************************
!  Calculate numbers of neighbours for each  *
!*********************************************
  do i = 1,numat
!
!  Set initial value for lopanyneigh - this variable indicates whether an atom has 
!  any neighbours for which derivatives are required
!
    if (.not.lfreeze) then
      lopanyneigh(i) = .true.
    else
      lopanyneigh(i) = lopf(nrelat(i))
    endif
    do n = 1,nneigh(i)
      j = neighno(n,i)
      rij = rneigh(n,i)
!
!  Check whether atom is free to optimise
!
      if (lopf(nrelat(j))) then
        lopanyneigh(i) = .true.
      endif
    enddo
  enddo
  if (index(keyword,'debu').ne.0) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(/,''  Number of neighbours for atoms :'',/)')
      write(ioout,'(''  i'',2x,''Nat'',3x,''N'')')
    endif
    call mpbarrier
    do ii = 1,numattodo_main
      i = numattodoptr(ii)
      write(ioout,'(i4,1x,i3,1x,i6)') i,nat(i),nneigh(i)
    enddo
    call mpbarrier
    if (ioproc) then
      write(ioout,'(/,''  Neighbours of atoms :'',/)')
    endif
    call mpbarrier
    do ii = 1,numattodo_main
      i = numattodoptr(ii)
      write(ioout,'(i4,8(1x,i4))') i,(neighno(nn,i),nn=1,nneigh(i))
    enddo
    call mpbarrier
  endif
!************************************************************
!  Loop over pairs of atoms to compute coordination number  *
!************************************************************
!
!  mneigh contains the real value of maxneigh need after removing neighbours
!  whose bond order tolerance is below the allowed threshold
!
  mneigh = 0
  Zmax = 0.0_dp
  do ii = 1,numattodo
    i = numattodoptr(ii)
!
!  Set variables relating to i
!
    if (nzs(i).gt.0) then
      nspeci = nzsptr(i)
!
!  Loop over neighbours of i 
!
      Zsum(i) = 0.0_dp
      do ni = 1,nneigh(i)
        j = neighno(ni,i)
!
!  Does j have a bond order species type?
!
        if (nzs(j).gt.0) then
!
!  Set variables relating to j
!
          nspecj = nzsptr(j)
!
!  Set up i-j quantities
!
          rij = rneigh(ni,i)
!
!  Set index to parameters for pairwise interaction of species
!
          if (nspeci.ge.nspecj) then
            nzij = nspeci*(nspeci - 1)/2 + nspecj
          else
            nzij = nspecj*(nspecj - 1)/2 + nspeci
          endif
          if (rij.lt.EDIPrmaxpair(nzij)) then
!*******************************************
!  Valid coordination number contribution  *
!*******************************************
            call EDIPcnpair(nzij,rij,Zpair,dZpairdr,lgrad1,lZnonzero)
            if (lZnonzero) then
              Z(ni,i) = Zpair
              if (lgrad1) then
                rrij = 1.0_dp/rij
                d1Z(ni,i) = rrij*dZpairdr
              endif
              Zsum(i) = Zsum(i) + Z(ni,i)
            else
              Z(ni,i) = 0.0_dp
              if (lgrad1) then
                d1Z(ni,i) = 0.0_dp
              endif
            endif
          else
            Z(ni,i) = 0.0_dp
            if (lgrad1) then
              d1Z(ni,i) = 0.0_dp
            endif
          endif
!
          if (rij.lt.EDIPrmaxpair(nzij)) then
!**************************************
!  Valid torsion cutoff contribution  *
!**************************************
            call EDIPppair(nzij,rij,Ppair,dPpairdr,lgrad1,lPnonzero)
            if (lPnonzero) then
              P(ni,i) = Ppair
              if (lgrad1) then
                rrij = 1.0_dp/rij
                d1P(ni,i) = rrij*dPpairdr
              endif
            else
              P(ni,i) = 0.0_dp
              if (lgrad1) then
                d1P(ni,i) = 0.0_dp
              endif
            endif
          else
            P(ni,i) = 0.0_dp
            if (lgrad1) then
              d1P(ni,i) = 0.0_dp
            endif
          endif
        endif
      enddo
    endif
    Zmax = max(Zmax,Zsum(i))
!
!  End loop over atoms i
!
  enddo
!
!  Check Zmax against cut-off Zmax
!
  if (Zmax.gt.EDIPmaxZcutoff) then
    EDIPmaxZcutoff = Zmax
    call setedip
    goto 100
  endif
!
!  Having computed the coordination number, screen neighbours based on cut-off distance
!
  do ii = 1,numattodo
    i = numattodoptr(ii)
!
!  Set variables relating to i
!
    if (nzs(i).gt.0) then
      nspeci = nzsptr(i)
      nivalid = 0
      do ni = 1,nneigh(i)
        j = neighno(ni,i)
!
!  Does j have a bond order species type?
!
        if (nzs(j).gt.0) then
          nspecj = nzsptr(j)
          if (nspeci.ge.nspecj) then
            nzij = nspeci*(nspeci - 1)/2 + nspecj
          else
            nzij = nspecj*(nspecj - 1)/2 + nspeci
          endif
          expcut = EDIP2a(nzij) + EDIP2aprime(nzij)*max(Zsum(i),Zsum(j)) + EDIP2sigma(nzij)*EDIPaccuracy2drmax
          rij = rneigh(ni,i)
          lPnonzero = (abs(P(ni,i)).gt.0.0_dp)
          lZnonzero = (abs(Z(ni,i)).gt.0.0_dp)
          if (lZnonzero.or.lPnonzero.or.rij.lt.expcut) then
!
!  Valid terms found - move terms to correct location
!
            nivalid = nivalid + 1
            if (nivalid.ne.ni) then
              neighno(nivalid,i) = neighno(ni,i)
              rneigh(nivalid,i) = rneigh(ni,i)
              xneigh(nivalid,i) = xneigh(ni,i)
              yneigh(nivalid,i) = yneigh(ni,i)
              zneigh(nivalid,i) = zneigh(ni,i)
              P(nivalid,i) = P(ni,i)
              Z(nivalid,i) = Z(ni,i)
              if (lgrad1) then
                d1P(nivalid,i) = d1P(ni,i)
                d1Z(nivalid,i) = d1Z(ni,i)
              endif
            endif
          endif
        endif
      enddo
!  
!  Now reset number of neighbours to reflect the number that have non-zero bond orders
!           
      nneigh(i) = nivalid
      mneigh = max(mneigh,nivalid)
    endif
!
!  End loop over atoms i
!
  enddo
!
!  Set neighnoRptr
!
  do ii = 1,numattodo
    i = numattodoptr(ii)
!
    if (nzs(i).gt.0) then
!
!  Loop over neighbours of i 
!
      nivalid = 0
      do ni = 1,nneigh(i)
        j = neighno(ni,i)
!
!  Does j have a bond order species type?
!
        if (nzs(j).gt.0) then
!
!  Set up i-j quantities
!
          rij = rneigh(ni,i)
          xij = xneigh(ni,i)
          yij = yneigh(ni,i)
          zij = zneigh(ni,i)
!
!  Find i in neighbour list for j
!
          nj = 1
          lfound = .false.
          do while (nj.le.nneigh(j).and..not.lfound)
            if (neighno(nj,j).eq.i) then
              xdiff = xneigh(nj,j) + xij
              ydiff = yneigh(nj,j) + yij
              zdiff = zneigh(nj,j) + zij
              lfound = ((abs(xdiff)+abs(ydiff)+abs(zdiff)).lt.1.0d-6)
            endif
            if (.not.lfound) nj = nj + 1
          enddo
          if (lfound) then
            neighnoRptr(ni,i) = nj
          else
            call outerror('neighbour lists are inconsistent in EDIP',0_i4)
            call stopnow('EDIPmd')
          endif
        endif
      enddo
    endif
!
!  End loop over atoms i
!
  enddo
  if (index(keyword,'debu').ne.0) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(/,''  Number of neighbours for atoms after coordination screening:'',/)')
      write(ioout,'(''  i'',2x,''Nat'',3x,''N'')')
    endif
    call mpbarrier
    do ii = 1,numattodo_main
      i = numattodoptr(ii)
      write(ioout,'(i4,1x,i3,1x,i6)') i,nat(i),nneigh(i)
    enddo
    call mpbarrier
  endif
  if (index(keyword,'verb').ne.0) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(/,''  EDIP: Coordination number : '',/)')
    endif
    call mpbarrier
    do ii = 1,numattodo_main
      i = numattodoptr(ii)
      write(ioout,'(i4,6(1x,i4,1x,f7.4))') i,(neighno(j,i),Z(j,i),j=1,nneigh(i))
    enddo
  endif
!*****************************
!  Loop over pairs of atoms  *
!*****************************
  iloop: do ii = 1,numattodo_main
    i = numattodoptr(ii)
!
!  If this atom has no ReaxFF terms then there is nothing to do...
!
    if (nzs(i).eq.0) cycle iloop
!
!  Set variables relating to i
!
    nspeci = nzsptr(i)
    nregioni = nregionno(nsft + nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
    lslicei = lsliceatom(nsft + nrelat(i))
    Zi = Zsum(i)
!
!  Set total number of distances for neighbours of i
!
    nneighi1 = nneigh(i)*(nneigh(i) + 1)/2 
    nneighi2 = nneigh(i) + nneighi1
    nneighi3 = nneighi2 + nneigh(i)*(mneigh+1)*mneigh
!
!  Initialise derivative storage for neighbours of i
!
    if (lgrad1) then
      d1i(1:3,1:nneighi3) = 0.0_dp
      d1Zi(1:3,1:nneighi3) = 0.0_dp
      if (lstr) then
        d1si(1:6,1:nneighi3) = 0.0_dp
        d1sZi(1:6,1:nneighi3) = 0.0_dp
      endif
!
!  Add d1Z to d1Zi
!
      do ni = 1,nneigh(i)
        xd = xneigh(ni,i)
        yd = yneigh(ni,i)
        zd = zneigh(ni,i)
        d1Zi(1,ni) = xd*d1Z(ni,i)
        d1Zi(2,ni) = yd*d1Z(ni,i)
        d1Zi(3,ni) = zd*d1Z(ni,i)
        if (lstr) then
          if (ndim.eq.3) then
            d1sZi(1,ni) = xd*xd*d1Z(ni,i)
            d1sZi(2,ni) = yd*yd*d1Z(ni,i)
            d1sZi(3,ni) = zd*zd*d1Z(ni,i)
            d1sZi(4,ni) = yd*zd*d1Z(ni,i)
            d1sZi(5,ni) = xd*zd*d1Z(ni,i)
            d1sZi(6,ni) = xd*yd*d1Z(ni,i)
          elseif (ndim.eq.2) then
            d1sZi(1,ni) = xd*xd*d1Z(ni,i)
            d1sZi(2,ni) = yd*yd*d1Z(ni,i)
            d1sZi(3,ni) = xd*yd*d1Z(ni,i)
          elseif (ndim.eq.1) then
            d1sZi(1,ni) = xd*xd*d1Z(ni,i)
          endif
        endif
      enddo
    endif
!
!  Loop over quartets of atoms to compute coordination number correction
!
    Zitot = Zi
!
!  Is pi_2 or pi_3 for i > 0 ?
!
    call EDIP_pi2(Zi,pi2i,dpi2idZi,lgrad1)
    call EDIP_pi3(Zi,pi3i,dpi3idZi,lgrad1)
    if (pi2i.gt.0.0_dp.or.pi3i.gt.0.0_dp) then
!
!  Loop over neighbours of i 
!
      do ni = 1,nneigh(i)
!
!  Is p > 0 for i-j?
!
        Pij = P(ni,i)
        if (Pij.gt.0.0_dp) then
!
!  Set variables relating to j
!
          j = neighno(ni,i)
          nspecj = nzsptr(j)
          Zj = Zsum(j)
          nj = neighnoRptr(ni,i)
          xij = xneigh(ni,i)
          yij = yneigh(ni,i)
          zij = zneigh(ni,i)
          rij = rneigh(ni,i)
          rrij = 1.0_dp/rij
!
!  Set up i-j quantities
!
          rij = rneigh(ni,i)
          if (nspeci.ge.nspecj) then
            indij = nspeci*(nspeci - 1)/2 + nspecj
          else
            indij = nspecj*(nspecj - 1)/2 + nspeci
          endif
          Crep_ij = ((rij - EDIPc0(indij))**2)*(1.0_dp - Pij)
!
!  Create pointer to based of i-j-j_neighbour position in d1i
!
          ijbase = nneighi2 + (ni - 1)*mneigh*(mneigh + 1) + ni*mneigh
!
!  Is pi or pi_3 for j > 0 ?
!
          call EDIP_pi(Zj,pi0j,dpi0jdZj,lgrad1)
          call EDIP_pi3(Zj,pi3j,dpi3jdZj,lgrad1)
!
          if (pi0j.gt.0.0_dp.or.pi3j.gt.0.0_dp) then
!
!  Loop over neighbours of i other than j
!
            do nk = 1,nneigh(i)
!
!  Is p > 0 for i-k?
!
              Pik = P(nk,i)
              if (Pik.gt.0.0_dp.and.nk.ne.ni) then
                k = neighno(nk,i)
                xik = xneigh(nk,i)
                yik = yneigh(nk,i)
                zik = zneigh(nk,i)
                rik = rneigh(nk,i)
                rrik = 1.0_dp/rik
                Cdih_ijk = Pij*Pik
                Crep2_ijk = Crep_ij*Pik
!
!  Compute dot product of i-j and i-k
!
                dotp0 = (xij*xik + yij*yik + zij*zik)*rrij*rrik
!
!  Calculate repulsive coordination contribution, Zrep2
!
                Zrep2 = pi2i*pi0j*EDIPZrep(indij)*Crep2_ijk*(1.0_dp - dotp0*dotp0)
!
!  Add coordination contribution to total
!
                Zitot = Zitot + Zrep2
!
                if (lgrad1) then
!
!  Add derivatives of coordination number:
!
!  Derivatives of Crep2_ijk
!
                  ztrm = pi2i*pi0j*EDIPZrep(indij)*(1.0_dp - dotp0*dotp0)
!
                  ztrm2 = ztrm*Pik*(rij - EDIPc0(indij))*(2.0_dp*rrij*(1.0_dp-Pij) - (rij - EDIPc0(indij))*d1P(ni,i))
                  d1Zi(1,ni) = d1Zi(1,ni) + ztrm2*xij
                  d1Zi(2,ni) = d1Zi(2,ni) + ztrm2*yij
                  d1Zi(3,ni) = d1Zi(3,ni) + ztrm2*zij
                  if (lstr) then
                    if (ndim.eq.3) then
                      d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                      d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                      d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*zij*zij
                      d1sZi(4,ni) = d1sZi(4,ni) + ztrm2*yij*zij
                      d1sZi(5,ni) = d1sZi(5,ni) + ztrm2*xij*zij
                      d1sZi(6,ni) = d1sZi(6,ni) + ztrm2*xij*yij
                    elseif (ndim.eq.2) then
                      d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                      d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                      d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*xij*yij
                    elseif (ndim.eq.1) then
                      d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                    endif
                  endif
!
                  ztrm2 = ztrm*Crep_ij*d1P(nk,i)
                  d1Zi(1,nk) = d1Zi(1,nk) + ztrm2*xik
                  d1Zi(2,nk) = d1Zi(2,nk) + ztrm2*yik
                  d1Zi(3,nk) = d1Zi(3,nk) + ztrm2*zik
                  if (lstr) then
                    if (ndim.eq.3) then
                      d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                      d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                      d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*zik*zik
                      d1sZi(4,nk) = d1sZi(4,nk) + ztrm2*yik*zik
                      d1sZi(5,nk) = d1sZi(5,nk) + ztrm2*xik*zik
                      d1sZi(6,nk) = d1sZi(6,nk) + ztrm2*xik*yik
                    elseif (ndim.eq.2) then
                      d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                      d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                      d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*xik*yik
                    elseif (ndim.eq.1) then
                      d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                    endif
                  endif
!
!  Derivatives of dotp0
!
                  ztrm = - 2.0_dp*pi2i*pi0j*EDIPZrep(indij)*Crep2_ijk*dotp0
!  Derivatives w.r.t. i-j
                  ztrm2 = ztrm*dotp0*rrij*rrij
                  d1Zi(1,ni) = d1Zi(1,ni) - ztrm2*xij + ztrm*xik*rrij*rrik
                  d1Zi(2,ni) = d1Zi(2,ni) - ztrm2*yij + ztrm*yik*rrij*rrik
                  d1Zi(3,ni) = d1Zi(3,ni) - ztrm2*zij + ztrm*zik*rrij*rrik
                  if (lstr) then
                    if (ndim.eq.3) then
                      d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*rrik*xik*xij
                      d1sZi(2,ni) = d1sZi(2,ni) - ztrm2*yij*yij + ztrm*rrij*rrik*yik*yij
                      d1sZi(3,ni) = d1sZi(3,ni) - ztrm2*zij*zij + ztrm*rrij*rrik*zik*zij
                      d1sZi(4,ni) = d1sZi(4,ni) - ztrm2*yij*zij + 0.5_dp*ztrm*rrij*rrik*(yik*zij + zik*yij)
                      d1sZi(5,ni) = d1sZi(5,ni) - ztrm2*xij*zij + 0.5_dp*ztrm*rrij*rrik*(xik*zij + zik*xij)
                      d1sZi(6,ni) = d1sZi(6,ni) - ztrm2*xij*yij + 0.5_dp*ztrm*rrij*rrik*(yik*xij + xik*yij)
                    elseif (ndim.eq.2) then
                      d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*rrik*xik*xij
                      d1sZi(2,ni) = d1sZi(2,ni) - ztrm2*yij*yij + ztrm*rrij*rrik*yik*yij
                      d1sZi(3,ni) = d1sZi(3,ni) - ztrm2*xij*yij + 0.5_dp*ztrm*rrij*rrik*(yik*xij + xik*yij)
                    elseif (ndim.eq.1) then
                      d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*rrik*xik*xij
                    endif
                  endif
!  Derivatives w.r.t. i-k
                  ztrm2 = ztrm*dotp0*rrik*rrik
                  d1Zi(1,nk) = d1Zi(1,nk) - ztrm2*xik + ztrm*xij*rrik*rrij
                  d1Zi(2,nk) = d1Zi(2,nk) - ztrm2*yik + ztrm*yij*rrik*rrij
                  d1Zi(3,nk) = d1Zi(3,nk) - ztrm2*zik + ztrm*zij*rrik*rrij
                  if (lstr) then
                    if (ndim.eq.3) then
                      d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*xik*xij
                      d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrij*rrik*yik*yij
                      d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*zik*zik + ztrm*rrij*rrik*zik*zij
                      d1sZi(4,nk) = d1sZi(4,nk) - ztrm2*yik*zik + 0.5_dp*ztrm*rrij*rrik*(yik*zij + zik*yij)
                      d1sZi(5,nk) = d1sZi(5,nk) - ztrm2*xik*zik + 0.5_dp*ztrm*rrij*rrik*(xik*zij + zik*xij)
                      d1sZi(6,nk) = d1sZi(6,nk) - ztrm2*xik*yik + 0.5_dp*ztrm*rrij*rrik*(yik*xij + xik*yij)
                    elseif (ndim.eq.2) then
                      d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*xik*xij
                      d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrij*rrik*yik*yij
                      d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*xik*yik + 0.5_dp*ztrm*rrij*rrik*(yik*xij + xik*yij)
                    elseif (ndim.eq.1) then
                      d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*xik*xij
                    endif
                  endif
!
!  Derivatives of pi2i term : Zrep2
!
                  ztrm = pi0j*EDIPZrep(indij)*Crep2_ijk*(1.0_dp - dotp0*dotp0)*dpi2idZi
                  do nn = 1,nneigh(i)
                    xd = xneigh(nn,i)
                    yd = yneigh(nn,i)
                    zd = zneigh(nn,i)
                    d1Zi(1,nn) = d1Zi(1,nn) + ztrm*d1Z(nn,i)*xd
                    d1Zi(2,nn) = d1Zi(2,nn) + ztrm*d1Z(nn,i)*yd
                    d1Zi(3,nn) = d1Zi(3,nn) + ztrm*d1Z(nn,i)*zd
                    if (lstr) then
                      if (ndim.eq.3) then
                        d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                        d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                        d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*zd*zd
                        d1sZi(4,nn) = d1sZi(4,nn) + ztrm*d1Z(nn,i)*yd*zd
                        d1sZi(5,nn) = d1sZi(5,nn) + ztrm*d1Z(nn,i)*xd*zd
                        d1sZi(6,nn) = d1sZi(6,nn) + ztrm*d1Z(nn,i)*xd*yd
                      elseif (ndim.eq.2) then
                        d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                        d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                        d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*xd*yd
                      elseif (ndim.eq.1) then
                        d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                      endif
                    endif
                  enddo
!
!  Derivatives of pi0j term : Zrep2
!
                  ztrm = pi2i*EDIPZrep(indij)*Crep2_ijk*(1.0_dp - dotp0*dotp0)*dpi0jdZj
                  do nn = 1,nneigh(j)
                    xd = xneigh(nn,j)
                    yd = yneigh(nn,j)
                    zd = zneigh(nn,j)
                    d1Zi(1,ijbase+nn) = d1Zi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd
                    d1Zi(2,ijbase+nn) = d1Zi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd
                    d1Zi(3,ijbase+nn) = d1Zi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd
                    if (lstr) then
                      if (ndim.eq.3) then
                        d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                        d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                        d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd*zd
                        d1sZi(4,ijbase+nn) = d1sZi(4,ijbase+nn) + ztrm*d1Z(nn,j)*yd*zd
                        d1sZi(5,ijbase+nn) = d1sZi(5,ijbase+nn) + ztrm*d1Z(nn,j)*xd*zd
                        d1sZi(6,ijbase+nn) = d1sZi(6,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                      elseif (ndim.eq.2) then
                        d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                        d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                        d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                      elseif (ndim.eq.1) then
                        d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                      endif
                    endif
                  enddo
                endif
!
!  Loop over neighbours of i other than j and k
!
                do nl = nk+1,nneigh(i)
!
!  Is p > 0 for i-l?
!
                  Pil = P(nl,i)
                  if (Pil.gt.0.0_dp.and.nl.ne.ni) then
                    xil = xneigh(nl,i)
                    yil = yneigh(nl,i)
                    zil = zneigh(nl,i)
                    ril = rneigh(nl,i)
                    rril = 1.0_dp/ril
                    Cdih_ijkl = Cdih_ijk*Pil
                    Crep3_ijkl = Crep_ij*Pik*Pil
!
!  Compute cross product of i-k and i-l
!
                    xcross = (yik*zil - yil*zik)*rrik*rril
                    ycross = (xil*zik - xik*zil)*rrik*rril
                    zcross = (xik*yil - xil*yik)*rrik*rril
!
!  Compute dot product for Zrep3
!
                    dotp2 = (xij*xcross + yij*ycross + zij*zcross)*rrij
!
!  Calculate repulsive coordination contribution, Zrep3
!
                    Zrep3 = pi3i*pi0j*EDIPZrep(indij)*Crep3_ijkl*dotp2*dotp2
!
!  Add coordination contribution to total
!
                    Zitot = Zitot + Zrep3
                    if (lgrad1) then
!
!  Add derivatives of coordination number:
!
!  Derivatives of Crep3_ijkl
!
                      ztrm = pi3i*pi0j*EDIPZrep(indij)*dotp2*dotp2
!
                      ztrm2 = ztrm*Pik*Pil*(rij - EDIPc0(indij))*(2.0_dp*rrij*(1.0_dp-Pij) - (rij - EDIPc0(indij))*d1P(ni,i))
                      d1Zi(1,ni) = d1Zi(1,ni) + ztrm2*xij
                      d1Zi(2,ni) = d1Zi(2,ni) + ztrm2*yij
                      d1Zi(3,ni) = d1Zi(3,ni) + ztrm2*zij
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                          d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                          d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*zij*zij
                          d1sZi(4,ni) = d1sZi(4,ni) + ztrm2*yij*zij
                          d1sZi(5,ni) = d1sZi(5,ni) + ztrm2*xij*zij
                          d1sZi(6,ni) = d1sZi(6,ni) + ztrm2*xij*yij
                        elseif (ndim.eq.2) then
                          d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                          d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                          d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*xij*yij
                        elseif (ndim.eq.1) then
                          d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                        endif
                      endif
!
                      ztrm2 = ztrm*Crep_ij*Pil*d1P(nk,i)
                      d1Zi(1,nk) = d1Zi(1,nk) + ztrm2*xik
                      d1Zi(2,nk) = d1Zi(2,nk) + ztrm2*yik
                      d1Zi(3,nk) = d1Zi(3,nk) + ztrm2*zik
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                          d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                          d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*zik*zik
                          d1sZi(4,nk) = d1sZi(4,nk) + ztrm2*yik*zik
                          d1sZi(5,nk) = d1sZi(5,nk) + ztrm2*xik*zik
                          d1sZi(6,nk) = d1sZi(6,nk) + ztrm2*xik*yik
                        elseif (ndim.eq.2) then
                          d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                          d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                          d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*xik*yik
                        elseif (ndim.eq.1) then
                          d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                        endif
                      endif
!
                      ztrm2 = ztrm*Crep_ij*Pik*d1P(nl,i)
                      d1Zi(1,nl) = d1Zi(1,nl) + ztrm2*xil
                      d1Zi(2,nl) = d1Zi(2,nl) + ztrm2*yil
                      d1Zi(3,nl) = d1Zi(3,nl) + ztrm2*zil
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                          d1sZi(2,nl) = d1sZi(2,nl) + ztrm2*yil*yil
                          d1sZi(3,nl) = d1sZi(3,nl) + ztrm2*zil*zil
                          d1sZi(4,nl) = d1sZi(4,nl) + ztrm2*yil*zil
                          d1sZi(5,nl) = d1sZi(5,nl) + ztrm2*xil*zil
                          d1sZi(6,nl) = d1sZi(6,nl) + ztrm2*xil*yil
                        elseif (ndim.eq.2) then
                          d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                          d1sZi(2,nl) = d1sZi(2,nl) + ztrm2*yil*yil
                          d1sZi(3,nl) = d1sZi(3,nl) + ztrm2*xil*yil
                        elseif (ndim.eq.1) then
                          d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                        endif
                      endif
!
!  Derivatives of dotp2
!
                      ztrm = 2.0_dp*pi3i*pi0j*EDIPZrep(indij)*Crep3_ijkl*dotp2
!  Derivatives w.r.t. i-j
                      ztrm2 = ztrm*dotp2*rrij*rrij
                      d1Zi(1,ni) = d1Zi(1,ni) - ztrm2*xij + ztrm*xcross*rrij
                      d1Zi(2,ni) = d1Zi(2,ni) - ztrm2*yij + ztrm*ycross*rrij
                      d1Zi(3,ni) = d1Zi(3,ni) - ztrm2*zij + ztrm*zcross*rrij
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*xcross*xij
                          d1sZi(2,ni) = d1sZi(2,ni) - ztrm2*yij*yij + ztrm*rrij*ycross*yij
                          d1sZi(3,ni) = d1sZi(3,ni) - ztrm2*zij*zij + ztrm*rrij*zcross*zij
                          d1sZi(4,ni) = d1sZi(4,ni) - ztrm2*yij*zij + 0.5_dp*ztrm*rrij*(ycross*zij + zcross*yij)
                          d1sZi(5,ni) = d1sZi(5,ni) - ztrm2*xij*zij + 0.5_dp*ztrm*rrij*(xcross*zij + zcross*xij)
                          d1sZi(6,ni) = d1sZi(6,ni) - ztrm2*xij*yij + 0.5_dp*ztrm*rrij*(xcross*yij + ycross*xij)
                        elseif (ndim.eq.2) then
                          d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*xcross*xij
                          d1sZi(2,ni) = d1sZi(2,ni) - ztrm2*yij*yij + ztrm*rrij*ycross*yij
                          d1sZi(3,ni) = d1sZi(3,ni) - ztrm2*xij*yij + 0.5_dp*ztrm*rrij*(xcross*yij + ycross*xij)
                        elseif (ndim.eq.1) then
                          d1sZi(1,ni) = d1sZi(1,ni) - ztrm2*xij*xij + ztrm*rrij*xcross*xij
                        endif
                      endif
!  Derivatives w.r.t. i-k
                      ztrm2 = ztrm*dotp2*rrik*rrik
                      d1Zi(1,nk) = d1Zi(1,nk) - ztrm2*xik + ztrm*(yil*zij - zil*yij)*rrik*rril*rrij
                      d1Zi(2,nk) = d1Zi(2,nk) - ztrm2*yik + ztrm*(zil*xij - xil*zij)*rrik*rril*rrij
                      d1Zi(3,nk) = d1Zi(3,nk) - ztrm2*zik + ztrm*(xil*yij - yil*xij)*rrik*rril*rrij
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*rril*xik*(yil*zij - zil*yij)
                          d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrij*rrik*rril*yik*(zil*xij - xil*zij)
                          d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*zik*zik + ztrm*rrij*rrik*rril*zik*(xil*yij - yil*xij)
                          d1sZi(4,nk) = d1sZi(4,nk) - ztrm2*yik*zik + &
                            0.5_dp*ztrm*rrij*rrik*rril*(xij*(zik*zil - yik*yil) + xil*(yik*yij - zik*zij))
                          d1sZi(5,nk) = d1sZi(5,nk) - ztrm2*xik*zik + &
                            0.5_dp*ztrm*rrij*rrik*rril*(yij*(xik*xil - zik*zil) + yil*(zik*zij - xik*xij))
                          d1sZi(6,nk) = d1sZi(6,nk) - ztrm2*xik*yik + &
                            0.5_dp*ztrm*rrij*rrik*rril*(zij*(yik*yil - xik*xil) + zil*(xik*xij - yik*yij))
                        elseif (ndim.eq.2) then
                          d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*rril*xik*(yil*zij - zil*yij)
                          d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrij*rrik*rril*yik*(zil*xij - xil*zij)
                          d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*xik*yik + &
                            0.5_dp*ztrm*rrij*rrik*rril*(zij*(yik*yil - xik*xil) + zil*(xik*xij - yik*yij))
                        elseif (ndim.eq.1) then
                          d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrij*rrik*rril*xik*(yil*zij - zil*yij)
                        endif
                      endif
!  Derivatives w.r.t. i-l
                      ztrm2 = ztrm*dotp2*rril*rril
                      d1Zi(1,nl) = d1Zi(1,nl) - ztrm2*xil + ztrm*(zik*yij - yik*zij)*rrik*rril*rrij
                      d1Zi(2,nl) = d1Zi(2,nl) - ztrm2*yil + ztrm*(xik*zij - zik*xij)*rrik*rril*rrij
                      d1Zi(3,nl) = d1Zi(3,nl) - ztrm2*zil + ztrm*(yik*xij - xik*yij)*rrik*rril*rrij
                      if (lstr) then
                        if (ndim.eq.3) then
                          d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrij*rrik*rril*xil*(zik*yij - yik*zij)
                          d1sZi(2,nl) = d1sZi(2,nl) - ztrm2*yil*yil + ztrm*rrij*rrik*rril*yil*(xik*zij - zik*xij)
                          d1sZi(3,nl) = d1sZi(3,nl) - ztrm2*zil*zil + ztrm*rrij*rrik*rril*zil*(yik*xij - xik*yij)
                          d1sZi(4,nl) = d1sZi(4,nl) - ztrm2*yil*zil + &
                            0.5_dp*ztrm*rrij*rrik*rril*(xij*(yik*yil - zik*zil) + xik*(zil*zij - yil*yij))
                          d1sZi(5,nl) = d1sZi(5,nl) - ztrm2*xil*zil + &
                            0.5_dp*ztrm*rrij*rrik*rril*(yij*(zik*zil - xik*xil) + yik*(xil*xij - zil*zij))
                          d1sZi(6,nl) = d1sZi(6,nl) - ztrm2*xil*yil + &
                            0.5_dp*ztrm*rrij*rrik*rril*(zij*(xik*xil - yik*yil) + zik*(yil*yij - xil*xij))
                        elseif (ndim.eq.2) then
                          d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrij*rrik*rril*xil*(zik*yij - yik*zij)
                          d1sZi(2,nl) = d1sZi(2,nl) - ztrm2*yil*yil + ztrm*rrij*rrik*rril*yil*(xik*zij - zik*xij)
                          d1sZi(3,nl) = d1sZi(3,nl) - ztrm2*xil*yil + &
                            0.5_dp*ztrm*rrij*rrik*rril*(zij*(xik*xil - yik*yil) + zik*(yil*yij - xil*xij))
                        elseif (ndim.eq.1) then
                          d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrij*rrik*rril*xil*(zik*yij - yik*zij)
                        endif
                      endif
!
!  Derivatives of pi3i term : Zrep3
!
                      ztrm = pi0j*EDIPZrep(indij)*Crep3_ijkl*dotp2*dotp2*dpi3idZi
                      do nn = 1,nneigh(i)
                        xd = xneigh(nn,i)
                        yd = yneigh(nn,i)
                        zd = zneigh(nn,i)
                        d1Zi(1,nn) = d1Zi(1,nn) + ztrm*d1Z(nn,i)*xd
                        d1Zi(2,nn) = d1Zi(2,nn) + ztrm*d1Z(nn,i)*yd
                        d1Zi(3,nn) = d1Zi(3,nn) + ztrm*d1Z(nn,i)*zd
                        if (lstr) then
                          if (ndim.eq.3) then
                            d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                            d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                            d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*zd*zd
                            d1sZi(4,nn) = d1sZi(4,nn) + ztrm*d1Z(nn,i)*yd*zd
                            d1sZi(5,nn) = d1sZi(5,nn) + ztrm*d1Z(nn,i)*xd*zd
                            d1sZi(6,nn) = d1sZi(6,nn) + ztrm*d1Z(nn,i)*xd*yd
                          elseif (ndim.eq.2) then
                            d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                            d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                            d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*xd*yd
                          elseif (ndim.eq.1) then
                            d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                          endif
                        endif
                      enddo
!
!  Derivatives of pi0j term : Zrep3
!
                      ztrm = pi3i*EDIPZrep(indij)*Crep3_ijkl*dotp2*dotp2*dpi0jdZj
                      do nn = 1,nneigh(j)
                        xd = xneigh(nn,j)
                        yd = yneigh(nn,j)
                        zd = zneigh(nn,j)
                        d1Zi(1,ijbase+nn) = d1Zi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd
                        d1Zi(2,ijbase+nn) = d1Zi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd
                        d1Zi(3,ijbase+nn) = d1Zi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd
                        if (lstr) then
                          if (ndim.eq.3) then
                            d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                            d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                            d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd*zd
                            d1sZi(4,ijbase+nn) = d1sZi(4,ijbase+nn) + ztrm*d1Z(nn,j)*yd*zd
                            d1sZi(5,ijbase+nn) = d1sZi(5,ijbase+nn) + ztrm*d1Z(nn,j)*xd*zd
                            d1sZi(6,ijbase+nn) = d1sZi(6,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                          elseif (ndim.eq.2) then
                            d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                            d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                            d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                          elseif (ndim.eq.1) then
                            d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                          endif
                        endif
                      enddo
!
!  End of Zrep3 derivatives
!
                    endif
!
!  Loop over neighbours of j ne i
!
                    do nm = 1,nneigh(j)
!
!  Is p > 0 for j-m?
!
                      Pjm = P(nm,j)
                      if (Pjm.gt.0.0_dp.and.nm.ne.nj) then
                        xjm = xneigh(nm,j)
                        yjm = yneigh(nm,j)
                        zjm = zneigh(nm,j)
                        rjm = rneigh(nm,j)
                        rrjm = 1.0_dp/rjm
                        Cdih_ijklm = Cdih_ijkl*Pjm
!
!  Compute dot products
!
                        dotp1 = (xjm*xcross + yjm*ycross + zjm*zcross)*rrjm
!
!  Calculate dihedral coordination contribution, Zdih
!
                        Zdih = pi3i*pi3j*EDIPZdih(indij)*Cdih_ijklm*dotp1*dotp1
!
!  Add coordination contributions to total
!
                        Zitot = Zitot + Zdih 
                        if (lgrad1) then
!
!  Add derivatives of coordination number:
!
!  Derivatives of Cdih_ijklm
!
                          ztrm = pi3i*pi3j*EDIPZdih(indij)*dotp1*dotp1
                          ztrm2 = ztrm*Pik*Pil*Pjm*d1P(ni,i)
                          d1Zi(1,ni) = d1Zi(1,ni) + ztrm2*xij
                          d1Zi(2,ni) = d1Zi(2,ni) + ztrm2*yij
                          d1Zi(3,ni) = d1Zi(3,ni) + ztrm2*zij
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                              d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                              d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*zij*zij
                              d1sZi(4,ni) = d1sZi(4,ni) + ztrm2*yij*zij
                              d1sZi(5,ni) = d1sZi(5,ni) + ztrm2*xij*zij
                              d1sZi(6,ni) = d1sZi(6,ni) + ztrm2*xij*yij
                            elseif (ndim.eq.2) then
                              d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                              d1sZi(2,ni) = d1sZi(2,ni) + ztrm2*yij*yij
                              d1sZi(3,ni) = d1sZi(3,ni) + ztrm2*xij*yij
                            elseif (ndim.eq.1) then
                              d1sZi(1,ni) = d1sZi(1,ni) + ztrm2*xij*xij
                            endif
                          endif
!
                          ztrm2 = ztrm*Pij*Pil*Pjm*d1P(nk,i)
                          d1Zi(1,nk) = d1Zi(1,nk) + ztrm2*xik
                          d1Zi(2,nk) = d1Zi(2,nk) + ztrm2*yik
                          d1Zi(3,nk) = d1Zi(3,nk) + ztrm2*zik
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                              d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                              d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*zik*zik
                              d1sZi(4,nk) = d1sZi(4,nk) + ztrm2*yik*zik
                              d1sZi(5,nk) = d1sZi(5,nk) + ztrm2*xik*zik
                              d1sZi(6,nk) = d1sZi(6,nk) + ztrm2*xik*yik
                            elseif (ndim.eq.2) then
                              d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                              d1sZi(2,nk) = d1sZi(2,nk) + ztrm2*yik*yik
                              d1sZi(3,nk) = d1sZi(3,nk) + ztrm2*xik*yik
                            elseif (ndim.eq.1) then
                              d1sZi(1,nk) = d1sZi(1,nk) + ztrm2*xik*xik
                            endif
                          endif
!
                          ztrm2 = ztrm*Pij*Pik*Pjm*d1P(nl,i)
                          d1Zi(1,nl) = d1Zi(1,nl) + ztrm2*xil
                          d1Zi(2,nl) = d1Zi(2,nl) + ztrm2*yil
                          d1Zi(3,nl) = d1Zi(3,nl) + ztrm2*zil
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                              d1sZi(2,nl) = d1sZi(2,nl) + ztrm2*yil*yil
                              d1sZi(3,nl) = d1sZi(3,nl) + ztrm2*zil*zil
                              d1sZi(4,nl) = d1sZi(4,nl) + ztrm2*yil*zil
                              d1sZi(5,nl) = d1sZi(5,nl) + ztrm2*xil*zil
                              d1sZi(6,nl) = d1sZi(6,nl) + ztrm2*xil*yil
                            elseif (ndim.eq.2) then
                              d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                              d1sZi(2,nl) = d1sZi(2,nl) + ztrm2*yil*yil
                              d1sZi(3,nl) = d1sZi(3,nl) + ztrm2*xil*yil
                            elseif (ndim.eq.1) then
                              d1sZi(1,nl) = d1sZi(1,nl) + ztrm2*xil*xil
                            endif
                          endif
!
                          ztrm2 = ztrm*Pij*Pik*Pil*d1P(nm,j)
                          nn = ijbase + nm
                          d1Zi(1,nn) = d1Zi(1,nn) + ztrm2*xjm
                          d1Zi(2,nn) = d1Zi(2,nn) + ztrm2*yjm
                          d1Zi(3,nn) = d1Zi(3,nn) + ztrm2*zjm
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nn) = d1sZi(1,nn) + ztrm2*xjm*xjm
                              d1sZi(2,nn) = d1sZi(2,nn) + ztrm2*yjm*yjm
                              d1sZi(3,nn) = d1sZi(3,nn) + ztrm2*zjm*zjm
                              d1sZi(4,nn) = d1sZi(4,nn) + ztrm2*yjm*zjm
                              d1sZi(5,nn) = d1sZi(5,nn) + ztrm2*xjm*zjm
                              d1sZi(6,nn) = d1sZi(6,nn) + ztrm2*xjm*yjm
                            elseif (ndim.eq.2) then
                              d1sZi(1,nn) = d1sZi(1,nn) + ztrm2*xjm*xjm
                              d1sZi(2,nn) = d1sZi(2,nn) + ztrm2*yjm*yjm
                              d1sZi(3,nn) = d1sZi(3,nn) + ztrm2*xjm*yjm
                            elseif (ndim.eq.1) then
                              d1sZi(1,nn) = d1sZi(1,nn) + ztrm2*xjm*xjm
                            endif
                          endif
!
!  Derivatives of dotp1
!
                          ztrm = 2.0_dp*pi3i*pi3j*EDIPZdih(indij)*Cdih_ijklm*dotp1
!  Derivatives w.r.t. j-m
                          ztrm2 = ztrm*dotp1*rrjm*rrjm
                          nn = ijbase + nm
                          d1Zi(1,nn) = d1Zi(1,nn) - ztrm2*xjm + ztrm*xcross*rrjm
                          d1Zi(2,nn) = d1Zi(2,nn) - ztrm2*yjm + ztrm*ycross*rrjm
                          d1Zi(3,nn) = d1Zi(3,nn) - ztrm2*zjm + ztrm*zcross*rrjm
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nn) = d1sZi(1,nn) - ztrm2*xjm*xjm + ztrm*rrjm*xcross*xjm
                              d1sZi(2,nn) = d1sZi(2,nn) - ztrm2*yjm*yjm + ztrm*rrjm*ycross*yjm
                              d1sZi(3,nn) = d1sZi(3,nn) - ztrm2*zjm*zjm + ztrm*rrjm*zcross*zjm
                              d1sZi(4,nn) = d1sZi(4,nn) - ztrm2*yjm*zjm + 0.5_dp*ztrm*rrjm*(ycross*zjm + zcross*yjm)
                              d1sZi(5,nn) = d1sZi(5,nn) - ztrm2*xjm*zjm + 0.5_dp*ztrm*rrjm*(xcross*zjm + zcross*xjm)
                              d1sZi(6,nn) = d1sZi(6,nn) - ztrm2*xjm*yjm + 0.5_dp*ztrm*rrjm*(xcross*yjm + ycross*xjm)
                            elseif (ndim.eq.2) then
                              d1sZi(1,nn) = d1sZi(1,nn) - ztrm2*xjm*xjm + ztrm*rrjm*xcross*xjm
                              d1sZi(2,nn) = d1sZi(2,nn) - ztrm2*yjm*yjm + ztrm*rrjm*ycross*yjm
                              d1sZi(3,nn) = d1sZi(3,nn) - ztrm2*xjm*yjm + 0.5_dp*ztrm*rrjm*(xcross*yjm + ycross*xjm)
                            elseif (ndim.eq.1) then
                              d1sZi(1,nn) = d1sZi(1,nn) - ztrm2*xjm*xjm + ztrm*rrjm*xcross*xjm
                            endif
                          endif
!  Derivatives w.r.t. i-k
                          ztrm2 = ztrm*dotp1*rrik*rrik
                          d1Zi(1,nk) = d1Zi(1,nk) - ztrm2*xik + ztrm*(yil*zjm - zil*yjm)*rrik*rril*rrjm
                          d1Zi(2,nk) = d1Zi(2,nk) - ztrm2*yik + ztrm*(zil*xjm - xil*zjm)*rrik*rril*rrjm
                          d1Zi(3,nk) = d1Zi(3,nk) - ztrm2*zik + ztrm*(xil*yjm - yil*xjm)*rrik*rril*rrjm
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrik*rril*rrjm*xik*(yil*zjm - zil*yjm)
                              d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrik*rril*rrjm*yik*(zil*xjm - xil*zjm)
                              d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*zik*zik + ztrm*rrik*rril*rrjm*zik*(xil*yjm - yil*xjm)
                              d1sZi(4,nk) = d1sZi(4,nk) - ztrm2*yik*zik + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(xjm*(zik*zil - yik*yil) + xil*(yik*yjm - zik*zjm))
                              d1sZi(5,nk) = d1sZi(5,nk) - ztrm2*xik*zik + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(yjm*(xik*xil - zik*zil) + yil*(zik*zjm - xik*xjm))
                              d1sZi(6,nk) = d1sZi(6,nk) - ztrm2*xik*yik + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(zjm*(yik*yil - xik*xil) + zil*(xik*xjm - yik*yjm))
                            elseif (ndim.eq.2) then
                              d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrik*rril*rrjm*xik*(yil*zjm - zil*yjm)
                              d1sZi(2,nk) = d1sZi(2,nk) - ztrm2*yik*yik + ztrm*rrik*rril*rrjm*yik*(zil*xjm - xil*zjm)
                              d1sZi(3,nk) = d1sZi(3,nk) - ztrm2*xik*yik + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(zjm*(yik*yil - xik*xil) + zil*(xik*xjm - yik*yjm))
                            elseif (ndim.eq.1) then
                              d1sZi(1,nk) = d1sZi(1,nk) - ztrm2*xik*xik + ztrm*rrik*rril*rrjm*xik*(yil*zjm - zil*yjm)
                            endif
                          endif
!  Derivatives w.r.t. i-l
                          ztrm2 = ztrm*dotp1*rril*rril
                          d1Zi(1,nl) = d1Zi(1,nl) - ztrm2*xil + ztrm*(zik*yjm - yik*zjm)*rrik*rril*rrjm
                          d1Zi(2,nl) = d1Zi(2,nl) - ztrm2*yil + ztrm*(xik*zjm - zik*xjm)*rrik*rril*rrjm
                          d1Zi(3,nl) = d1Zi(3,nl) - ztrm2*zil + ztrm*(yik*xjm - xik*yjm)*rrik*rril*rrjm
                          if (lstr) then
                            if (ndim.eq.3) then
                              d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrik*rril*rrjm*xil*(zik*yjm - yik*zjm)
                              d1sZi(2,nl) = d1sZi(2,nl) - ztrm2*yil*yil + ztrm*rrik*rril*rrjm*yil*(xik*zjm - zik*xjm)
                              d1sZi(3,nl) = d1sZi(3,nl) - ztrm2*zil*zil + ztrm*rrik*rril*rrjm*zil*(yik*xjm - xik*yjm)
                              d1sZi(4,nl) = d1sZi(4,nl) - ztrm2*yil*zil + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(xjm*(yik*yil - zik*zil) + xik*(zil*zjm - yil*yjm))
                              d1sZi(5,nl) = d1sZi(5,nl) - ztrm2*xil*zil + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(yjm*(zik*zil - xik*xil) + yik*(xil*xjm - zil*zjm))
                              d1sZi(6,nl) = d1sZi(6,nl) - ztrm2*xil*yil + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(zjm*(xik*xil - yik*yil) + zik*(yil*yjm - xil*xjm))
                            elseif (ndim.eq.2) then
                              d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrik*rril*rrjm*xil*(zik*yjm - yik*zjm)
                              d1sZi(2,nl) = d1sZi(2,nl) - ztrm2*yil*yil + ztrm*rrik*rril*rrjm*yil*(xik*zjm - zik*xjm)
                              d1sZi(3,nl) = d1sZi(3,nl) - ztrm2*xil*yil + &
                                0.5_dp*ztrm*rrik*rril*rrjm*(zjm*(xik*xil - yik*yil) + zik*(yil*yjm - xil*xjm))
                            elseif (ndim.eq.1) then
                              d1sZi(1,nl) = d1sZi(1,nl) - ztrm2*xil*xil + ztrm*rrik*rril*rrjm*xil*(zik*yjm - yik*zjm)
                            endif
                          endif
!
!  Derivatives of pi3i term : Zdih
!
                          ztrm = pi3j*EDIPZdih(indij)*Cdih_ijklm*dotp1*dotp1*dpi3idZi
                          do nn = 1,nneigh(i)
                            xd = xneigh(nn,i)
                            yd = yneigh(nn,i)
                            zd = zneigh(nn,i)
                            d1Zi(1,nn) = d1Zi(1,nn) + ztrm*d1Z(nn,i)*xd
                            d1Zi(2,nn) = d1Zi(2,nn) + ztrm*d1Z(nn,i)*yd
                            d1Zi(3,nn) = d1Zi(3,nn) + ztrm*d1Z(nn,i)*zd
                            if (lstr) then
                              if (ndim.eq.3) then
                                d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                                d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                                d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*zd*zd
                                d1sZi(4,nn) = d1sZi(4,nn) + ztrm*d1Z(nn,i)*yd*zd
                                d1sZi(5,nn) = d1sZi(5,nn) + ztrm*d1Z(nn,i)*xd*zd
                                d1sZi(6,nn) = d1sZi(6,nn) + ztrm*d1Z(nn,i)*xd*yd
                              elseif (ndim.eq.2) then
                                d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                                d1sZi(2,nn) = d1sZi(2,nn) + ztrm*d1Z(nn,i)*yd*yd
                                d1sZi(3,nn) = d1sZi(3,nn) + ztrm*d1Z(nn,i)*xd*yd
                              elseif (ndim.eq.1) then
                                d1sZi(1,nn) = d1sZi(1,nn) + ztrm*d1Z(nn,i)*xd*xd
                              endif
                            endif
                          enddo
!
!  Derivatives of pi3j term : Zdih
!
                          ztrm = pi3i*EDIPZdih(indij)*Cdih_ijklm*dotp1*dotp1*dpi3jdZj
                          do nn = 1,nneigh(j)
                            xd = xneigh(nn,j)
                            yd = yneigh(nn,j)
                            zd = zneigh(nn,j)
                            d1Zi(1,ijbase+nn) = d1Zi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd
                            d1Zi(2,ijbase+nn) = d1Zi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd
                            d1Zi(3,ijbase+nn) = d1Zi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd
                            if (lstr) then
                              if (ndim.eq.3) then
                                d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                                d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                                d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*zd*zd
                                d1sZi(4,ijbase+nn) = d1sZi(4,ijbase+nn) + ztrm*d1Z(nn,j)*yd*zd
                                d1sZi(5,ijbase+nn) = d1sZi(5,ijbase+nn) + ztrm*d1Z(nn,j)*xd*zd
                                d1sZi(6,ijbase+nn) = d1sZi(6,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                              elseif (ndim.eq.2) then
                                d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                                d1sZi(2,ijbase+nn) = d1sZi(2,ijbase+nn) + ztrm*d1Z(nn,j)*yd*yd
                                d1sZi(3,ijbase+nn) = d1sZi(3,ijbase+nn) + ztrm*d1Z(nn,j)*xd*yd
                              elseif (ndim.eq.1) then
                                d1sZi(1,ijbase+nn) = d1sZi(1,ijbase+nn) + ztrm*d1Z(nn,j)*xd*xd
                              endif
                            endif
                          enddo
                        endif
                      endif
                    enddo
!
!  End of check on p > 0 for i-l
!
                  endif
                enddo
!
!  End of check on p > 0 for i-k
!
              endif
            enddo
!
!  End of check on pi_3 for j > 0
!
          endif
        endif
      enddo
!
!  End of check on pi_3 for i > 0
!
    endif
!****************************************************
!  Compute energy terms using corrected bond order  *
!****************************************************
!
!  Loop over neighbours of i (=> j)
!
    do ni = 1,nneigh(i)
      j = neighno(ni,i)
      nregionj = nregionno(nsft + nrelat(j))
      nregiontypj = nregiontype(nregionj,ncf)
!
!  QM/MM handling : i & j are both QM atoms => exclude
!
      lQMMMok = .true.
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.nregiontypj.eq.1) lQMMMok = .false.
      endif
!
!  Do we need to do this pair of atoms
!
      if (lQMMMok.and.nzs(j).gt.0) then
!
!  If i = j set scale to be half to correct for double counting
!
        if (i.eq.j) then
          scale = 0.5_dp
        else
          scale = 1.0_dp
        endif
!
!  Set variables relating to j
!
        nspecj = nzsptr(j)
        lslicej = lsliceatom(nsft + nrelat(j))
!
!  Set up i-j quantities
!
        lreg2one  = .false.
        lreg2pair = .false.
        if (lseok.and.nregions(ncf).gt.1) then
          lreg2pair = (nregioni.gt.1.and.nregionj.gt.1)
          if (.not.lreg2pair) lreg2one = (nregioni.gt.1.or.nregionj.gt.1)
        endif
        lattach = (lslicei.and..not.lslicej.or.lslicej.and..not.lslicei)
!
!  Find i in neighbour list for j
!
        nj = neighnoRptr(ni,i)
!
!  Set index to parameters for pairwise interaction of species
!
        if (nspeci.ge.nspecj) then
          nzij = nspeci*(nspeci - 1)/2 + nspecj
        else
          nzij = nspecj*(nspecj - 1)/2 + nspeci
        endif
        rij = rneigh(ni,i)
        xij = xneigh(ni,i)
        yij = yneigh(ni,i)
        zij = zneigh(ni,i)
!*************************************
!  Valid pairwise EDIP contribution  *
!*************************************
        call EDIP_twobody(nzij,rij,Zitot,e2i,de2idrij,de2idZi,lgrad1)
!
!  Calculate total i-j potential
!
        eij = scale*e2i
!
!  Add to surface energy totals if appropriate
!
        if (lseok) then
          if (lreg2one) then
            esregion12 = esregion12 + eij
          elseif (lreg2pair) then
            esregion2 = esregion2 + eij
          else
            eedip = eedip + eij
          endif
        else
          eedip = eedip + eij
        endif
        if (lattach) eattach = eattach + eij
!
!  Derivatives of Bond Order potential energy
!
        if (lgrad1) then
          deijdrij = scale*de2idrij
          d1i(1,ni) = d1i(1,ni) + deijdrij*xij
          d1i(2,ni) = d1i(2,ni) + deijdrij*yij
          d1i(3,ni) = d1i(3,ni) + deijdrij*zij
          if (lstr) then
            if (ndim.eq.3) then
              d1si(1,ni) = d1si(1,ni) + deijdrij*xij*xij
              d1si(2,ni) = d1si(2,ni) + deijdrij*yij*yij
              d1si(3,ni) = d1si(3,ni) + deijdrij*zij*zij
              d1si(4,ni) = d1si(4,ni) + deijdrij*yij*zij
              d1si(5,ni) = d1si(5,ni) + deijdrij*xij*zij
              d1si(6,ni) = d1si(6,ni) + deijdrij*xij*yij
            elseif (ndim.eq.2) then
              d1si(1,ni) = d1si(1,ni) + deijdrij*xij*xij
              d1si(2,ni) = d1si(2,ni) + deijdrij*yij*yij
              d1si(3,ni) = d1si(3,ni) + deijdrij*xij*yij
            elseif (ndim.eq.1) then
              d1si(1,ni) = d1si(1,ni) + deijdrij*xij*xij
            endif
          endif
!
          deijdZi = scale*de2idZi
          do nn = 1,nneighi3
            d1i(1:3,nn) = d1i(1:3,nn) + deijdZi*d1Zi(1:3,nn) 
          enddo
          if (lstr) then
            do nn = 1,nneighi3
              d1si(1:nstrains,nn) = d1si(1:nstrains,nn) + deijdZi*d1sZi(1:nstrains,nn) 
            enddo
          endif
        endif
!*********************************
!  Three-body EDIP contribution  *
!*********************************
!
!  Loop over neighbours of i .ne. j 
!
        do nk = ni+1,nneigh(i)
          k = neighno(nk,i)
          nspeck = nzsptr(k)
          rik = rneigh(nk,i)
          xik = xneigh(nk,i)
          yik = yneigh(nk,i)
          zik = zneigh(nk,i)
!
!  Compute three-body term
!
          call EDIP_threebody(nspeci,nspecj,nspeck,xij,yij,zij,rij,xik,yik,zik,rik,Zitot,e3i,de3idr,de3idZi,lgrad1)
!
!  Add on energy contribution
!
          eedip = eedip + e3i
!
          if (lgrad1) then
!
!  Derivatives
!
!  Find index for j-k 
!
            if (ni.ge.nk) then
              njk = nneigh(i) + ni*(ni-1)/2 + nk
            else
              njk = nneigh(i) + nk*(nk-1)/2 + ni
            endif
!
            d1i(1,ni) = d1i(1,ni) + de3idr(1)*xij
            d1i(2,ni) = d1i(2,ni) + de3idr(1)*yij
            d1i(3,ni) = d1i(3,ni) + de3idr(1)*zij
            if (lstr) then
              if (ndim.eq.3) then
                d1si(1,ni) = d1si(1,ni) + de3idr(1)*xij*xij
                d1si(2,ni) = d1si(2,ni) + de3idr(1)*yij*yij
                d1si(3,ni) = d1si(3,ni) + de3idr(1)*zij*zij
                d1si(4,ni) = d1si(4,ni) + de3idr(1)*yij*zij
                d1si(5,ni) = d1si(5,ni) + de3idr(1)*xij*zij
                d1si(6,ni) = d1si(6,ni) + de3idr(1)*xij*yij
              elseif (ndim.eq.2) then
                d1si(1,ni) = d1si(1,ni) + de3idr(1)*xij*xij
                d1si(2,ni) = d1si(2,ni) + de3idr(1)*yij*yij
                d1si(3,ni) = d1si(3,ni) + de3idr(1)*xij*yij
              elseif (ndim.eq.1) then
                d1si(1,ni) = d1si(1,ni) + de3idr(1)*xij*xij
              endif
            endif
!
            d1i(1,nk) = d1i(1,nk) + de3idr(2)*xik
            d1i(2,nk) = d1i(2,nk) + de3idr(2)*yik
            d1i(3,nk) = d1i(3,nk) + de3idr(2)*zik
            if (lstr) then
              if (ndim.eq.3) then
                d1si(1,nk) = d1si(1,nk) + de3idr(2)*xik*xik
                d1si(2,nk) = d1si(2,nk) + de3idr(2)*yik*yik
                d1si(3,nk) = d1si(3,nk) + de3idr(2)*zik*zik
                d1si(4,nk) = d1si(4,nk) + de3idr(2)*yik*zik
                d1si(5,nk) = d1si(5,nk) + de3idr(2)*xik*zik
                d1si(6,nk) = d1si(6,nk) + de3idr(2)*xik*yik
              elseif (ndim.eq.2) then
                d1si(1,nk) = d1si(1,nk) + de3idr(2)*xik*xik
                d1si(2,nk) = d1si(2,nk) + de3idr(2)*yik*yik
                d1si(3,nk) = d1si(3,nk) + de3idr(2)*xik*yik
              elseif (ndim.eq.1) then
                d1si(1,nk) = d1si(1,nk) + de3idr(2)*xik*xik
              endif
            endif
!
            xjk = xik - xij
            yjk = yik - yij
            zjk = zik - zij
            d1i(1,njk) = d1i(1,njk) + de3idr(3)*xjk
            d1i(2,njk) = d1i(2,njk) + de3idr(3)*yjk
            d1i(3,njk) = d1i(3,njk) + de3idr(3)*zjk
            if (lstr) then
              if (ndim.eq.3) then
                d1si(1,njk) = d1si(1,njk) + de3idr(3)*xjk*xjk
                d1si(2,njk) = d1si(2,njk) + de3idr(3)*yjk*yjk
                d1si(3,njk) = d1si(3,njk) + de3idr(3)*zjk*zjk
                d1si(4,njk) = d1si(4,njk) + de3idr(3)*yjk*zjk
                d1si(5,njk) = d1si(5,njk) + de3idr(3)*xjk*zjk
                d1si(6,njk) = d1si(6,njk) + de3idr(3)*xjk*yjk
              elseif (ndim.eq.2) then
                d1si(1,njk) = d1si(1,njk) + de3idr(3)*xjk*xjk
                d1si(2,njk) = d1si(2,njk) + de3idr(3)*yjk*yjk
                d1si(3,njk) = d1si(3,njk) + de3idr(3)*xjk*yjk
              elseif (ndim.eq.1) then
                d1si(1,njk) = d1si(1,njk) + de3idr(3)*xjk*xjk
              endif
            endif
!
            do nn = 1,nneighi3
              d1i(1:3,nn) = d1i(1:3,nn) + de3idZi*d1Zi(1:3,nn)
            enddo
            if (lstr) then
              do nn = 1,nneighi3
                d1si(1:nstrains,nn) = d1si(1:nstrains,nn) + de3idZi*d1sZi(1:nstrains,nn)
              enddo
            endif
          endif
!
!  End of three-body terms
!
        enddo
!
!  End condition section on i or j being associated with moving atom
!
      endif
    enddo
!
!  Add derivatives due to neighbours of i
!
    if (lgrad1.and.ii.le.numattodo_main) then
      call d1addc(i,maxneigh,mneigh,nneigh,neighno,nfreeatom,nzatomRptr,d1i,d1si,.true.)
    endif
  enddo iloop
!
!  Free local memory
!
  deallocate(d1sZi,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1sZi')
  deallocate(d1Zi,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1Zi')
  deallocate(d1si,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1si')
  deallocate(d1i,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1i')
  deallocate(d1Z,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1Z')
  deallocate(d1P,stat=status)
  if (status/=0) call deallocate_error('edipmd','d1P')
  deallocate(Z,stat=status)
  if (status/=0) call deallocate_error('edipmd','Z')
  deallocate(P,stat=status)
  if (status/=0) call deallocate_error('edipmd','P')
  deallocate(zneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','zneigh')
  deallocate(yneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','yneigh')
  deallocate(xneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','xneigh')
  deallocate(rneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','rneigh')
  deallocate(neighnoRptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','neighnoRptr')
  deallocate(neighno,stat=status)
  if (status/=0) call deallocate_error('edipmd','neighno')
  deallocate(Zsum,stat=status)
  if (status/=0) call deallocate_error('edipmd','Zsum')
  deallocate(nneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','nneigh')
  deallocate(nfreeqptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','nfreeqptr')
  deallocate(nfreeatom,stat=status)
  if (status/=0) call deallocate_error('edipmd','nfreeatom')
  deallocate(lopanyneigh,stat=status)
  if (status/=0) call deallocate_error('edipmd','lopanyneigh')
  deallocate(latomdone,stat=status)
  if (status/=0) call deallocate_error('edipmd','latomdone')
  deallocate(nzsptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','nzsptr')
  deallocate(nzs,stat=status)
  if (status/=0) call deallocate_error('edipmd','nzs')
  deallocate(numattodoRptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','numattodoRptr')
  deallocate(numattodoptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','numattodoptr')
  deallocate(nzatomRptr,stat=status)
  if (status/=0) call deallocate_error('edipmd','nzatomRptr')
!
  t2 = cputime()
  tedip = tedip + t2 - t1
!
  return
  end
