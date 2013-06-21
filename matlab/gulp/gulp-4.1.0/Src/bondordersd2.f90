  subroutine bondordersd2(ebondorder,lgrad1,lgrad2)
!
!  Calculates the energy and derivatives for the Bond Order potentials.
!  Symmetry adapted version.
!
!  On entry : 
!
!  lgrad1          = if .true. calculate the first derivatives
!  lgrad2          = if .true. calculate the second derivatives
!
!  On exit :
!
!  ebondorder      = the value of the energy contribution
!
!  11/03 Created from bondorder.f
!  12/03 Maxneigh can now be dynamically changed
!   6/04 M coefficient added for attractive/repulsive terms
!   6/04 Calculation of bondorder corrected
!   9/04 Separate spatial decomposition added for BO potentials
!  10/04 Neighbour list now determined in subroutine
!   2/07 Unused variables removed
!   6/07 nboatom and pointers added as dummys for calls to d1add/d2add
!  11/07 Unused variables cleaned up
!   4/08 Call to d1add modified
!   6/10 Modified to handle optional second atom type for bond-order
!        attractive and repulsive terms.
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
!  Julian Gale, NRI, Curtin University, June 2010
!
  use datatypes
  use bondorderdata
  use control,        only : keyword
  use current
  use iochannels
  use neighbours
  use optimisation,   only : lfreeze, lopf
  use spatialbo,      only : lspatialok => lspatialBOok
  use symmetry,       only : lstr
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)                       :: ebondorder
  logical,     intent(in)                        :: lgrad1
  logical,     intent(in)                        :: lgrad2
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: itmp
  integer(i4)                                    :: j
  integer(i4)                                    :: k
  integer(i4)                                    :: maxneigh2
  integer(i4)                                    :: maxneigh22
  integer(i4)                                    :: mA
  integer(i4)                                    :: mR
  integer(i4)                                    :: n
  integer(i4)                                    :: nati
  integer(i4)                                    :: natj
  integer(i4)                                    :: nboij
  integer(i4)                                    :: nboAij
  integer(i4)                                    :: nboAji
  integer(i4)                                    :: nboRij
  integer(i4)                                    :: nboRji
  integer(i4), dimension(:,:), allocatable, save :: nbopotptr
  integer(i4)                                    :: nboatom
  integer(i4), dimension(:),   allocatable, save :: nboatomRptr
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: nik
  integer(i4)                                    :: njk
  integer(i4)                                    :: nmin
  integer(i4)                                    :: nn
  integer(i4)                                    :: nn1
  integer(i4)                                    :: nn2
  integer(i4), dimension(:,:), allocatable, save :: neighno
  integer(i4), dimension(:),   allocatable, save :: nauatom
  integer(i4), dimension(:),   allocatable, save :: nfreeatom
  integer(i4), dimension(:),   allocatable, save :: nfreeatomau
  integer(i4), dimension(:),   allocatable, save :: nneigh
  integer(i4)                                    :: nneighi2
  integer(i4)                                    :: nneighj2
  integer(i4)                                    :: nneighi22
  integer(i4)                                    :: nneighj22
  integer(i4)                                    :: npki
  integer(i4)                                    :: npkj
  integer(i4)                                    :: nptr
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: ntypj
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: lanyau
  logical,     dimension(:),   allocatable, save :: latomdone
  logical                                        :: lfound
  logical                                        :: lmaxneighok
  logical,     dimension(:),   allocatable, save :: lopanyneigh
  real(dp)                                       :: bijA
  real(dp)                                       :: bijR
  real(dp)                                       :: bjiA
  real(dp)                                       :: bjiR
  real(dp)                                       :: bijsumA
  real(dp)                                       :: bijsumA1
  real(dp)                                       :: bijsumAn1
  real(dp)                                       :: bijsumR
  real(dp)                                       :: bijsumR1
  real(dp)                                       :: bijsumRn1
  real(dp)                                       :: bjisumA
  real(dp)                                       :: bjisumA1
  real(dp)                                       :: bjisumAn1
  real(dp)                                       :: bjisumR
  real(dp)                                       :: bjisumR1
  real(dp)                                       :: bjisumRn1
  real(dp)                                       :: btotA
  real(dp)                                       :: btotR
  real(dp)                                       :: cputime
  real(dp),    dimension(:),   allocatable, save :: d1i
  real(dp),    dimension(:),   allocatable, save :: d1j
  real(dp),    dimension(:),   allocatable, save :: d2i
  real(dp),    dimension(:),   allocatable, save :: d2j
  real(dp),    dimension(:),   allocatable, save :: d1BtotiA
  real(dp),    dimension(:),   allocatable, save :: d1BtotiR
  real(dp),    dimension(:),   allocatable, save :: d1BtotjA
  real(dp),    dimension(:),   allocatable, save :: d1BtotjR
  real(dp),    dimension(:),   allocatable, save :: d2BtotiA
  real(dp),    dimension(:),   allocatable, save :: d2BtotiR
  real(dp),    dimension(:),   allocatable, save :: d2BtotjA
  real(dp),    dimension(:),   allocatable, save :: d2BtotjR
  real(dp)                                       :: dexpijkdr
  real(dp)                                       :: dexpjikdr
  real(dp)                                       :: d2expijkdr2
  real(dp)                                       :: d2expjikdr2
  real(dp)                                       :: dfdr
  real(dp)                                       :: dfikdr
  real(dp)                                       :: dfjkdr
  real(dp)                                       :: d2fdr2
  real(dp)                                       :: d2fikdr2
  real(dp)                                       :: d2fjkdr2
  real(dp)                                       :: d3fdr3
  real(dp)                                       :: d3fikdr3
  real(dp)                                       :: d3fjkdr3
  real(dp)                                       :: dGijkdr(3)
  real(dp)                                       :: dGjikdr(3)
  real(dp)                                       :: d2Gijkdr2(6)
  real(dp)                                       :: d2Gjikdr2(6)
  real(dp)                                       :: d3Gijkdr3(10)
  real(dp)                                       :: d3Gjikdr3(10)
  real(dp)                                       :: eij
  real(dp)                                       :: expijk
  real(dp)                                       :: expjik
  real(dp)                                       :: f
  real(dp)                                       :: fik
  real(dp)                                       :: fjk
  real(dp)                                       :: Gijk
  real(dp)                                       :: Gjik
  real(dp)                                       :: RmA
  real(dp)                                       :: RmR
  real(dp)                                       :: rbijsumA1
  real(dp)                                       :: rbijsumR1
  real(dp)                                       :: rbjisumA1
  real(dp)                                       :: rbjisumR1
  real(dp),    dimension(:),   allocatable, save :: rBOcutmax
  real(dp)                                       :: rij
  real(dp)                                       :: rik
  real(dp)                                       :: rjk
  real(dp)                                       :: rrij
  real(dp)                                       :: rrik
  real(dp)                                       :: rrjk
  real(dp)                                       :: rtmp
  real(dp)                                       :: scale
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp),    dimension(:,:), allocatable, save :: rneigh
  real(dp),    dimension(:,:), allocatable, save :: xneigh
  real(dp),    dimension(:,:), allocatable, save :: yneigh
  real(dp),    dimension(:,:), allocatable, save :: zneigh
  real(dp)                                       :: Va
  real(dp)                                       :: Vr
  real(dp)                                       :: dVadr
  real(dp)                                       :: dVrdr
  real(dp)                                       :: d2Vadr2
  real(dp)                                       :: d2Vrdr2
  real(dp)                                       :: xdiff
  real(dp)                                       :: ydiff
  real(dp)                                       :: zdiff
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xki
  real(dp)                                       :: yki
  real(dp)                                       :: zki
  real(dp)                                       :: xkj
  real(dp)                                       :: ykj
  real(dp)                                       :: zkj
  real(dp)                                       :: zAi3
  real(dp)                                       :: zAj3
  real(dp)                                       :: zRi3
  real(dp)                                       :: zRj3
!
  t1 = cputime()
  allocate(nboatomRptr(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nboatomRptr')
!
!  Set up a dummy pointer for derivative array calls
!
  nboatom = 0
  do i = 1,numat
    nboatom = nboatom + 1
    nboatomRptr(i) = nboatom
  enddo
!
!  Allocate memory that does not depend on maxneigh             
!
  allocate(lanyau(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','lanyau')
  allocate(latomdone(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','latomdone')
  allocate(lopanyneigh(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','lopanyneigh')
  allocate(nauatom(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nauatom')
  allocate(nfreeatom(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nfreeatom')
  allocate(nfreeatomau(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nfreeatomau')
  allocate(nneigh(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nneigh')
  allocate(rBOcutmax(numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','rBOcutmax')
!
!  Reinitialisation point should maxneigh be increased             
!
100 continue
  lmaxneighok = .true.
  if (allocated(rneigh)) then
    deallocate(d2BtotjR,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2BtotjR')
    deallocate(d2BtotjA,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2BtotjA')
    deallocate(d2BtotiR,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2BtotiR')
    deallocate(d2BtotiA,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2BtotiA')
    deallocate(d2j,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2j')
    deallocate(d2i,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d2i')
    deallocate(d1BtotjR,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1BtotjR')
    deallocate(d1BtotjA,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1BtotjA')
    deallocate(d1BtotiR,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1BtotiR')
    deallocate(d1BtotiA,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1BtotiA')
    deallocate(d1j,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1j')
    deallocate(d1i,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','d1i')
    deallocate(zneigh,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','zneigh')
    deallocate(yneigh,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','yneigh')
    deallocate(xneigh,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','xneigh')
    deallocate(rneigh,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','rneigh')
    deallocate(nbopotptr,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','nbopotptr')
    deallocate(neighno,stat=status)
    if (status/=0) call deallocate_error('bondordersd2','neighno')
  endif
!
!  Initialise Bond Order energy
!
  ebondorder = 0.0_dp
!
!  Set parameter for pairwise storage memory
!
  maxneigh2 = maxneigh + maxneigh*(maxneigh + 1)/2
  maxneigh22 = maxneigh2*(maxneigh2 + 1)/2
!
!  Allocate local memory
!
  allocate(neighno(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','neighno')
  allocate(nbopotptr(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','nbopotptr')
  allocate(rneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','rneigh')
  allocate(xneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','xneigh')
  allocate(yneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','yneigh')
  allocate(zneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordersd2','zneigh')
  if (lgrad1) then
    allocate(d1i(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1i')
    allocate(d1j(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1j')
    allocate(d1BtotiA(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotiA')
    allocate(d1BtotiR(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotiR')
    allocate(d1BtotjA(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotjA')
    allocate(d1BtotjR(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotjR')
  else
    allocate(d1i(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1i')
    allocate(d1j(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1j')
    allocate(d1BtotiA(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotiA')
    allocate(d1BtotiR(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotiR')
    allocate(d1BtotjA(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotjA')
    allocate(d1BtotjR(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d1BtotjR')
  endif
  if (lgrad2) then
    allocate(d2i(maxneigh22),stat=status)   
    if (status/=0) call outofmemory('bondordersd2','d2i')
    allocate(d2j(maxneigh22),stat=status)   
    if (status/=0) call outofmemory('bondordersd2','d2j')
    allocate(d2BtotiA(maxneigh22),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotiA')
    allocate(d2BtotiR(maxneigh22),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotiR')
    allocate(d2BtotjA(maxneigh22),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotjA')
    allocate(d2BtotjR(maxneigh22),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotjR')
  else
    allocate(d2i(1),stat=status)   
    if (status/=0) call outofmemory('bondordersd2','d2i')
    allocate(d2j(1),stat=status)   
    if (status/=0) call outofmemory('bondordersd2','d2j')
    allocate(d2BtotiA(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotiA')
    allocate(d2BtotiR(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotiR')
    allocate(d2BtotjA(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotjA')
    allocate(d2BtotjR(1),stat=status)
    if (status/=0) call outofmemory('bondordersd2','d2BtotjR')
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
    ii = 0
    do i = 1,nasym
      if (lopf(i)) then
        ii = ii + 1
        nfreeatomau(i) = ii
      else
        nfreeatomau(i) = 0
      endif
    enddo
  else
    do i = 1,numat
      nfreeatom(i) = i
    enddo
    do i = 1,nasym
      nfreeatomau(i) = i      
    enddo
  endif
!******************************************************
!  Set list of full cell to asymmetric unit mappings  *
!******************************************************
  do i = 1,numat
    if (nrel2(nrelat(i)).eq.i) then
      nauatom(i) = nrelat(i)
    else  
      nauatom(i) = 0    
    endif  
  enddo
!*************************************
!  Find cut-off radii for all atoms  *
!*************************************
  do i = 1,numat
    nati = nat(i)
    ntypi = nftype(i)
    rBOcutmax(i) = 0.0_dp
!
!  Check twobody potentials
!
    do j = 1,nbopot
      if (nati.eq.nBOspec1(j).and.(ntypi.eq.nBOtyp1(j).or.nBOtyp1(j).eq.0)) then
        rBOcutmax(i) = max(rBOcutmax(i),rBOmax(j))
      endif
      if (nati.eq.nBOspec2(j).and.(ntypi.eq.nBOtyp2(j).or.nBOtyp2(j).eq.0)) then
        rBOcutmax(i) = max(rBOcutmax(i),rBOmax(j))
      endif
    enddo
  enddo
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  call getBOneighbour(maxneigh,rBOcutmax,nBOpotptr,nneigh,neighno,rneigh, &
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
!*******************************
!  Sort neighbours into order  *
!******************************* 
  if (lspatialok) then
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
          itmp = nbopotptr(nptr,i)
          nbopotptr(nptr,i) = nbopotptr(nn,i)
          nbopotptr(nn,i)  = itmp
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
!  Set initial value for lopanyneigh - this
!  variable indicates whether an atom has 
!  any neighbours for which derivatives are
!  required
!
    if (.not.lfreeze) then
      lopanyneigh(i) = .true.
    else
      lopanyneigh(i) = lopf(nrelat(i))
    endif
!
!  Set initial value for lanyau - this variable indicates whether
!  an atom interacts with any atom in the asymmetric unit
!
    lanyau(i) = (nauatom(i).gt.0)
    do n = 1,nneigh(i)
      j = neighno(n,i)
      rij = rneigh(n,i)
!
!  Check whether atom is free to optimise
!
      if (lopf(nrelat(j))) then
        lopanyneigh(i) = .true.
      endif
!
!  Check whether neighbour is in the asymmetric unit
!
      if (nauatom(j).gt.0) then
        lanyau(i) = .true.
      endif
    enddo
  enddo
  if (index(keyword,'debu').ne.0) then
    write(ioout,'(/,''  Number of neighbours for atoms :'',/)')
    write(ioout,'(''  i'',2x,''Nat'',3x,''N'')')
    do i = 1,numat
      write(ioout,'(i4,1x,i3,1x,i6)') i,nat(i),nneigh(i)
    enddo
    write(ioout,'(/,''  Neighbours of atoms :'',/)')
    do i = 1,numat
      write(ioout,'(i4,8(1x,i4))') i,(neighno(nn,i),nn=1,nneigh(i))
    enddo
  endif
!*****************************
!  Loop over pairs of atoms  *
!*****************************
  do i = 1,numat
!
!  Set variables relating to i
!
    nati = nat(i)
    ntypi = nftype(i)
!
!  Set total number of distances for neighbours of i
!
    nneighi2 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2
    nneighi22 = nneighi2*(nneighi2 + 1)/2
!
!  Initialise derivative storage for neighbours of i
!
    if (lgrad1) then
      d1i(1:nneighi2) = 0.0_dp
      if (lgrad2) then
        d2i(1:nneighi22) = 0.0_dp
      endif
    endif
!
!  Loop over neighbours of i (=> j)
!
    ni = 1
    do while (ni.le.nneigh(i).and.neighno(ni,i).le.i)
!
      j = neighno(ni,i)
!
!  Do we need to do this pair of atoms
!
      if ((lopanyneigh(i).or.lopanyneigh(j)).and.(lanyau(i).or.lanyau(j).or.lstr)) then
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
      natj = nat(j)
      ntypj = nftype(j)
!
!  Set up i-j quantities
!
      rij = rneigh(ni,i)
      xji = xneigh(ni,i)
      yji = yneigh(ni,i)
      zji = zneigh(ni,i)
      rrij = 1.0_dp/rij
!
!  Find i in neighbour list for j
!
      nj = 1
      lfound = .false.
      do while (nj.lt.nneigh(j).and..not.lfound)
        if (neighno(nj,j).eq.i) then
          xdiff = xneigh(nj,j) + xji
          ydiff = yneigh(nj,j) + yji
          zdiff = zneigh(nj,j) + zji
          lfound = ((abs(xdiff)+abs(ydiff)+abs(zdiff)).lt.1.0d-6)
        endif
        if (.not.lfound) nj = nj + 1
      enddo
!
!  Set total number of distances for neighbours of j
!
      nneighj2 = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2
      nneighj22 = nneighj2*(nneighj2 + 1)/2
!
!  Initialise derivative storage for neighbours of i
!
      if (lgrad1) then
        d1j(1:nneighj2) = 0.0_dp
        if (lgrad2) then
          d2j(1:nneighj22) = 0.0_dp
        endif
      endif
!
!  Find repulsive bond order potential from j to i
!
      lfound = .false.
      nboRij = 0
      do while (.not.lfound.and.nboRij.lt.nboR) 
        nboRij = nboRij + 1
        if (nBOspecR1(nboRij).eq.nati.and.nBOspecR2(nboRij).eq.natj) then
          if ((nBOtypR1(nboRij).eq.ntypi.or.nBOtypR1(nboRij).eq.0).and. &
              (nBOtypR2(nboRij).eq.ntypj.or.nBOtypR2(nboRij).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (.not.lfound) nboRij = 0
!
!  Find repulsive bond order potential from i to j
!
      lfound = .false.
      nboRji = 0
      do while (.not.lfound.and.nboRji.lt.nboR)
        nboRji = nboRji + 1
        if (nBOspecR1(nboRji).eq.natj.and.nBOspecR2(nboRji).eq.nati) then
          if ((nBOtypR1(nboRji).eq.ntypj.or.nBOtypR1(nboRji).eq.0).and. &
              (nBOtypR2(nboRji).eq.ntypi.or.nBOtypR2(nboRji).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (.not.lfound) nboRji = 0
!
!  Find attractive bond order potential from j to i
!
      lfound = .false.
      nboAij = 0
      do while (.not.lfound.and.nboAij.lt.nboA)
        nboAij = nboAij + 1
        if (nBOspecA1(nboAij).eq.nati.and.nBOspecA2(nboAij).eq.natj) then
          if ((nBOtypA1(nboAij).eq.ntypi.or.nBOtypA1(nboAij).eq.0).and. &
              (nBOtypA2(nboAij).eq.ntypj.or.nBOtypA2(nboAij).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (.not.lfound) nboAij = 0
!
!  Find attractive bond order potential from i to j
!  
      lfound = .false.
      nboAji = 0
      do while (.not.lfound.and.nboAji.lt.nboA)
        nboAji = nboAji + 1
        if (nBOspecA1(nboAji).eq.natj.and.nBOspecA2(nboAji).eq.nati) then
          if ((nBOtypA1(nboAji).eq.ntypj.or.nBOtypA1(nboAji).eq.0).and. &
              (nBOtypA2(nboAji).eq.ntypi.or.nBOtypA2(nboAji).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (.not.lfound) nboAji = 0
!
!  Find two-body bond order potential between i and j
!
      lfound = .false.
      nboij = 0
      do while (.not.lfound.and.nboij.lt.nbopot) 
        nboij = nboij + 1
        if (nBOspec1(nboij).eq.nati.and.nBOspec2(nboij).eq.natj) then
          if ((nBOtyp1(nboij).eq.ntypi.or.nBOtyp1(nboij).eq.0).and. &
              (nBOtyp2(nboij).eq.ntypj.or.nBOtyp2(nboij).eq.0)) then
            lfound = .true.
          endif
        elseif (nBOspec1(nboij).eq.natj.and.nBOspec2(nboij).eq.nati) then
          if ((nBOtyp1(nboij).eq.ntypj.or.nBOtyp1(nboij).eq.0).and. &
              (nBOtyp2(nboij).eq.ntypi.or.nBOtyp2(nboij).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (.not.lfound) nboij = 0
      if (nboij.gt.0) then
!****************************************
!  Valid two-body bond order potential  *
!****************************************
!
!  Calculate fij
!
        call ctaper(rij,rBOmin(nboij),rBOmax(nboij),f,dfdr,d2fdr2,d3fdr3,lgrad1,lgrad2,.false.)
!
!  Calculate Bij and Bji - loop over all other neighbours
!
        bijsumA = 0.0_dp
        bijsumR = 0.0_dp
        bjisumA = 0.0_dp
        bjisumR = 0.0_dp
        if (lgrad1) then
          d1BtotiA(1:nneighi2) = 0.0_dp
          d1BtotiR(1:nneighi2) = 0.0_dp
          d1BtotjA(1:nneighj2) = 0.0_dp
          d1BtotjR(1:nneighj2) = 0.0_dp
          if (lgrad2) then
            d2BtotiA(1:nneighi22) = 0.0_dp
            d2BtotiR(1:nneighi22) = 0.0_dp
            d2BtotjA(1:nneighj22) = 0.0_dp
            d2BtotjR(1:nneighj22) = 0.0_dp
          endif
        endif
!
!  Loop over neighbours of i .ne. j 
!
        do k = 1,nneigh(i)
          npki = nbopotptr(k,i)
          if (k.ne.ni) then
            rik = rneigh(k,i)
            xki = xneigh(k,i)
            yki = yneigh(k,i)
            zki = zneigh(k,i)
!
!  Repulsive component
!
            if (nboRij.gt.0) then
              if (rik.lt.rBOmax(npki)) then
!
!  Calculate fik
!
                call ctaper(rik,rBOmin(npki),rBOmax(npki),fik,dfikdr,d2fikdr2,d3fikdr3,lgrad1,lgrad2,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeR(nboRij).ne.1) then
                  call GthetaBO(xji,yji,zji,xki,yki,zki,BOccoeffR(nboRij),BOdcoeffR(nboRij), &
                    BOhcoeffR(nboRij),Gijk,dGijkdr,d2Gijkdr2,d3Gijkdr3,lgrad1,lgrad2,.false.)
                else
                  Gijk = 1.0_dp
                  dGijkdr = 0.0_dp
                  d2Gijkdr2 = 0.0_dp
                endif
!
!  Calculate exponential factor
!
                mR = nint(BOmcoeffR(nboRij))
                expijk = exp((BOlcoeffR(nboRij)*(rij - rik))**mR)
!
!  Combine terms
!
                bijsumR = bijsumR + Gijk*fik*expijk
                if (lgrad1) then
!
!  Derivatives
!
!  Find index for j-k 
!
                  if (ni.ge.k) then
                    njk = nneigh(i) + ni*(ni-1)/2 + k
                  else
                    njk = nneigh(i) + k*(k-1)/2 + ni
                  endif
!
                  rrik = 1.0_dp/rik
                  dfikdr = rrik*dfikdr
                  RmR = dble(mR)
                  if (mR.ge.1) then
                    dexpijkdr = RmR*(BOlcoeffR(nboRij)**mR)*((rij-rik)**(mR-1))*expijk
                  else
                    dexpijkdr = 0.0_dp
                  endif
!
                  d1BtotiR(k)   = d1BtotiR(k)   + Gijk*dfikdr*expijk
!
                  d1BtotiR(ni)  = d1BtotiR(ni)  + dGijkdr(1)*fik*expijk
                  d1BtotiR(k)   = d1BtotiR(k)   + dGijkdr(2)*fik*expijk
                  d1BtotiR(njk) = d1BtotiR(njk) + dGijkdr(3)*fik*expijk
!
                  d1BtotiR(ni) = d1BtotiR(ni) + Gijk*fik*dexpijkdr*rrij
                  d1BtotiR(k)  = d1BtotiR(k)  - Gijk*fik*dexpijkdr*rrik
!
                  if (lgrad2) then
                    d2fikdr2 = rrik*rrik*(d2fikdr2 - dfikdr)
                    if (mR.ge.2) then
                      d2expijkdr2 = RmR*(BOlcoeffR(nboRij)**mR)*((rij-rik)**(mR-2))*expijk* &
                        ((RmR-1.0_dp) + RmR*(BOlcoeffR(nboRij)*(rij-rik))**mR)
                    else
                      d2expijkdr2 = 0.0_dp
                    endif
!
                    nn = ni*(ni + 1)/2
                    d2BtotiR(nn) = d2BtotiR(nn) + Gijk*fik*d2expijkdr2*rrij*rrij
                    d2BtotiR(nn) = d2BtotiR(nn) - Gijk*fik*dexpijkdr*rrij*rrij*rrij
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(1)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) + 2.0_dp*dGijkdr(1)*fik*dexpijkdr*rrij
!
                    if (ni.ge.k) then
                      nn = ni*(ni - 1)/2 + k
                    else
                      nn = k*(k - 1)/2 + ni
                    endif
                    d2BtotiR(nn) = d2BtotiR(nn) + Gijk*dfikdr*dexpijkdr*rrij
                    d2BtotiR(nn) = d2BtotiR(nn) - Gijk*fik*d2expijkdr2*rrij*rrik
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(2)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) - dGijkdr(1)*fik*dexpijkdr*rrik
                    d2BtotiR(nn) = d2BtotiR(nn) + dGijkdr(1)*dfikdr*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) + dGijkdr(2)*fik*dexpijkdr*rrij
!
                    if (ni.ge.njk) then
                      nn = ni*(ni - 1)/2 + njk
                    else
                      nn = njk*(njk - 1)/2 + ni
                    endif
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(3)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) + dGijkdr(3)*fik*dexpijkdr*rrij
!
                    nn = k*(k + 1)/2
                    d2BtotiR(nn) = d2BtotiR(nn) + Gijk*d2fikdr2*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) - 2.0_dp*Gijk*dfikdr*dexpijkdr*rrik
                    d2BtotiR(nn) = d2BtotiR(nn) + Gijk*fik*d2expijkdr2*rrik*rrik
                    d2BtotiR(nn) = d2BtotiR(nn) + Gijk*fik*dexpijkdr*rrik*rrik*rrik
                    d2BtotiR(nn) = d2BtotiR(nn) + 2.0_dp*dGijkdr(2)*dfikdr*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(4)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) - 2.0_dp*dGijkdr(2)*fik*dexpijkdr*rrik
!
                    if (k.ge.njk) then
                      nn = k*(k - 1)/2 + njk
                    else
                      nn = njk*(njk - 1)/2 + k
                    endif
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(5)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) + dGijkdr(3)*dfikdr*expijk
!
                    nn = njk*(njk + 1)/2
                    d2BtotiR(nn) = d2BtotiR(nn) + d2Gijkdr2(6)*fik*expijk
                    d2BtotiR(nn) = d2BtotiR(nn) - 2.0_dp*dGijkdr(3)*fik*dexpijkdr*rrik
                  endif
                endif
              endif
            endif
!
!  Attractive component
!
            if (nboAij.gt.0) then
              if (rik.lt.rBOmax(npki)) then
!
!  Calculate fik
!
                call ctaper(rik,rBOmin(npki),rBOmax(npki),fik,dfikdr,d2fikdr2,d3fikdr3,lgrad1,lgrad2,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeA(nboAij).ne.1) then
                  call GthetaBO(xji,yji,zji,xki,yki,zki,BOccoeffA(nboAij),BOdcoeffA(nboAij), &
                    BOhcoeffA(nboAij),Gijk,dGijkdr,d2Gijkdr2,d3Gijkdr3,lgrad1,lgrad2,.false.)
                else
                  Gijk = 1.0_dp
                  dGijkdr = 0.0_dp
                  d2Gijkdr2 = 0.0_dp
                endif
!
!  Calculate exponential factor
!
                mA = nint(BOmcoeffA(nboAij))
                expijk = exp((BOlcoeffA(nboAij)*(rij - rik))**mA)
!
!  Combine terms
!
                bijsumA = bijsumA + Gijk*fik*expijk
                if (lgrad1) then
!
!  Derivatives
!
!  Find index for j-k 
!
                  if (ni.ge.k) then
                    njk = nneigh(i) + ni*(ni-1)/2 + k
                  else
                    njk = nneigh(i) + k*(k-1)/2 + ni
                  endif
!
                  rrik = 1.0_dp/rik
                  dfikdr = rrik*dfikdr
                  RmA = dble(mA)
                  if (mA.ge.1) then
                    dexpijkdr = RmA*(BOlcoeffA(nboAij)**mA)*((rij-rik)**(mA-1))*expijk
                  else
                    dexpijkdr = 0.0_dp
                  endif
!
                  d1BtotiA(k)   = d1BtotiA(k)   + Gijk*dfikdr*expijk
!
                  d1BtotiA(ni)  = d1BtotiA(ni)  + dGijkdr(1)*fik*expijk
                  d1BtotiA(k)   = d1BtotiA(k)   + dGijkdr(2)*fik*expijk
                  d1BtotiA(njk) = d1BtotiA(njk) + dGijkdr(3)*fik*expijk
!
                  d1BtotiA(ni) = d1BtotiA(ni) + Gijk*fik*dexpijkdr*rrij
                  d1BtotiA(k)  = d1BtotiA(k)  - Gijk*fik*dexpijkdr*rrik
!
                  if (lgrad2) then
                    d2fikdr2 = rrik*rrik*(d2fikdr2 - dfikdr)
                    if (mA.ge.2) then
                      d2expijkdr2 = RmA*(BOlcoeffA(nboAij)**mA)*((rij-rik)**(mA-2))*expijk* &
                        ((RmA-1.0_dp) + RmA*(BOlcoeffA(nboAij)*(rij-rik))**mA)
                    else
                      d2expijkdr2 = 0.0_dp
                    endif
!
                    nn = ni*(ni + 1)/2
                    d2BtotiA(nn) = d2BtotiA(nn) + Gijk*fik*d2expijkdr2*rrij*rrij
                    d2BtotiA(nn) = d2BtotiA(nn) - Gijk*fik*dexpijkdr*rrij*rrij*rrij
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(1)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) + 2.0_dp*dGijkdr(1)*fik*dexpijkdr*rrij
!
                    if (ni.ge.k) then
                      nn = ni*(ni - 1)/2 + k
                    else
                      nn = k*(k - 1)/2 + ni
                    endif
                    d2BtotiA(nn) = d2BtotiA(nn) + Gijk*dfikdr*dexpijkdr*rrij
                    d2BtotiA(nn) = d2BtotiA(nn) - Gijk*fik*d2expijkdr2*rrij*rrik
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(2)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) - dGijkdr(1)*fik*dexpijkdr*rrik
                    d2BtotiA(nn) = d2BtotiA(nn) + dGijkdr(1)*dfikdr*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) + dGijkdr(2)*fik*dexpijkdr*rrij
!
                    if (ni.ge.njk) then
                      nn = ni*(ni - 1)/2 + njk
                    else
                      nn = njk*(njk - 1)/2 + ni
                    endif
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(3)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) + dGijkdr(3)*fik*dexpijkdr*rrij
!
                    nn = k*(k + 1)/2
                    d2BtotiA(nn) = d2BtotiA(nn) + Gijk*d2fikdr2*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) - 2.0_dp*Gijk*dfikdr*dexpijkdr*rrik
                    d2BtotiA(nn) = d2BtotiA(nn) + Gijk*fik*d2expijkdr2*rrik*rrik
                    d2BtotiA(nn) = d2BtotiA(nn) + Gijk*fik*dexpijkdr*rrik*rrik*rrik
                    d2BtotiA(nn) = d2BtotiA(nn) + 2.0_dp*dGijkdr(2)*dfikdr*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(4)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) - 2.0_dp*dGijkdr(2)*fik*dexpijkdr*rrik
!
                    if (k.ge.njk) then
                      nn = k*(k - 1)/2 + njk
                    else
                      nn = njk*(njk - 1)/2 + k
                    endif
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(5)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) + dGijkdr(3)*dfikdr*expijk
!
                    nn = njk*(njk + 1)/2
                    d2BtotiA(nn) = d2BtotiA(nn) + d2Gijkdr2(6)*fik*expijk
                    d2BtotiA(nn) = d2BtotiA(nn) - 2.0_dp*dGijkdr(3)*fik*dexpijkdr*rrik
                  endif
                endif
              endif
            endif
          endif
        enddo
!
!  Loop over neighbours of j .ne. i 
!
        do k = 1,nneigh(j)
          npkj = nbopotptr(k,j)
          if (k.ne.nj) then
            rjk = rneigh(k,j)
            xkj = xneigh(k,j)
            ykj = yneigh(k,j)
            zkj = zneigh(k,j)
!
!  Repsulve component
!
            if (nboRji.gt.0) then
              if (rjk.lt.rBOmax(npkj)) then
!
!  Calculate fik
!
                call ctaper(rjk,rBOmin(npkj),rBOmax(npkj),fjk,dfjkdr,d2fjkdr2,d3fjkdr3,lgrad1,lgrad2,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeR(nboRji).ne.1) then
                  call GthetaBO(-xji,-yji,-zji,xkj,ykj,zkj,BOccoeffR(nboRji),BOdcoeffR(nboRji), &
                    BOhcoeffR(nboRji),Gjik,dGjikdr,d2Gjikdr2,d3Gjikdr3,lgrad1,lgrad2,.false.)
                else
                  Gjik = 1.0_dp
                  dGjikdr = 0.0_dp
                  d2Gjikdr2 = 0.0_dp
                endif
!
!  Calculate exponential factor
!
                mR = nint(BOmcoeffR(nboRji))
                expjik = exp((BOlcoeffR(nboRji)*(rij - rjk))**mR)
!
!  Combine terms
!
                bjisumR = bjisumR + Gjik*fjk*expjik
                if (lgrad1) then
!
!  Derivatives
!
!  Find index for i-k
!
                  if (nj.ge.k) then
                    nik = nneigh(j) + nj*(nj-1)/2 + k
                  else
                    nik = nneigh(j) + k*(k-1)/2 + nj
                  endif
!
                  rrjk = 1.0_dp/rjk
                  dfjkdr = rrjk*dfjkdr
                  RmR = dble(mR)
                  if (mR.ge.1) then
                    dexpjikdr = RmR*(BOlcoeffR(nboRji)**mR)*((rij-rjk)**(mR-1))*expjik
                  else
                    dexpjikdr = 0.0_dp
                  endif
!
                  d1BtotjR(k)   = d1BtotjR(k)   + Gjik*dfjkdr*expjik
!
                  d1BtotjR(nj)  = d1BtotjR(nj)  + dGjikdr(1)*fjk*expjik
                  d1BtotjR(k)   = d1BtotjR(k)   + dGjikdr(2)*fjk*expjik
                  d1BtotjR(nik) = d1BtotjR(nik) + dGjikdr(3)*fjk*expjik
!
                  d1BtotjR(nj) = d1BtotjR(nj) + Gjik*fjk*dexpjikdr*rrij
                  d1BtotjR(k)  = d1BtotjR(k)  - Gjik*fjk*dexpjikdr*rrjk
                  if (lgrad2) then
                    d2fjkdr2 = rrjk*rrjk*(d2fjkdr2 - dfjkdr)
                    if (mR.ge.2) then
                      d2expjikdr2 = RmR*(BOlcoeffR(nboRji)**mR)*((rij-rjk)**(mR-2))*expjik* &
                        ((RmR-1.0_dp) + RmR*(BOlcoeffR(nboRji)*(rij-rjk))**mR)
                    else
                      d2expjikdr2 = 0.0_dp
                    endif
!
                    nn = nj*(nj + 1)/2
                    d2BtotjR(nn) = d2BtotjR(nn) + Gjik*fjk*d2expjikdr2*rrij*rrij
                    d2BtotjR(nn) = d2BtotjR(nn) - Gjik*fjk*dexpjikdr*rrij*rrij*rrij
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(1)*fjk*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) + 2.0_dp*dGjikdr(1)*fjk*dexpjikdr*rrij
!
                    if (nj.ge.k) then
                      nn = nj*(nj - 1)/2 + k
                    else
                      nn = k*(k - 1)/2 + nj
                    endif
                    d2BtotjR(nn) = d2BtotjR(nn) - Gjik*fjk*d2expjikdr2*rrij*rrjk
                    d2BtotjR(nn) = d2BtotjR(nn) + Gjik*dfjkdr*dexpjikdr*rrij
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(2)*fjk*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) + dGjikdr(1)*dfjkdr*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) - dGjikdr(1)*fjk*dexpjikdr*rrjk
                    d2BtotjR(nn) = d2BtotjR(nn) + dGjikdr(2)*fjk*dexpjikdr*rrij
!
                    if (nj.ge.nik) then
                      nn = nj*(nj - 1)/2 + nik
                    else
                      nn = nik*(nik - 1)/2 + nj
                    endif
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(3)*fjk*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) + dGjikdr(3)*fjk*dexpjikdr*rrij
!
                    nn = k*(k + 1)/2
                    d2BtotjR(nn) = d2BtotjR(nn) + Gjik*d2fjkdr2*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) - 2.0_dp*Gjik*dfjkdr*dexpjikdr*rrjk
                    d2BtotjR(nn) = d2BtotjR(nn) + Gjik*fjk*d2expjikdr2*rrjk*rrjk
                    d2BtotjR(nn) = d2BtotjR(nn) + Gjik*fjk*dexpjikdr*rrjk*rrjk*rrjk
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(4)*fjk*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) + 2.0_dp*dGjikdr(2)*dfjkdr*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) - 2.0_dp*dGjikdr(2)*fjk*dexpjikdr*rrjk
!
                    if (k.ge.nik) then
                      nn = k*(k - 1)/2 + nik
                    else
                      nn = nik*(nik - 1)/2 + k
                    endif
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(5)*fjk*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) + dGjikdr(3)*dfjkdr*expjik
                    d2BtotjR(nn) = d2BtotjR(nn) - dGjikdr(3)*fjk*dexpjikdr*rrjk
!
                    nn = nik*(nik + 1)/2
                    d2BtotjR(nn) = d2BtotjR(nn) + d2Gjikdr2(6)*fjk*expjik
                  endif
                endif
              endif
            endif
!
!  Attractive component
!
            if (nboAji.gt.0) then
              if (rjk.lt.rBOmax(npkj)) then
!
!  Calculate fik
!
                call ctaper(rjk,rBOmin(npkj),rBOmax(npkj),fjk,dfjkdr,d2fjkdr2,d3fjkdr3,lgrad1,lgrad2,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeA(nboAji).ne.1) then
                  call GthetaBO(-xji,-yji,-zji,xkj,ykj,zkj,BOccoeffA(nboAji),BOdcoeffA(nboAji), &
                    BOhcoeffA(nboAji),Gjik,dGjikdr,d2Gjikdr2,d3Gjikdr3,lgrad1,lgrad2,.false.)
                else
                  Gjik = 1.0_dp
                  dGjikdr = 0.0_dp
                  d2Gjikdr2 = 0.0_dp
                endif
!
!  Calculate exponential factor
!
                mA = nint(BOmcoeffA(nboAji))
                expjik = exp((BOlcoeffA(nboAji)*(rij - rjk))**mA)
!
!  Combine terms
!
                bjisumA = bjisumA + Gjik*fjk*expjik
                if (lgrad1) then
!
!  Derivatives
!
!  Find index for i-k
!
                  if (nj.ge.k) then
                    nik = nneigh(j) + nj*(nj-1)/2 + k
                  else
                    nik = nneigh(j) + k*(k-1)/2 + nj
                  endif
!
                  rrjk = 1.0_dp/rjk
                  dfjkdr = rrjk*dfjkdr
                  RmA = dble(mA)
                  if (mA.ge.1) then
                    dexpjikdr = mA*(BOlcoeffA(nboAji)**mA)*((rij-rjk)**(mA-1))*expjik
                  else
                    dexpjikdr = 0.0_dp
                  endif
!
                  d1BtotjA(k)   = d1BtotjA(k)   + Gjik*dfjkdr*expjik
!
                  d1BtotjA(nj)  = d1BtotjA(nj)  + dGjikdr(1)*fjk*expjik
                  d1BtotjA(k)   = d1BtotjA(k)   + dGjikdr(2)*fjk*expjik
                  d1BtotjA(nik) = d1BtotjA(nik) + dGjikdr(3)*fjk*expjik
!
                  d1BtotjA(nj) = d1BtotjA(nj) + Gjik*fjk*dexpjikdr*rrij
                  d1BtotjA(k)  = d1BtotjA(k)  - Gjik*fjk*dexpjikdr*rrjk
                  if (lgrad2) then
                    d2fjkdr2 = rrjk*rrjk*(d2fjkdr2 - dfjkdr)
                    if (mA.ge.2) then
                      d2expjikdr2 = RmA*(BOlcoeffA(nboAji)**mA)*((rij-rjk)**(mA-2))*expjik* &
                        ((RmA-1.0_dp) + RmA*(BOlcoeffA(nboAji)*(rij-rjk))**mA)
                    else
                      d2expjikdr2 = 0.0_dp
                    endif
!
                    nn = nj*(nj + 1)/2
                    d2BtotjA(nn) = d2BtotjA(nn) + Gjik*fjk*d2expjikdr2*rrij*rrij
                    d2BtotjA(nn) = d2BtotjA(nn) - Gjik*fjk*dexpjikdr*rrij*rrij*rrij
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(1)*fjk*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) + 2.0_dp*dGjikdr(1)*fjk*dexpjikdr*rrij
!
                    if (nj.ge.k) then
                      nn = nj*(nj - 1)/2 + k
                    else
                      nn = k*(k - 1)/2 + nj
                    endif
                    d2BtotjA(nn) = d2BtotjA(nn) - Gjik*fjk*d2expjikdr2*rrij*rrjk
                    d2BtotjA(nn) = d2BtotjA(nn) + Gjik*dfjkdr*dexpjikdr*rrij
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(2)*fjk*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) + dGjikdr(1)*dfjkdr*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) - dGjikdr(1)*fjk*dexpjikdr*rrjk
                    d2BtotjA(nn) = d2BtotjA(nn) + dGjikdr(2)*fjk*dexpjikdr*rrij
!
                    if (nj.ge.nik) then
                      nn = nj*(nj - 1)/2 + nik
                    else
                      nn = nik*(nik - 1)/2 + nj
                    endif
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(3)*fjk*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) + dGjikdr(3)*fjk*dexpjikdr*rrij
!
                    nn = k*(k + 1)/2
                    d2BtotjA(nn) = d2BtotjA(nn) + Gjik*d2fjkdr2*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) - 2.0_dp*Gjik*dfjkdr*dexpjikdr*rrjk
                    d2BtotjA(nn) = d2BtotjA(nn) + Gjik*fjk*d2expjikdr2*rrjk*rrjk
                    d2BtotjA(nn) = d2BtotjA(nn) + Gjik*fjk*dexpjikdr*rrjk*rrjk*rrjk
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(4)*fjk*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) + 2.0_dp*dGjikdr(2)*dfjkdr*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) - 2.0_dp*dGjikdr(2)*fjk*dexpjikdr*rrjk
!
                    if (k.ge.nik) then
                      nn = k*(k - 1)/2 + nik
                    else
                      nn = nik*(nik - 1)/2 + k
                    endif
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(5)*fjk*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) + dGjikdr(3)*dfjkdr*expjik
                    d2BtotjA(nn) = d2BtotjA(nn) - dGjikdr(3)*fjk*dexpjikdr*rrjk
!
                    nn = nik*(nik + 1)/2
                    d2BtotjA(nn) = d2BtotjA(nn) + d2Gjikdr2(6)*fjk*expjik
                  endif
                endif
              endif
            endif
          endif
        enddo
!
!  Raise terms to the power of n, add 1, and then raise to -2*n
!
        zAi3 = BOecoeffA(nboAij)**BOncoeffA(nboAij)
        zRi3 = BOecoeffR(nboRij)**BOncoeffR(nboRij)
        zAj3 = BOecoeffA(nboAji)**BOncoeffA(nboAji)
        zRj3 = BOecoeffR(nboRji)**BOncoeffR(nboRji)
!
        if (abs(bijsumA).gt.1.0d-12) then
          bijsumAn1 = bijsumA**(BOncoeffA(nboAij) - 1.0_dp)
        else
          bijsumAn1 = 0.0_dp
        endif
        if (abs(bijsumR).gt.1.0d-12) then
          bijsumRn1 = bijsumR**(BOncoeffR(nboRij) - 1.0_dp)
        else
          bijsumRn1 = 0.0_dp
        endif
        if (abs(bjisumA).gt.1.0d-12) then
          bjisumAn1 = bjisumA**(BOncoeffA(nboAji) - 1.0_dp)
        else
          bjisumAn1 = 0.0_dp
        endif
        if (abs(bjisumR).gt.1.0d-12) then
          bjisumRn1 = bjisumR**(BOncoeffR(nboRji) - 1.0_dp)
        else
          bjisumRn1 = 0.0_dp
        endif
!
        bijsumA1 = 1.0_dp + zAi3*bijsumA*bijsumAn1
        bijsumR1 = 1.0_dp + zRi3*bijsumR*bijsumRn1
        bjisumA1 = 1.0_dp + zAj3*bjisumA*bjisumAn1
        bjisumR1 = 1.0_dp + zRj3*bjisumR*bjisumRn1
!
        rbijsumA1 = 1.0_dp/bijsumA1
        rbijsumR1 = 1.0_dp/bijsumR1
        rbjisumA1 = 1.0_dp/bjisumA1
        rbjisumR1 = 1.0_dp/bjisumR1
!
        bijA = bijsumA1**(-0.5_dp/BOncoeffA(nboAij))
        bijR = bijsumR1**(-0.5_dp/BOncoeffR(nboRij))
        bjiA = bjisumA1**(-0.5_dp/BOncoeffA(nboAji))
        bjiR = bjisumR1**(-0.5_dp/BOncoeffR(nboRji))
!
!  Scale derivatives by bijsum/bjisum factors
!
        if (lgrad1) then
          if (lgrad2) then
!
!  Second derivatives
!
            if (bijsumA.gt.0.0_dp) then
              rtmp = 0.25_dp*zAi3*bijsumAn1*bijA*rbijsumA1
              do nn = 1,nneighi22
                d2BtotiA(nn) = - rtmp*d2BtotiA(nn)
              enddo
              rtmp = 0.25_dp*zAi3*bijA*rbijsumA1*(zAi3*(1.0_dp + 0.5_dp/BOncoeffA(nboAij))*(bijsumAn1**2.0_dp)* &
                BOncoeffA(nboAij)*rbijsumA1 - ((BOncoeffA(nboAij)-1)*bijsumA**(BOncoeffA(nboAij)-2)))
              nn = 0 
              do nn1 = 1,nneighi2
                do nn2 = 1,nn1
                  nn = nn + 1
                  d2BtotiA(nn) = d2BtotiA(nn) + rtmp*d1BtotiA(nn2)*d1BtotiA(nn1)
                enddo
              enddo
            endif
            if (bjisumA.gt.0.0_dp) then
              rtmp = 0.25_dp*zAj3*bjisumAn1*bjiA*rbjisumA1
              do nn = 1,nneighj22
                d2BtotjA(nn) = - rtmp*d2BtotjA(nn)
              enddo
              rtmp = 0.25_dp*zAj3*bjiA*rbjisumA1*(zAj3*(1.0_dp + 0.5_dp/BOncoeffA(nboAji))*(bjisumAn1**2.0_dp)* &
                BOncoeffA(nboAji)*rbjisumA1 - ((BOncoeffA(nboAji)-1)*bjisumA**(BOncoeffA(nboAji)-2)))
              nn = 0 
              do nn1 = 1,nneighj2
                do nn2 = 1,nn1
                  nn = nn + 1
                  d2BtotjA(nn) = d2BtotjA(nn) + rtmp*d1BtotjA(nn2)*d1BtotjA(nn1)
                enddo
              enddo
            endif
            if (bijsumR.gt.0.0_dp) then
              rtmp = 0.25_dp*zRi3*bijsumRn1*bijR*rbijsumR1
              do nn = 1,nneighi22
                d2BtotiR(nn) = - rtmp*d2BtotiR(nn)
              enddo
              rtmp = 0.25_dp*zRi3*bijR*rbijsumR1*(zRi3*(1.0_dp + 0.5_dp/BOncoeffR(nboRij))*(bijsumRn1**2.0_dp)* &
                BOncoeffR(nboRij)*rbijsumR1 - ((BOncoeffR(nboRij)-1)*bijsumR**(BOncoeffR(nboRij)-2)))
              nn = 0 
              do nn1 = 1,nneighi2
                do nn2 = 1,nn1
                  nn = nn + 1
                  d2BtotiR(nn) = d2BtotiR(nn) + rtmp*d1BtotiR(nn2)*d1BtotiR(nn1)
                enddo
              enddo
            endif
            if (bjisumR.gt.0.0_dp) then
              rtmp = 0.25_dp*zRj3*bjisumRn1*bjiR*rbjisumR1
              do nn = 1,nneighj22
                d2BtotjR(nn) = - rtmp*d2BtotjR(nn)
              enddo
              rtmp = 0.25_dp*zRj3*bjiR*rbjisumR1*(zRj3*(1.0_dp + 0.5_dp/BOncoeffR(nboRji))*(bjisumRn1**2.0_dp)* &
                BOncoeffR(nboRji)*rbjisumR1 - ((BOncoeffR(nboRji)-1)*bjisumR**(BOncoeffR(nboRji)-2)))
              nn = 0 
              do nn1 = 1,nneighj2
                do nn2 = 1,nn1
                  nn = nn + 1
                  d2BtotjR(nn) = d2BtotjR(nn) + rtmp*d1BtotjR(nn2)*d1BtotjR(nn1)
                enddo
              enddo
            endif
          endif
!
!  First derivatives
!
          if (bijsumA.gt.0.0_dp) then
            rtmp = 0.25_dp*zAi3*bijsumAn1*bijA*rbijsumA1
            do nn = 1,nneighi2
              d1BtotiA(nn) = - rtmp*d1BtotiA(nn)
            enddo
          endif
          if (bijsumR.gt.0.0_dp) then
            rtmp = 0.25_dp*zRi3*bijsumRn1*bijR*rbijsumR1
            do nn = 1,nneighi2
              d1BtotiR(nn) = - rtmp*d1BtotiR(nn)
            enddo
          endif
          if (bjisumA.gt.0.0_dp) then
            rtmp = 0.25_dp*zAj3*bjisumAn1*bjiA*rbjisumA1
            do nn = 1,nneighj2
              d1BtotjA(nn) = - rtmp*d1BtotjA(nn)
            enddo
          endif
          if (bjisumR.gt.0.0_dp) then
            rtmp = 0.25_dp*zRj3*bjisumRn1*bjiR*rbjisumR1
            do nn = 1,nneighj2
              d1BtotjR(nn) = - rtmp*d1BtotjR(nn)
            enddo
          endif
        endif
!
!  Calculate two-body component of potential
!
        Vr = BOacoeff(nboij)*exp(-BOzacoeff(nboij)*rij)
        Va = BObcoeff(nboij)*exp(-BOzbcoeff(nboij)*rij)
        if (lgrad1) then
          dVrdr = - BOzacoeff(nboij)*Vr
          dVadr = - BOzbcoeff(nboij)*Va
!
          dVrdr = rrij*dVrdr
          dVadr = rrij*dVadr
          if (lgrad2) then
            d2Vrdr2 = BOzacoeff(nboij)*BOzacoeff(nboij)*Vr
            d2Vadr2 = BOzbcoeff(nboij)*BOzbcoeff(nboij)*Va
            d2Vrdr2 = rrij*rrij*(d2Vrdr2 - dVrdr)
            d2Vadr2 = rrij*rrij*(d2Vadr2 - dVadr)
          endif
        endif
!
!  Calculate total i-j potential
!
        BtotA = 0.5_dp*(bijA + bjiA) 
        BtotR = 0.5_dp*(bijR + bjiR) 
        eij = scale*f*(BtotR*Vr - BtotA*Va)
!
        ebondorder = ebondorder + eij
!
!  Derivatives of Bond Order potential energy
!
        if (lgrad1) then
          dfdr = rrij*dfdr
          d1i(ni) = d1i(ni) + scale*dfdr*(BtotR*Vr - BtotA*Va)
          d1i(ni) = d1i(ni) + scale*f*(BtotR*dVrdr - BtotA*dVadr)
          do nn = 1,nneighi2
            d1i(nn) = d1i(nn) + scale*f*(Vr*d1BtotiR(nn) - Va*d1BtotiA(nn))
          enddo
          do nn = 1,nneighj2
            d1j(nn) = d1j(nn) + scale*f*(Vr*d1BtotjR(nn) - Va*d1BtotjA(nn))
          enddo
          if (lgrad2) then
            ind = ni*(ni + 1)/2
            d2fdr2 = rrij*rrij*(d2fdr2 - dfdr)
            d2i(ind) = d2i(ind) + scale*d2fdr2*(BtotR*Vr - BtotA*Va)
            d2i(ind) = d2i(ind) + scale*2.0_dp*dfdr*(BtotR*dVrdr - BtotA*dVadr)
            d2i(ind) = d2i(ind) + scale*f*(BtotR*d2Vrdr2 - BtotA*d2Vadr2)
            do nn = 1,nneighi2
              if (ni.ge.nn) then
                ind = ni*(ni - 1)/2 + nn
              else
                ind = nn*(nn - 1)/2 + ni
              endif
              d2i(ind) = d2i(ind) + scale*((dfdr*Vr + f*dVrdr)*d1BtotiR(nn) - (dfdr*Va + f*dVadr)*d1BtotiA(nn))
              if (ni.eq.nn) then
                d2i(ind) = d2i(ind) + scale*((dfdr*Vr + f*dVrdr)*d1BtotiR(nn) - (dfdr*Va + f*dVadr)*d1BtotiA(nn))
              endif
            enddo
            do nn = 1,nneighj2
              if (nj.ge.nn) then
                ind = nj*(nj - 1)/2 + nn
              else
                ind = nn*(nn - 1)/2 + nj
              endif
              d2j(ind) = d2j(ind) + scale*((dfdr*Vr + f*dVrdr)*d1BtotjR(nn) - (dfdr*Va + f*dVadr)*d1BtotjA(nn))
              if (nj.eq.nn) then
                d2j(ind) = d2j(ind) + scale*((dfdr*Vr + f*dVrdr)*d1BtotjR(nn) - (dfdr*Va + f*dVadr)*d1BtotjA(nn))
              endif
            enddo
            do nn = 1,nneighi22
              d2i(nn) = d2i(nn) + scale*f*(Vr*d2BtotiR(nn) - Va*d2BtotiA(nn))
            enddo
            do nn = 1,nneighj22
              d2j(nn) = d2j(nn) + scale*f*(Vr*d2BtotjR(nn) - Va*d2BtotjA(nn))
            enddo
          endif
        endif
!
!  Add derivatives due to neighbours of j 
!
        if (lgrad1) then
          call d1adds(j,maxneigh,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nboatomRptr,nauatom,neqv,d1j,.false.)
          if (lgrad2) then
            call d2adds(j,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nfreeatomau,nboatomRptr,nauatom, &
                        neqv,d1j,d2j,.false.)
          endif
        endif
      endif
!
!  End condition section on i or j being associated with moving atom
!
      endif
!
!  End of loop over neighbours of i
!
      ni = ni + 1
    enddo
!
!  Add derivatives due to neighbours of i
!
    if (lgrad1) then
      call d1adds(i,maxneigh,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nboatomRptr,nauatom,neqv,d1i,.false.)
      if (lgrad2) then
        call d2adds(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nfreeatomau,nboatomRptr,nauatom, &
                    neqv,d1i,d2i,.false.)
      endif
    endif
  enddo
!
!  Free local memory
!
  deallocate(d2BtotjR,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2BtotjR')
  deallocate(d2BtotjA,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2BtotjA')
  deallocate(d2BtotiR,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2BtotiR')
  deallocate(d2BtotiA,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2BtotiA')
  deallocate(d2j,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2j')
  deallocate(d2i,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d2i')
  deallocate(d1BtotjR,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1BtotjR')
  deallocate(d1BtotjA,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1BtotjA')
  deallocate(d1BtotiR,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1BtotiR')
  deallocate(d1BtotiA,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1BtotiA')
  deallocate(d1j,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1j')
  deallocate(d1i,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','d1i')
  deallocate(zneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','zneigh')
  deallocate(yneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','yneigh')
  deallocate(xneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','xneigh')
  deallocate(rneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','rneigh')
  deallocate(nbopotptr,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nbopotptr')
  deallocate(neighno,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','neighno')
  deallocate(rBOcutmax,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','rBOcutmax')
  deallocate(nneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nneigh')
  deallocate(nfreeatomau,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nfreeatomau')
  deallocate(nfreeatom,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nfreeatom')
  deallocate(nauatom,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nauatom')
  deallocate(lopanyneigh,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','lopanyneigh')
  deallocate(latomdone,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','latomdone')
  deallocate(lanyau,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','lanyau')
  deallocate(nboatomRptr,stat=status)
  if (status/=0) call deallocate_error('bondordersd2','nboatomRptr')
!
  t2 = cputime()
  tbondorder = tbondorder + t2 - t1
!
  return
  end
