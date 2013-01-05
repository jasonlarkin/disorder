  subroutine bondordermd(ebondorder,lgrad1)
!
!  Calculates the energy and up to first derivatives for the Bond Order potentials.
!
!  On entry : 
!
!  lgrad1          = if .true. calculate the first derivatives
!
!  On exit :
!
!  ebondorder      = the value of the energy contribution
!
!  11/03 Created from bondorder.f
!  12/03 Maxneigh can now be dynamically changed
!   6/04 M coefficient added for attractive/repulsive terms
!   9/04 Algorithm for parallel case changed in that looping over shells
!        is now included
!   9/04 Separate spatial decomposition added for BO potentials
!  10/04 Neighbour subroutine introduced
!  11/04 Missing definition of maxxy added
!  10/05 I/O made parallel only
!   2/07 ettach addition moved outside lseok condition
!   5/07 QM/MM schemes added
!   6/07 nboatom and pointers added as dummys for calls to d1add/d2add
!   6/07 Structure of arrays for storing distribution changed to 1-D
!  11/07 Unused variables cleaned up
!   4/08 Modified for variable domain size in spatial algorithm
!   4/08 Call to d1add modified
!   4/08 xvec1cell replaced by xvec2cell etc
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
  use configurations, only : nregionno, nregions, lsliceatom, nregiontype, QMMMmode
  use control,        only : keyword, lseok
  use current
  use energies,       only : eattach, esregion12, esregion2
  use iochannels
  use neighbours
  use optimisation,   only : lfreeze, lopf
  use parallel
  use spatialbo,      only : lspatialok => lspatialBOok
  use spatialbo,      only : natomcell => natomcellbo
  use spatialbo,      only : natomnodeptr => natomnodeptrbo
  use spatialbo,      only : natompernode => natompernodebo
  use spatialbo,      only : ncellsearch => ncellsearchbo
  use spatialbo,      only : nspcell => nspcellbo
  use spatialbo,      only : nspcellat => nspcellatbo
  use spatialbo,      only : nspcellatptr => nspcellatptrbo
  use spatialbo,      only : nspcellat1ptr => nspcellat1ptrbo
  use spatialbo,      only : nspcellatptrcell => nspcellatptrcellbo
  use spatialbo,      only : xinbox => xinboxbo
  use spatialbo,      only : yinbox => yinboxbo
  use spatialbo,      only : zinbox => zinboxbo
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)                       :: ebondorder
  logical,     intent(in)                        :: lgrad1
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ic
  integer(i4)                                    :: ii
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind
  integer(i4)                                    :: ind2
  integer(i4)                                    :: indn
  integer(i4)                                    :: itmp
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: k
  integer(i4)                                    :: kc
  integer(i4)                                    :: kmax
  integer(i4)                                    :: l
  integer(i4)                                    :: m
  integer(i4)                                    :: maxneigh2
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: maxx
  integer(i4)                                    :: mA
  integer(i4)                                    :: mR
  integer(i4)                                    :: n
  integer(i4)                                    :: n1j
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
  integer(i4)                                    :: ndone
  integer(i4), dimension(:),   allocatable, save :: ndoneptr
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: nik
  integer(i4)                                    :: njk
  integer(i4)                                    :: nmin
  integer(i4)                                    :: nn
  integer(i4)                                    :: nn2
  integer(i4)                                    :: nnshell
  integer(i4)                                    :: npki   
  integer(i4)                                    :: npkj
  integer(i4)                                    :: nptr
  integer(i4), dimension(:,:), allocatable, save :: neighno
  integer(i4), dimension(:),   allocatable, save :: nfreeatom
  integer(i4), dimension(:),   allocatable, save :: nneigh
  integer(i4)                                    :: nneighi2
  integer(i4)                                    :: nneighj2
  integer(i4)                                    :: nregioni
  integer(i4)                                    :: nregionj
  integer(i4)                                    :: nregiontypi
  integer(i4)                                    :: nregiontypj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: ntypj
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: lalreadydone
  logical,     dimension(:),   allocatable, save :: latomdone
  logical,     dimension(:),   allocatable, save :: latomdone2
  logical                                        :: lattach
  logical                                        :: lfound
  logical                                        :: lmaxneighok
  logical                                        :: lok
  logical                                        :: lQMMMok
  logical                                        :: lreg2one
  logical                                        :: lreg2pair
  logical                                        :: lslicei
  logical                                        :: lslicej
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
  real(dp)                                       :: bR22
  real(dp)                                       :: btotA
  real(dp)                                       :: btotR
  real(dp)                                       :: cputime
  real(dp),    dimension(:),   allocatable, save :: d1i
  real(dp),    dimension(:),   allocatable, save :: d1j
  real(dp),    dimension(:),   allocatable, save :: d1BtotiA
  real(dp),    dimension(:),   allocatable, save :: d1BtotiR
  real(dp),    dimension(:),   allocatable, save :: d1BtotjA
  real(dp),    dimension(:),   allocatable, save :: d1BtotjR
  real(dp)                                       :: dexpijkdr
  real(dp)                                       :: dexpjikdr
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
  real(dp)                                       :: rbijsumA1
  real(dp)                                       :: rbijsumR1
  real(dp)                                       :: rbjisumA1
  real(dp)                                       :: rbjisumR1
  real(dp),    dimension(:),   allocatable, save :: rBOcutmax
  real(dp)                                       :: rij
  real(dp)                                       :: rik
  real(dp)                                       :: rjk
  real(dp)                                       :: r2
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
  real(dp)                                       :: xdiff
  real(dp)                                       :: ydiff
  real(dp)                                       :: zdiff
  real(dp)                                       :: xi
  real(dp)                                       :: yi     
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xki
  real(dp)                                       :: yki
  real(dp)                                       :: zki
  real(dp)                                       :: xkj
  real(dp)                                       :: ykj
  real(dp)                                       :: zkj
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
  real(dp)                                       :: zAi3
  real(dp)                                       :: zAj3
  real(dp)                                       :: zRi3
  real(dp)                                       :: zRj3
!
  t1 = cputime()
  allocate(nboatomRptr(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','nboatomRptr')
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
  allocate(lalreadydone(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','lalreadydone')
  allocate(latomdone(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','latomdone')
  allocate(latomdone2(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','latomdone2')
  allocate(lopanyneigh(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','lopanyneigh')
  allocate(ndoneptr(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','ndoneptr')
  allocate(nfreeatom(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','nfreeatom')
  allocate(nneigh(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','nneigh')
  allocate(rBOcutmax(numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','rBOcutmax')
!
!  Reinitialisation point should maxneigh be increased             
!
100 continue
  lmaxneighok = .true.
  if (allocated(rneigh)) then
    deallocate(d1BtotjR,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1BtotjR')
    deallocate(d1BtotjA,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1BtotjA')
    deallocate(d1BtotiR,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1BtotiR')
    deallocate(d1BtotiA,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1BtotiA')
    deallocate(d1j,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1j')
    deallocate(d1i,stat=status)
    if (status/=0) call deallocate_error('bondordermd','d1i')
    deallocate(zneigh,stat=status)
    if (status/=0) call deallocate_error('bondordermd','zneigh')
    deallocate(yneigh,stat=status)
    if (status/=0) call deallocate_error('bondordermd','yneigh')
    deallocate(xneigh,stat=status)
    if (status/=0) call deallocate_error('bondordermd','xneigh')
    deallocate(rneigh,stat=status)
    if (status/=0) call deallocate_error('bondordermd','rneigh')
    deallocate(nbopotptr,stat=status)
    if (status/=0) call deallocate_error('bondordermd','nbopotptr')
    deallocate(neighno,stat=status)
    if (status/=0) call deallocate_error('bondordermd','neighno')
  endif
!
!  Initialise Bond Order energy
!
  ebondorder = 0.0_dp
!
!  Set parameter for pairwise storage memory
!
  maxneigh2 = maxneigh + maxneigh*(maxneigh + 1)/2
!
!  Allocate local memory
!
  allocate(neighno(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','neighno')
  allocate(nbopotptr(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','nbopotptr')
  allocate(rneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','rneigh')
  allocate(xneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','xneigh')
  allocate(yneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','yneigh')
  allocate(zneigh(maxneigh,numat),stat=status)
  if (status/=0) call outofmemory('bondordermd','zneigh')
  if (lgrad1) then
    allocate(d1i(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1i')
    allocate(d1j(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1j')
    allocate(d1BtotiA(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotiA')
    allocate(d1BtotiR(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotiR')
    allocate(d1BtotjA(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotjA')
    allocate(d1BtotjR(maxneigh2),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotjR')
  else
    allocate(d1i(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1i')
    allocate(d1j(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1j')
    allocate(d1BtotiA(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotiA')
    allocate(d1BtotiR(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotiR')
    allocate(d1BtotjA(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotjA')
    allocate(d1BtotjR(1),stat=status)
    if (status/=0) call outofmemory('bondordermd','d1BtotjR')
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
!
!  Set up logical array of atoms done, so that only those needed are done in parallel
!
  latomdone(1:numat) = .false.
!
!  Compute neighbour list
!
  call getBOneighbour(maxneigh,rBOcutmax,nBOpotptr,nneigh,neighno,rneigh, &
                      xneigh,yneigh,zneigh,latomdone,lmaxneighok)
!**********************************************************************
!  If maxneigh has been exceeded, increase value and return to start  *
!**********************************************************************
  if (.not.lmaxneighok) then
    do i = 1,numat
      if (nneigh(i).gt.maxneigh) maxneigh = nneigh(i)
    enddo
    if (ioproc.and.index(keyword,'verb').ne.0) then
      write(ioout,'(/,''  Increasing maxneigh to '',i6)') maxneigh
    endif
    goto 100
  endif
  if (nprocs.gt.1) then
!******************************
!  Parallel additional setup  *
!******************************
!
!  Loop over atoms again to do those that are neighbours of the main atoms
!  since these will be needed in the energy evaluation. This process has to
!  be done once (in contrast to Brenner potential) since there are no
!  torsional terms. However, construct is left in here in case we want to 
!  add torsions later!
!
    if (lspatialok) then 
!******************** 
!  Spatial version  *
!********************
      maxxy = nspcell(1)*nspcell(2)
      maxx  = nspcell(1)
!                 
!  Initialise atom done once pointer
!                   
      ndone = 0
      do i = 1,numat
        lalreadydone(i) = .false.
      enddo
!
!  Loop over shells
!
      do nnshell = 1,1
        latomdone2(1:numat) = latomdone(1:numat)
        if (nnshell.eq.1) then
          kmax = natompernode
        else
          kmax = numat
        endif
        do kc = 1,kmax
          if (nnshell.eq.1) then
            k = natomnodeptr(kc)
          else
            k = kc
          endif
          if (latomdone2(k)) then
            do l = 1,nneigh(k)
              i = neighno(l,k)
              if (.not.latomdone(i)) then
                nati = nat(i)
                ntypi = nftype(i)
                nneigh(i) = 0
!
!  Find cell containing central image of i 
!
                ind = natomcell(i)
                ind2 = ind - 1
                iz = ind2/maxxy
                ind2 = ind2 - maxxy*iz
                iy = ind2/maxx
                ix = ind2 - maxx*iy + 1
                iy = iy + 1 
                iz = iz + 1
!
                xi = xinbox(i)
                yi = yinbox(i)
                zi = zinbox(i)
!
!  Set cell search bounds
!
                nspupper(1) = min(ix+ncellsearch(1),nspcell(1))
                nspupper(2) = min(iy+ncellsearch(2),nspcell(2))
                nspupper(3) = min(iz+ncellsearch(3),nspcell(3))
                nsplower(1) = max(ix-ncellsearch(1),1)
                nsplower(2) = max(iy-ncellsearch(2),1)
                nsplower(3) = max(iz-ncellsearch(3),1)
!
!  Compute square of cut-off for distance checking
!
                bR22 = rBOcutmax(i)**2
!
!  Loop over neighbouring cells
!
                do imz = nsplower(3),nspupper(3)
                  do imy = nsplower(2),nspupper(2)
                    do imx = nsplower(1),nspupper(1)
                      indn = (imz-1)*maxxy + (imy-1)*maxx + imx
!
!  Loop over atoms within neighbouring cells
!
                      nj = nspcellat(indn)
                      n1j = nspcellat1ptr(indn)
                      do jj = 1,nj
                        j = nspcellatptr(n1j+jj)
!
!  Exclude self term
!               
                        if (.not.lalreadydone(j)) then
                          if (i.ne.j.or.ind.ne.indn) then
                            if (latomdone(j)) then
!
!  Atom has already been done and therefore information can be copied
!
                              do ii = 1,nneigh(j)
                                if (neighno(ii,j).eq.i) then
                                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                                    lmaxneighok = .false.
                                    nneigh(i) = nneigh(i) + 1
                                  else
                                    nneigh(i) = nneigh(i) + 1
                                    neighno(nneigh(i),i) = j
                                    nbopotptr(nneigh(i),i) = nbopotptr(ii,j)
                                    rneigh(nneigh(i),i) = rneigh(ii,j)
                                    xneigh(nneigh(i),i) = - xneigh(ii,j)
                                    yneigh(nneigh(i),i) = - yneigh(ii,j)
                                    zneigh(nneigh(i),i) = - zneigh(ii,j)
                                  endif
                                endif
                              enddo
!
!  Set pointer to avoid repetition of this atom in the copy phase
!
                              ndone = ndone + 1
                              ndoneptr(ndone) = j
                              lalreadydone(j) = .true.
                            else
                              natj = nat(j)
                              ntypj = nftype(j)
!
!  Set centre cell coordinate differences
!
                              jc = nspcellatptrcell(n1j+jj)
                              xji = xvec2cell(jc) + xinbox(j) - xi
                              yji = yvec2cell(jc) + yinbox(j) - yi
                              zji = zvec2cell(jc) + zinbox(j) - zi
                              r2 = xji*xji + yji*yji + zji*zji
                              if (r2 .lt. bR22) then
                                m = 0
                                lok = .false.
                                do while (m.lt.nbopot.and..not.lok)
                                  m = m + 1
                                  if (nati.eq.nBOspec1(m).and.(ntypi.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0).and. &
                                      natj.eq.nBOspec2(m).and.(ntypj.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0)) then
                                    lok = (r2.lt.rBOmax(m)**2)
                                  elseif (nati.eq.nBOspec2(m).and.(ntypi.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0).and. &
                                      natj.eq.nBOspec1(m).and.(ntypj.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0)) then
                                    lok = (r2.lt.rBOmax(m)**2)
                                  endif
                                enddo
                                if (lok) then
                                  if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                                    lmaxneighok = .false.
                                    nneigh(i) = nneigh(i) + 1
                                  else
                                    rij = sqrt(r2)
                                    nneigh(i) = nneigh(i) + 1
                                    neighno(nneigh(i),i) = j
                                    nbopotptr(nneigh(i),i) = m
                                    rneigh(nneigh(i),i) = rij
                                    xneigh(nneigh(i),i) = xji
                                    yneigh(nneigh(i),i) = yji
                                    zneigh(nneigh(i),i) = zji
!
!  Set pointer to avoid repetition of this atom in the search phase
!
                                    ndone = ndone + 1
                                    ndoneptr(ndone) = j
                                    lalreadydone(j) = .true.
                                  endif
                                endif
                              endif
                            endif
                          endif
                        endif
                      enddo
                    enddo
                  enddo
                enddo
!
!  Set flag for this atom to indicate that it has been done
!
                latomdone(i) = .true.
!
!  Clear already done pointers for this atom
!
                do j = 1,ndone
                  lalreadydone(ndoneptr(j)) = .false.
                enddo
                ndone = 0
              endif
            enddo
          endif
        enddo
      enddo
    else
!************************
!  Non-spatial version  *
!************************
      do nnshell = 1,1
        latomdone2(1:numat) = latomdone(1:numat)
        if (nnshell.eq.1) then
          kmax = natompernode   
        else
          kmax = numat
        endif
        do kc = 1,kmax
          if (nnshell.eq.1) then
            k = natomnodeptr(kc)
          else
            k = kc   
          endif
          if (latomdone2(k)) then
            do l = 1,nneigh(k)
              i = neighno(l,k)
!
              if (.not.latomdone(i)) then
                nneigh(i) = 0
                nati = nat(i)
                ntypi = nftype(i)
!     
!  Compute square of cut-off for distance checking   
!     
                bR22 = rBOcutmax(i)**2
!
!  Loop over atoms
!  
                do j = 1,numat
                  if (latomdone(j)) then
!  
!  Atom has already been done and therefore information can be copied      
!  
                    do ii = 1,nneigh(j)
                      if (neighno(ii,j).eq.i) then
                        if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                          lmaxneighok = .false.
                          nneigh(i) = nneigh(i) + 1
                        else
                          nneigh(i) = nneigh(i) + 1
                          neighno(nneigh(i),i) = j
                          nbopotptr(nneigh(i),i) = nbopotptr(ii,j)
                          rneigh(nneigh(i),i) = rneigh(ii,j)
                          xneigh(nneigh(i),i) = - xneigh(ii,j)
                          yneigh(nneigh(i),i) = - yneigh(ii,j)
                          zneigh(nneigh(i),i) = - zneigh(ii,j)
                        endif     
                      endif     
                    enddo       
                  else
                    natj = nat(j)  
                    ntypj = nftype(j)
!
!  Set centre cell coordinate differences
!
                    xji0 = xclat(j) - xclat(i)
                    yji0 = yclat(j) - yclat(i)
                    zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
                    do ii = 1,iimax2
!
!  Exclude self term
!
                      if (i.ne.j.or.ii.ne.iimid2) then
                        xji = xji0 + xvec2cell(ii)
                        yji = yji0 + yvec2cell(ii)
                        zji = zji0 + zvec2cell(ii)
                        r2 = xji*xji + yji*yji + zji*zji
                        if (r2 .lt. bR22) then
                          m = 0
                          lok = .false.
                          do while (m.lt.nbopot.and..not.lok)
                            m = m + 1
                            if (nati.eq.nBOspec1(m).and.(ntypi.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0).and. &
                                natj.eq.nBOspec2(m).and.(ntypj.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0)) then
                              lok = (r2.lt.rBOmax(m)**2)
                            elseif (nati.eq.nBOspec2(m).and.(ntypi.eq.nBOtyp2(m).or.nBOtyp2(m).eq.0).and. &
                                natj.eq.nBOspec1(m).and.(ntypj.eq.nBOtyp1(m).or.nBOtyp1(m).eq.0)) then
                              lok = (r2.lt.rBOmax(m)**2)
                            endif
                          enddo
                          if (lok) then
                            if (nneigh(i).ge.maxneigh.or..not.lmaxneighok) then
                              lmaxneighok = .false.
                              nneigh(i) = nneigh(i) + 1
                            else
                              rij = sqrt(r2)
                              nneigh(i) = nneigh(i) + 1
                              neighno(nneigh(i),i) = j
                              nbopotptr(nneigh(i),i) = m
                              rneigh(nneigh(i),i) = rij
                              xneigh(nneigh(i),i) = xji
                              yneigh(nneigh(i),i) = yji
                              zneigh(nneigh(i),i) = zji
                            endif
                          endif
                        endif
                      endif
                    enddo
                  endif
                enddo
!
!  Set flag for this atom to indicate that it has been done
!
                latomdone(i) = .true.
              endif
            enddo
          endif
        enddo
      enddo
    endif
!**********************************************************************
!  If maxneigh has been exceeded, increase value and return to start  *
!**********************************************************************
    if (.not.lmaxneighok) then
      do i = 1,numat
        if (nneigh(i).gt.maxneigh) maxneigh = nneigh(i)
      enddo
      goto 100
    endif
  endif
!*******************************
!  Sort neighbours into order  *
!******************************* 
  if (lspatialok) then
    do i = 1,numat
      if (latomdone(i)) then
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
      endif
    enddo
  endif
!*********************************************
!  Calculate numbers of neighbours for each  *
!*********************************************
  do i = 1,numat
    if (latomdone(i)) then
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
    endif
  enddo
  if (ioproc.and.index(keyword,'debu').ne.0) then
    write(ioout,'(/,''  Number of neighbours for atoms :'',/)')
    write(ioout,'(''  i'',2x,''Nat'',3x,''N'')')
    do ic = 1,natompernode
      i = natomnodeptr(ic)
      write(ioout,'(i4,1x,i3,1x,i6)') i,nat(i),nneigh(i)
    enddo
    write(ioout,'(/,''  Neighbours of atoms :'',/)')
    do ic = 1,natompernode
      i = natomnodeptr(ic)
      write(ioout,'(i4,8(1x,i4))') i,(neighno(nn,i),nn=1,nneigh(i))
    enddo
  endif
!*****************************
!  Loop over pairs of atoms  *
!*****************************
  do ic = 1,natompernode
    i = natomnodeptr(ic)
!orig do i = 1,numat
!
!  Set variables relating to i
!
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft + nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
    lslicei = lsliceatom(nsft + nrelat(i))
!
!  Set total number of distances for neighbours of i
!
    nneighi2 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2
!
!  Initialise derivative storage for neighbours of i
!
    if (lgrad1) then
      d1i(1:nneighi2) = 0.0_dp
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
      if (lopanyneigh(i).or.lopanyneigh(j)) then
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
      nregionj = nregionno(nsft + nrelat(j))
      nregiontypj = nregiontype(nregionj,ncf)
      lslicej = lsliceatom(nsft + nrelat(j))
!
!  Set up i-j quantities
!
      rij = rneigh(ni,i)
      xji = xneigh(ni,i)
      yji = yneigh(ni,i)
      zji = zneigh(ni,i)
      rrij = 1.0_dp/rij
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
!
!  Initialise derivative storage for neighbours of i
!
      if (lgrad1) then
        d1j(1:nneighj2) = 0.0_dp
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
!
!  QM/MM handling : i & j are both QM atoms => exclude
!
      lQMMMok = .true.
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.nregiontypj.eq.1) lQMMMok = .false.
      endif
      if (nboij.gt.0.and.lQMMMok) then
!****************************************
!  Valid two-body bond order potential  *
!****************************************
!
!  Calculate fij
!
        call ctaper(rij,rBOmin(nboij),rBOmax(nboij),f,dfdr,d2fdr2,d3fdr3,lgrad1,.false.,.false.)
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
                call ctaper(rik,rBOmin(npki),rBOmax(npki),fik,dfikdr,d2fikdr2,d3fikdr3,lgrad1,.false.,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeR(nboRij).ne.1) then
                  call GthetaBO(xji,yji,zji,xki,yki,zki,BOccoeffR(nboRij),BOdcoeffR(nboRij), &
                    BOhcoeffR(nboRij),Gijk,dGijkdr,d2Gijkdr2,d3Gijkdr3,lgrad1,.false.,.false.)
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
                  if (mR.ge.1) then
                    dexpijkdr = dble(mR)*(BOlcoeffR(nboRij)**mR)*((rij-rik)**(mR-1))*expijk
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
                call ctaper(rik,rBOmin(npki),rBOmax(npki),fik,dfikdr,d2fikdr2,d3fikdr3,lgrad1,.false.,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeA(nboAij).ne.1) then
                  call GthetaBO(xji,yji,zji,xki,yki,zki,BOccoeffA(nboAij),BOdcoeffA(nboAij), &
                    BOhcoeffA(nboAij),Gijk,dGijkdr,d2Gijkdr2,d3Gijkdr3,lgrad1,.false.,.false.)
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
                  if (mA.ge.1) then
                    dexpijkdr = dble(mA)*(BOlcoeffA(nboAij)**mA)*((rij-rik)**(mA-1))*expijk
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
                call ctaper(rjk,rBOmin(npkj),rBOmax(npkj),fjk,dfjkdr,d2fjkdr2,d3fjkdr3,lgrad1,.false.,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeR(nboRji).ne.1) then
                  call GthetaBO(-xji,-yji,-zji,xkj,ykj,zkj,BOccoeffR(nboRji),BOdcoeffR(nboRji), &
                    BOhcoeffR(nboRji),Gjik,dGjikdr,d2Gjikdr2,d3Gjikdr3,lgrad1,.false.,.false.)
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
                  if (mR.ge.1) then
                    dexpjikdr = dble(mR)*(BOlcoeffR(nboRji)**mR)*((rij-rjk)**(mR-1))*expjik
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
                call ctaper(rjk,rBOmin(npkj),rBOmax(npkj),fjk,dfjkdr,d2fjkdr2,d3fjkdr3,lgrad1,.false.,.false.)
!
!  Calculate Gijk
!
                if (nBOtypeA(nboAji).ne.1) then
                  call GthetaBO(-xji,-yji,-zji,xkj,ykj,zkj,BOccoeffA(nboAji),BOdcoeffA(nboAji), &
                    BOhcoeffA(nboAji),Gjik,dGjikdr,d2Gjikdr2,d3Gjikdr3,lgrad1,.false.,.false.)
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
                  if (mA.ge.1) then
                    dexpjikdr = dble(mA)*(BOlcoeffA(nboAji)**mA)*((rij-rjk)**(mA-1))*expjik
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
        endif
!
!  Calculate total i-j potential
!
        BtotA = 0.5_dp*(bijA + bjiA) 
        BtotR = 0.5_dp*(bijR + bjiR) 
        eij = scale*f*(BtotR*Vr - BtotA*Va)
!
!  Add to surface energy totals if appropriate
!
        if (lseok) then
          if (lreg2one) then
            esregion12 = esregion12 + eij
          elseif (lreg2pair) then
            esregion2 = esregion2 + eij
          else
            ebondorder = ebondorder + eij
          endif
        else
          ebondorder = ebondorder + eij
        endif
        if (lattach) eattach = eattach + eij
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
        endif
!
!  Add derivatives due to neighbours of j 
!
        if (lgrad1) then
          call d1add(j,maxneigh,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nboatomRptr,d1j,.false.)
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
      call d1add(i,maxneigh,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nboatomRptr,d1i,.false.)
    endif
  enddo
!
!  Free local memory
!
  deallocate(d1BtotjR,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1BtotjR')
  deallocate(d1BtotjA,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1BtotjA')
  deallocate(d1BtotiR,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1BtotiR')
  deallocate(d1BtotiA,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1BtotiA')
  deallocate(d1j,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1j')
  deallocate(d1i,stat=status)
  if (status/=0) call deallocate_error('bondordermd','d1i')
  deallocate(zneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','zneigh')
  deallocate(yneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','yneigh')
  deallocate(xneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','xneigh')
  deallocate(rneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','rneigh')
  deallocate(nbopotptr,stat=status)
  if (status/=0) call deallocate_error('bondordermd','nbopotptr')
  deallocate(neighno,stat=status)
  if (status/=0) call deallocate_error('bondordermd','neighno')
  deallocate(rBOcutmax,stat=status)
  if (status/=0) call deallocate_error('bondordermd','rBOcutmax')
  deallocate(nneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','nneigh')
  deallocate(nfreeatom,stat=status)
  if (status/=0) call deallocate_error('bondordermd','nfreeatom')
  deallocate(ndoneptr,stat=status)
  if (status/=0) call deallocate_error('bondordermd','ndoneptr')
  deallocate(lopanyneigh,stat=status)
  if (status/=0) call deallocate_error('bondordermd','lopanyneigh')
  deallocate(latomdone2,stat=status)
  if (status/=0) call deallocate_error('bondordermd','latomdone2')
  deallocate(latomdone,stat=status)
  if (status/=0) call deallocate_error('bondordermd','latomdone')
  deallocate(lalreadydone,stat=status)
  if (status/=0) call deallocate_error('bondordermd','lalreadydone')
  deallocate(nboatomRptr,stat=status)
  if (status/=0) call deallocate_error('bondorder','nboatomRptr')
!
  t2 = cputime()
  tbondorder = tbondorder + t2 - t1
!
  return
  end
