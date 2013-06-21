  subroutine setmols
!
!  Determines number of molecules and their constituent atoms.
!  This version uses a spatial decomposition algorithm.
!
!  Assignment of nmolind is made here for clusters - for bulk
!  the values are set in molind
!  
!  moldim  = dimensionality of molecule (0,1,2,3)
!  moldimi = index to periodic directions of molecule
!
!  Guide to molecule dimensionality indices:
!
!    if moldim = 0: moldimi = 0
!    if moldim = 1: moldimi = 1 => x
!                           = 2 => y
!                           = 3 => z
!    if moldim = 2: moldimi = 1 => xy
!                           = 2 => xz
!                           = 3 => yz
!    if moldim = 3: moldimi = 0
!
!   4/08 Created from setmol
!   4/08 Use of nlow being present to a number replace by use of logical lfirstlow
!   4/08 Recursive algorithm for natmol update added
!   5/08 Bug in ndone/ldone algorithm fixed
!   6/09 New pointers to atoms in molecules calculated once nmol is finalised
!   2/10 For EVB calculation, add individual atoms as molecules
!  12/12 Handing of nbat/nmlist changes so they are only reallocated and set before use
!  12/12 Parallelisation corrected as the original one didn't work in this case
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use control
  use current
  use element
  use molecule
  use parallel
  use reallocate
  use spatial
  use species
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ib
  integer(i4)                                  :: ic
  integer(i4)                                  :: ico
  integer(i4)                                  :: icx
  integer(i4)                                  :: icy
  integer(i4)                                  :: icz
  integer(i4)                                  :: ii
  integer(i4)                                  :: iii
  integer(i4)                                  :: iimin
  integer(i4)                                  :: imx
  integer(i4)                                  :: imy
  integer(i4)                                  :: imz
  integer(i4)                                  :: in
  integer(i4)                                  :: ind1
  integer(i4)                                  :: ind2
  integer(i4)                                  :: indb
  integer(i4)                                  :: indn
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: ixyz
  integer(i4)                                  :: j
  integer(i4)                                  :: jc
  integer(i4)                                  :: jcx
  integer(i4)                                  :: jcy
  integer(i4)                                  :: jcz
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: l
  integer(i4)                                  :: m
  integer(i4)                                  :: maxx
  integer(i4)                                  :: maxxy
  integer(i4)                                  :: n1i
  integer(i4)                                  :: n1j
  integer(i4)                                  :: na
  integer(i4)                                  :: nb1
  integer(i4)                                  :: nb2
  integer(i4), dimension(:), allocatable, save :: nbat
  integer(i4)                                  :: nbond
  integer(i4)                                  :: nbondcheck
  integer(i4)                                  :: ndone
  integer(i4), dimension(:), allocatable, save :: ndonelist
  integer(i4), dimension(:), allocatable, save :: nexist
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: ni1
  integer(i4)                                  :: nj1
  integer(i4)                                  :: ninc
  integer(i4)                                  :: nlow
  integer(i4)                                  :: nml
  integer(i4), dimension(:), allocatable, save :: nmlist
  integer(i4)                                  :: nmolfin
  integer(i4)                                  :: nbondsearch(3)
  integer(i4)                                  :: nsplower(3)
  integer(i4)                                  :: nspupper(3)
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntj
  integer(i4)                                  :: nti1
  integer(i4)                                  :: ntj1
  integer(i4)                                  :: ntodo
  integer(i4)                                  :: ntodonew
  integer(i4), dimension(:), allocatable, save :: ntodolist
  integer(i4), dimension(:), allocatable, save :: ntodolistnew
  integer(i4)                                  :: ntotbonds
  integer(i4)                                  :: status
  logical                                      :: lbondok
  logical                                      :: lduplicate
  logical                                      :: lfirstlow
  logical,     dimension(:), allocatable, save :: ldone
  real(dp)                                     :: r2
  real(dp)                                     :: r2min
  real(dp)                                     :: rcut
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rj
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
  real(dp)                                     :: xji
  real(dp)                                     :: yji
  real(dp)                                     :: zji
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  lbondok = .true.
!
!  Check that spatial decomposition is OK
!
  if (.not.lspatialOK) then
    call outerror('setmols called when spatial decomposition is not valid!',0_i4)
    call stopnow('setmols')
  endif
!
!  Initialise arrays
!
  do i = 1,numat
    natmol(i) = 0
    nmolind(i) = 0
    nbonds(i) = 0
    do j = 1,maxbond
      nbonded(j,i) = 0
    enddo
  enddo
  do i = 1,maxmol
    moldim(i) = 0
    moldimi(i) = 0
  enddo
!
!  Allocate local memory that depends on numat
!
  allocate(nexist(numat),stat=status)
  if (status/=0) call outofmemory('setmols','nexist')
  allocate(ldone(numat),stat=status)
  if (status/=0) call outofmemory('setmols','ldone')
  allocate(ndonelist(numat),stat=status)
  if (status/=0) call outofmemory('setmols','ndonelist')
  allocate(ntodolist(numat),stat=status)
  if (status/=0) call outofmemory('setmols','ntodolist')
  allocate(ntodolistnew(numat),stat=status)
  if (status/=0) call outofmemory('setmols','ntodolistnew')
!
  ldone(1:numat) = .false.
  nmol = 0
!
  maxxy = nspcell(1)*nspcell(2)
  maxx  = nspcell(1)
!
!  Set maximum number of cells to be search based on largest bond length
!
  rcut = 0.0_dp
  do i = 1,nspec
    ni = natspec(i)
    if (ni.gt.maxele) ni = ni - maxele
    ri = rcov(ni)
    rcut = max(rcut,ri)
  enddo
  rcut = 2.0_dp*rcut
  nbondsearch(1) = 1 + rcut/rnearestx
  nbondsearch(2) = 1 + rcut/rnearesty
  nbondsearch(3) = 1 + rcut/rnearestz
!******************************
! (1) Generate molecule lists *
!******************************
!  
!  Loop over all local spatial cells except buffer regions
!   
  do ixyz = 1,ncellpernode
    ind1 = ncellnodeptr(ixyz)
    ind2 = ind1 - 1
    iz = ind2/maxxy
    ind2 = ind2 - maxxy*iz
    iy = ind2/maxx
    ix = ind2 - maxx*iy + 1
    iy = iy + 1
    iz = iz + 1
    if (.not.lbuffercell(ixyz)) then
!  
!  Get number of atoms in this cell
!       
      ni = nspcellat(ind1)
      n1i = nspcellat1ptr(ind1)
!
!  Set cell search bounds
!
      nspupper(1) = min(ix+nbondsearch(1),nspcell(1))
      nspupper(2) = min(iy+nbondsearch(2),nspcell(2))
      nspupper(3) = min(iz+nbondsearch(3),nspcell(3))
      nsplower(1) = max(ix-nbondsearch(1),1)
      nsplower(2) = max(iy-nbondsearch(2),1)
      nsplower(3) = max(iz-nbondsearch(3),1)
!  
!  Outer loop over atoms within this cell
!       
      do in = 1,ni
        i = nspcellatptr(n1i+in)
        ic = nspcellatptrcell(n1i+in)
        call ind2toijk(ic,icx,icy,icz)
!  
!  Set coordinates of atom i
!           
        xi = xinbox(i)
        yi = yinbox(i)
        zi = zinbox(i)
!
!  Set other properties of i
!
        ni1 = nat(i)
        nti = nftype(i)
        ni = ni1
        if (ni.gt.maxele) ni = ni - maxele
        ri = rcov(ni)
!
!  Find all atoms bonded to atom i
!
        nbond = 0
        if (nconnect.gt.0) then
          do ico = 1,nconnect
            if (nconnectcfg(ico).eq.ncf) then
              if (n1connect(ico).eq.i) then
                nbond = nbond + 1
                nbonds(i) = nbonds(i) + 1
                if (nbond.gt.maxbond) then
                  maxbond = nbond + 2
                  call changemaxbond
                endif
                j = n2connect(ico)
                nbonded(nbond,i) = j
                nbondedtype(1,nbond,i) = nconnecttype(1,ico)
                nbondedtype(2,nbond,i) = nconnecttype(2,ico)
                if (nconnectind(ico).gt.0) then
                  nbondind(nbond,i) = nconnectind(ico)
                else
!
!  Find nearest image
!
                  xji = xinbox(j) - xi
                  yji = yinbox(j) - yi
                  zji = zinbox(j) - zi
                  r2min = 1000000.0_dp
                  iimin = 0
                  do iii = 1,iimax
                    xcrd = xji + xvec1cell(iii)
                    ycrd = yji + yvec1cell(iii)
                    zcrd = zji + zvec1cell(iii)
                    r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
                    if (r2.lt.r2min) then
                      r2min = r2
                      iimin = iii
                    endif
                  enddo
                  call lintoijk(ii,jj,kk,iimin,imaxl,jmaxl,kmaxl)
                  nbondind(nbond,i) = (ii+5) + 10*(jj+5) + 100*(kk+5)
                endif
!
!  If i = j, then add bond in opposite direction
!
                if (i.eq.j) then
                  nbond = nbond + 1
                  nbonds(i) = nbonds(i) + 1
                  if (nbond.gt.maxbond) then
                    maxbond = nbond + 2
                    call changemaxbond
                  endif
                  nbonded(nbond,i) = j
                  nbondedtype(1,nbond,i) = nconnecttype(1,ico)
                  nbondedtype(2,nbond,i) = nconnecttype(2,ico)
                  if (nconnectind(ico).gt.0) then
                    nbondind(nbond,i) = 1110 - nconnectind(ico)
                  endif
                endif
              elseif (n2connect(ico).eq.i) then
                nbond = nbond + 1
                nbonds(i) = nbonds(i) + 1
                if (nbond.gt.maxbond) then
                  maxbond = nbond + 2
                  call changemaxbond
                endif
                j = n1connect(ico)
                nbonded(nbond,i) = j
                nbondedtype(1,nbond,i) = nconnecttype(1,ico)
                nbondedtype(2,nbond,i) = nconnecttype(2,ico)
                if (nconnectind(ico).gt.0) then
                  nbondind(nbond,i) = 1110 - nconnectind(ico)
                else
!
!  Find nearest image
!
                  xji = xinbox(j) - xi
                  yji = yinbox(j) - yi
                  zji = zinbox(j) - zi
                  r2min = 1000000.0_dp
                  iimin = 0
                  do iii = 1,iimax
                    xcrd = xji + xvec1cell(iii)
                    ycrd = yji + yvec1cell(iii)
                    zcrd = zji + zvec1cell(iii)
                    r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
                    if (r2.lt.r2min) then
                      r2min = r2
                      iimin = iii
                    endif
                  enddo
                  call lintoijk(ii,jj,kk,iimin,imaxl,jmaxl,kmaxl)
                  nbondind(nbond,i) = (ii+5) + 10*(jj+5) + 100*(kk+5)
                endif
              endif
            endif
          enddo
        endif
!
!  Now set coordinates of i including cell contribution
!
        xi = xinbox(i) + xvec2cell(ic)
        yi = yinbox(i) + yvec2cell(ic)
        zi = zinbox(i) + zvec2cell(ic)
!
        if (.not.lnoautobond.and.ri.ne.0.0_dp) then
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
                  jc = nspcellatptrcell(n1j+jj)
                  call ind2toijk(jc,jcx,jcy,jcz)
!
!  Set properties of j
!
                  nj1 = nat(j)
                  ntj = nftype(j)
                  nj = nj1
                  if (nj.gt.maxele) nj = nj - maxele
                  rj = rcov(nj)
!
!  Check whether bond type is excluded
!
                  if (nnobo.gt.0) then
                    if (ni1.eq.nj1) then
                      indb = nj1 + 1000*ni1
                      if (nti.lt.ntj) then
                        nti1 = nti
                        ntj1 = ntj
                      else
                        nti1 = ntj
                        ntj1 = nti
                      endif
                    elseif (ni1.lt.nj1) then
                      indb = nj1 + 1000*ni1
                      nti1 = nti
                      ntj1 = ntj
                    else
                      indb = ni1 + 1000*nj1
                      nti1 = ntj
                      ntj1 = nti
                    endif
                    lbondok = .true.
                    ib = 1
                    do while (lbondok.and.(ib.le.nnobo))
                      if (indb.eq.nobond(ib)) then
                        nb1 = nobotyp(ib)/1000
                        nb2 = nobotyp(ib) - 1000*nb1
                        if ((nb1.eq.nti1.or.nb1.eq.0).and.(nb2.eq.ntj1.or.nb2.eq.0)) lbondok = .false.
                        if (ni1.eq.nj1.and.lbondok) then
                          if ((nb1.eq.ntj1.or.nb1.eq.0).and.(nb2.eq.nti1.or.nb2.eq.0)) lbondok = .false.
                        endif
                      endif
                      ib = ib + 1
                    enddo
                  endif
!
!  Distance check
!
                  if (rj.ne.0.0_dp.and.lbondok) then
                    rcut = rtol*(ri+rj)
                    rcut = rcut*rcut
                    if (rj.ne.0.0_dp) then
                      xji = xvec2cell(jc) + xinbox(j) - xi
                      yji = yvec2cell(jc) + yinbox(j) - yi
                      zji = zvec2cell(jc) + zinbox(j) - zi
                      rij = xji*xji + yji*yji + zji*zji
                      if (rij.le.rcut.and.rij.gt.0.0_dp.and.(i.ne.j.or.ind1.ne.indn)) then
!
!  Valid bond
!
                        nbond = nbond + 1
                        nbonds(i) = nbonds(i) + 1
                        if (nbond.gt.maxbond) then
                          maxbond = nbond + 2
                          call changemaxbond
                        endif
                        nbonded(nbond,i) = j
                        nbondedtype(1,nbond,i) = 1
                        nbondedtype(2,nbond,i) = 1
!
!  Set relative cell index
!
                        ixx = jcx - icx
                        iyy = jcy - icy
                        izz = jcz - icz
                        nbondind(nbond,i) = ixx + 5 + 10*(iyy + 5) + 100*(izz + 5)
!
!  Check that bond doesn't duplicate one already specified
!
                        lduplicate = .false.
                        nbondcheck = 1
                        do while (.not.lduplicate.and.nbondcheck.lt.nbond)
                          lduplicate = (nbonded(nbondcheck,i).eq.nbonded(nbond,i).and. &
                                        nbondind(nbondcheck,i).eq.nbondind(nbond,i))
                          nbondcheck = nbondcheck + 1
                        enddo
                        if (lduplicate) then
                          nbond = nbond - 1
                          nbonds(i) = nbonds(i) - 1
                        endif
                      endif
                    endif
                  endif
                enddo
              enddo
            enddo
          enddo
        endif
      enddo
    endif
!
!  End of loop over cells
!
  enddo
  if (nprocs.gt.0) then
!
!  Globalisation when in parallel - use nexist, nmlist and nbat as work space arrays
!
!  First globalise number of bonds per atom in nbonds
!
    call isumall(nbonds,nexist,numat,"setmols","nbonds")
    nbonds(1:numat) = nexist(1:numat)
!
!  Compute total number of bonds over all atoms (ntotbonds) and create workspace arrays of this dimension
!
    ntotbonds = 0
    do i = 1,numat
      ntotbonds = ntotbonds + nbonds(i)
    enddo
    allocate(nbat(ntotbonds),stat=status)
    if (status.ne.0) call outofmemory('setmols','nbat')
    allocate(nmlist(ntotbonds),stat=status)
    if (status.ne.0) call outofmemory('setmols','nmlist')
!
!  Now place elements of nbonded for bonds into workspace linear array
!
    ntotbonds = 0
    do i = 1,numat
      do k = 1,nbonds(i)
        nbat(ntotbonds+k) = nbonded(k,i)
      enddo
      ntotbonds = ntotbonds + nbonds(i)
    enddo
!
!  Globalise bond indices
!
    call isumall(nbat,nmlist,ntotbonds,"setmols","nbonded")
!
!  Return global values back to nbonded
!
    ntotbonds = 0
    do i = 1,numat
      do k = 1,nbonds(i)
        nbonded(k,i) = nmlist(ntotbonds+k)
      enddo
      ntotbonds = ntotbonds + nbonds(i)
    enddo
!
!  Free workspace memory
!
    deallocate(nmlist,stat=status)
    if (status/=0) call deallocate_error('setmols','nmlist')
    deallocate(nbat,stat=status)
    if (status/=0) call deallocate_error('setmols','nbat')
  endif
!
!  Allocate local memory now that we know the value of maxbond, with 1 added just in case for the self term
!
  allocate(nbat(maxbond+1),stat=status)
  if (status.ne.0) call outofmemory('setmols','nbat')
  allocate(nmlist(maxbond+1),stat=status)
  if (status.ne.0) call outofmemory('setmols','nmlist')
!
!
!  Use connectivity information to find unique molecules
!
  do i = 1,numat
    nbond = nbonds(i)
    if (nbond.gt.0) then
!
!  Include reference atom i in list
!
      nbond = nbond + 1
      if (nbond.gt.maxbond) then
        maxbond = nbond + 2
        call changemaxbond
      endif
!
!  Copy initial data into nbat and then add self term
!
      do k = 1,nbond - 1
        nbat(k) = nbonded(k,i)
      enddo
      nbat(nbond) = i
!
!  Check if any of the bonded atoms are in molecule already
!  and find lowest molecule number
!
      nml = 0
      lfirstlow = .true.
      do k = 1,nbond
        na = natmol(nbat(k))
        if (na.ne.0) then
          nml = nml + 1
          nmlist(nml) = na
          if (lfirstlow) then
            lfirstlow = .false.
            nlow = na
          else
            if (na.lt.nlow) nlow = na
          endif
        endif
      enddo
!
!  If connected atom is already classified apply lowest number to all
!  atoms in molecule else create new molecule. Also apply to all atoms
!  with same molecule number everywhere.
!
      if (lfirstlow) then
        nmol = nmol + 1
        if (nmol.gt.maxmol) then
          maxmol = nmol + 10
          call changemaxmol
        endif
        nlow = nmol
      endif
!
!  Perform recursive search for all bonded atoms to reset molecule numbers
!
      ntodo = nbond
      do k = 1,nbond
        ntodolist(k) = nbat(k)
      enddo
      ndone = 1
      ndonelist(1) = i
      ldone(i) = .true.
      do while (ntodo.gt.0)
!
!  Set this atom as having been done
!
        do k = 1,ntodo
          j = ntodolist(k)
          if (.not.ldone(j)) then
            ndone = ndone + 1
            ldone(j) = .true.
            ndonelist(ndone) = j
          endif
        enddo
!
!  Loop over atoms to be done setting natmol and adding their neighbours to todolist
!
        ntodonew = 0
        do k = 1,ntodo
          j = ntodolist(k)
          natmol(j) = nlow
          nmolind(j) = 555
          do jj = 1,nbonds(j)
            l = nbonded(jj,j)
            if (.not.ldone(l)) then
!
!  Check previous entries to prevent duplication
!
              lduplicate = .false.
              do m = 1,ntodonew
                if (ntodolistnew(m).eq.l) lduplicate = .true.
              enddo
              if (.not.lduplicate) then
                ntodonew = ntodonew + 1
                ntodolistnew(ntodonew) = l
              endif
            endif
          enddo
        enddo
!
!  Move new todolist to be current one
!
        ntodo = ntodonew
        ntodolist(1:ntodo) = ntodolistnew(1:ntodo)
      enddo
!
!  Reset done list
!
      do k = 1,ndone
        ldone(ndonelist(k)) = .false.
      enddo
    endif
  enddo
!************************************
!  Sift out non-existent molecules  *
!************************************
  do i = 1,nmol
    ninc = 0
    do j = 1,numat
      if (natmol(j).eq.i) then
        ninc = ninc + 1
      endif
    enddo
    if (ninc.gt.0) then
      nexist(i) = 1
    else
      nexist(i) = 0
    endif
  enddo
  nmolfin = 0
  do i = 1,nmol
    if (nexist(i).gt.0) then
      nmolfin = nmolfin + 1
      nexist(i) = nmolfin
    endif
  enddo
  do i = 1,numat
    na = natmol(i)
    if (na.gt.0) natmol(i) = nexist(na)
  enddo
  nmol = nmolfin
!******************************************
!  Create pointers to atoms in molecules  *
!******************************************
  ninc = 0
  do i = 1,nmol
    nmolptr(i) = ninc
    nmolatom(i) = 0
    do j = 1,numat
      if (natmol(j).eq.i) then
        ninc = ninc + 1
        nmolatom(i) = nmolatom(i) + 1
        nmollist(ninc) = j
      endif
    enddo
  enddo
!**************************************
!  Obtain cell indices for each atom  *
!**************************************
  if (ndim.ne.0) call molind
!
!  Check that coulomb subtraction is not present with periodic
!  molecules as this would cause an error.
!
  if (index(keyword,'mole').ne.0) then
    do i = 1,nmol
      if (moldim(i).gt.0) then
        call outerror('Coulomb subtraction within periodic molecules is not allowed',0_i4)
        call stopnow('setmols')
      endif
    enddo
  endif
!
!  Check that the dimensionality of the molecule doesn't
!  exceed that of the system which would clearly be an error!
!
  do i = 1,nmol
    if (moldim(i).gt.ndim) then
      call outerror('dimensionality of molecule is too high',0_i4)
      call stopnow('setmols')
    endif
  enddo
!
!  Free local memory
!
  deallocate(nmlist,stat=status)
  if (status/=0) call deallocate_error('setmols','nmlist')
  deallocate(nbat,stat=status)
  if (status/=0) call deallocate_error('setmols','nbat')
!
  deallocate(ntodolistnew,stat=status)
  if (status/=0) call deallocate_error('setmols','ntodolistnew')
  deallocate(ntodolist,stat=status)
  if (status/=0) call deallocate_error('setmols','ntodolist')
  deallocate(ndonelist,stat=status)
  if (status/=0) call deallocate_error('setmols','ndonelist')
  deallocate(ldone,stat=status)
  if (status/=0) call deallocate_error('setmols','ldone')
  deallocate(nexist,stat=status)
  if (status/=0) call deallocate_error('setmols','nexist')
!
  return
  end
