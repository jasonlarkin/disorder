  subroutine setmol
!
!  Determines number of molecules and their constituent atoms
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
!   3/95 Periodic molecules now allowed when not coulomb subtracted
!   4/01 Input of connectivity lists added
!  12/02 Changed from setmol to oldsetmol
!  12/02 Setting of nbondind altered to allow for 1/2-D case
!   9/04 Modified to use inbox coordinates if lspatialok is true
!   8/06 Pointers nullified on first call
!   1/07 Modified for noautobond option
!   2/07 Bond types added
!   2/07 Check to remove duplicate bonds added
!   4/07 Removal of duplicate bonds corrected
!   4/08 Use of nlow being present to a number replace by use of logical lfirstlow
!   6/09 New pointers to atoms in molecules calculated once nmol is finalised
!   2/10 For EVB calculation, add individual atoms as molecules
!   5/10 Timing added
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
!  Julian Gale, NRI, Curtin University, May 2010
!
  use control
  use current
  use element
  use molecule
  use parallel
  use reallocate
  use spatial,   only : lspatialok, xinbox, yinbox, zinbox
  use times,     only : tmol
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ic
  integer(i4)                                  :: ierror
  integer(i4)                                  :: ii
  integer(i4)                                  :: iii
  integer(i4)                                  :: iimin
  integer(i4)                                  :: ind
  integer(i4)                                  :: indb
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: l
  integer(i4)                                  :: na
  integer(i4)                                  :: nb1
  integer(i4)                                  :: nb2
  integer(i4), dimension(:), pointer,     save :: nbat
  integer(i4)                                  :: nbond
  integer(i4)                                  :: nbondcheck
  integer(i4), dimension(:), allocatable       :: nexist
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: ni1
  integer(i4)                                  :: nj1
  integer(i4)                                  :: ninc
  integer(i4)                                  :: nlow
  integer(i4)                                  :: nml
  integer(i4), dimension(:), pointer,     save :: nmlist
  integer(i4)                                  :: nmolfin
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntj
  integer(i4)                                  :: nti1
  integer(i4)                                  :: ntj1
  integer(i4)                                  :: status
  logical                                      :: lbondok
  logical                                      :: lduplicate
  logical                                      :: lfind
  logical                                      :: lfirstlow
  logical,                                save :: firstcall = .true.
  real(dp)                                     :: cputime
  real(dp)                                     :: r2
  real(dp)                                     :: r2min
  real(dp)                                     :: rcut
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rj
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: t3
  real(dp)                                     :: t4
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  t1 = cputime()
  lbondok = .true.
!
!  Nullify pointers on first call
!
  if (firstcall) then
    nullify(nbat)
    nullify(nmlist)
    firstcall = .false.
  endif
!
!  Allocate local memory
!
  call realloc(nbat,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('setmol','nbat')
  call realloc(nmlist,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('setmol','nmlist')
  allocate(nexist(numat),stat=status)
  if (status/=0) call outofmemory('setmol','nexist')
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
  nmol = 0
!******************************
! (1) Generate molecule lists *
!******************************
  do i = 1,numat
    if (lspatialok) then
      xal = xinbox(i)
      yal = yinbox(i)
      zal = zinbox(i)
    else
      xal = xclat(i)
      yal = yclat(i)
      zal = zclat(i)
    endif
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
      do ic = 1,nconnect
        if (nconnectcfg(ic) .eq. ncf) then
          if (n1connect(ic).eq.i) then
            nbond = nbond + 1
            nbonds(i) = nbonds(i) + 1
            if (nbond.gt.maxbond) then
              maxbond = nbond + 2
              call changemaxbond
              call realloc(nbat,maxbond,ierror)
              if (ierror.ne.0) call outofmemory('setmol','nbat')
              call realloc(nmlist,maxbond,ierror)
              if (ierror.ne.0) call outofmemory('setmol','nmlist')
            endif
            nbat(nbond) = n2connect(ic)
            j = n2connect(ic)
            nbonded(nbond,i) = j
            nbondedtype(1,nbond,i) = nconnecttype(1,ic)
            nbondedtype(2,nbond,i) = nconnecttype(2,ic)
            if (nconnectind(ic).gt.0) then
              nbondind(nbond,i) = nconnectind(ic)
            else
!
!  Find nearest image
!
              if (lspatialok) then
                xcdi = xinbox(j) - xal
                ycdi = yinbox(j) - yal
                zcdi = zinbox(j) - zal
              else
                xcdi = xclat(j) - xal
                ycdi = yclat(j) - yal
                zcdi = zclat(j) - zal
              endif
              r2min = 1000000.0_dp
              iimin = 0
              do iii = 1,iimax
                xcrd = xcdi + xvec1cell(iii)
                ycrd = ycdi + yvec1cell(iii)
                zcrd = zcdi + zvec1cell(iii)
                r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
                if (r2.lt.r2min) then
                  r2min = r2
                  iimin = iii
                endif
              enddo
              call lintoijk(ii,jj,kk,iimin,imaxl,jmaxl,kmaxl)
              nbondind(nbond,i) = (ii+5)+10*(jj+5)+100*(kk+5)
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
                call realloc(nbat,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmol','nbat')
                call realloc(nmlist,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmol','nmlist')
              endif
              nbat(nbond) = j
              nbonded(nbond,i) = j
              nbondedtype(1,nbond,i) = nconnecttype(1,ic)
              nbondedtype(2,nbond,i) = nconnecttype(2,ic)
              if (nconnectind(ic).gt.0) then
                nbondind(nbond,i) = 1110 - nconnectind(ic)
              endif
            endif
          elseif (n2connect(ic).eq.i) then
            nbond = nbond + 1
            nbonds(i) = nbonds(i) + 1
            if (nbond.gt.maxbond) then
              maxbond = nbond + 2
              call changemaxbond
              call realloc(nbat,maxbond,ierror)
              if (ierror.ne.0) call outofmemory('setmol','nbat')
              call realloc(nmlist,maxbond,ierror)
              if (ierror.ne.0) call outofmemory('setmol','nmlist')
            endif
            nbat(nbond) = n1connect(ic)
            j = n1connect(ic)
            nbonded(nbond,i) = j
            nbondedtype(1,nbond,i) = nconnecttype(1,ic)
            nbondedtype(2,nbond,i) = nconnecttype(2,ic)
            if (nconnectind(ic).gt.0) then
              nbondind(nbond,i) = 1110 - nconnectind(ic)
            else
!
!  Find nearest image
!
              if (lspatialok) then
                xcdi = xinbox(j) - xal
                ycdi = yinbox(j) - yal
                zcdi = zinbox(j) - zal
              else
                xcdi = xclat(j) - xal
                ycdi = yclat(j) - yal
                zcdi = zclat(j) - zal
              endif
              r2min = 1000000.0_dp
              iimin = 0
              do iii = 1,iimax
                xcrd = xcdi + xvec1cell(iii)
                ycrd = ycdi + yvec1cell(iii)
                zcrd = zcdi + zvec1cell(iii)
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
    if (.not.lnoautobond.and.ri.ne.0.0_dp) then
      do j = 1,numat
        if (lspatialok) then
          xcd = xinbox(j)
          ycd = yinbox(j)
          zcd = zinbox(j)
        else
          xcd = xclat(j)
          ycd = yclat(j)
          zcd = zclat(j)
        endif
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
          ii = 1
          do while (lbondok.and.(ii.le.nnobo))
            if (indb.eq.nobond(ii)) then
              nb1 = nobotyp(ii)/1000
              nb2 = nobotyp(ii) - 1000*nb1
              if ((nb1.eq.nti1.or.nb1.eq.0).and.(nb2.eq.ntj1.or.nb2.eq.0)) lbondok = .false.
              if (ni1.eq.nj1.and.lbondok) then
                if ((nb1.eq.ntj1.or.nb1.eq.0).and.(nb2.eq.nti1.or.nb2.eq.0)) lbondok = .false.
              endif
            endif
            ii = ii + 1
          enddo
        endif
!
!  Distance check
!
        if (rj.ne.0.0_dp.and.lbondok) then
          rcut = rtol*(ri+rj)
          rcut = rcut*rcut
          if (rj.ne.0.0_dp) then
            xcdi = xcd - xal
            ycdi = ycd - yal
            zcdi = zcd - zal
            do ii = 1,iimax
              xcrd = xcdi + xvec1cell(ii)
              ycrd = ycdi + yvec1cell(ii)
              zcrd = zcdi + zvec1cell(ii)
              rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
              if (rij.le.rcut.and.rij.gt.0.0_dp.and.(i.ne.j.or.ii.ne.iimid)) then
!
!  Valid bond
!
                nbond = nbond + 1
                nbonds(i) = nbonds(i) + 1
                if (nbond.gt.maxbond) then
                  maxbond = nbond + 2
                  call changemaxbond
                  call realloc(nbat,maxbond,ierror)
                  if (ierror.ne.0) call outofmemory('setmol','nbat')
                  call realloc(nmlist,maxbond,ierror)
                  if (ierror.ne.0) call outofmemory('setmol','nmlist')
                endif
                nbat(nbond) = j
                nbonded(nbond,i) = j
                nbondedtype(1,nbond,i) = 1
                nbondedtype(2,nbond,i) = 1
                if (ii.eq.iimid) then
                  nbondind(nbond,i) = 555
                else
                  ind = ii - 1
                  ixx = (ind/((2*jmaxl+1)*(2*kmaxl+1)))
                  ind = ind - ixx*(2*jmaxl+1)*(2*kmaxl+1)
                  iyy = (ind/(2*kmaxl+1))
                  ind = ind - iyy*(2*kmaxl+1)
                  izz = ind - kmaxl
                  iyy = iyy - jmaxl
                  ixx = ixx - imaxl
                  nbondind(nbond,i) = ixx + 5 + 10*(iyy + 5) + 100*(izz + 5)
                endif
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
            enddo
          endif
        endif
      enddo
    endif
    if (nbond.gt.0) then
!
!  Include reference atom i in list
!
      nbond = nbond + 1
      if (nbond.gt.maxbond) then
        maxbond = nbond + 2
        call changemaxbond
        call realloc(nbat,maxbond,ierror)
        if (ierror.ne.0) call outofmemory('setmol','nbat')
        call realloc(nmlist,maxbond,ierror)
        if (ierror.ne.0) call outofmemory('setmol','nmlist')
      endif
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
!  Do bonded atoms first to makesure they are located
!
      do k = 1,nbond
        na = nbat(k)
        natmol(na) = nlow
        nmolind(na) = 555
      enddo
!
!  Do other previously designated atoms which may be linked
!  to atoms altered in the current pass.
!
      do k = 1,numat
        lfind = .false.
        do l = 1,nml
          if (natmol(k).eq.nmlist(l)) lfind = .true.
        enddo
        if (lfind) then
          natmol(k) = nlow              
          nmolind(k) = 555
        endif
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
  if (ndim.ne.0) then
    t3 = cputime()
    call molind
    t4 = cputime()
    tmol = tmol - t4 + t3
  endif
!
!  Check that coulomb subtraction is not present with periodic
!  molecules as this would cause an error.
!
  if (index(keyword,'mole').ne.0) then
    do i = 1,nmol
      if (moldim(i).gt.0) then
        call outerror('Coulomb subtraction within periodic molecules is not allowed',0_i4)
        call stopnow('setmol')
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
      call stopnow('setmol')
    endif
  enddo
!
!  Free local memory
!
  deallocate(nexist,stat=status)
  if (status/=0) call deallocate_error('setmol','nexist')
  call realloc(nbat,0_i4,ierror)
  call realloc(nmlist,0_i4,ierror)
!
  t2 = cputime()
  tmol = tmol + t2 - t1
!
  return
  end
