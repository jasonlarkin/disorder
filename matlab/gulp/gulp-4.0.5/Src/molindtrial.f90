  subroutine molindtrial(ntrialatom,nptrtrialatom,ltrialatom)
!
!  Determines cell indices for molecules that contain atoms from a trial set
!
!   1/08 Created from molind
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
!  Julian Gale, NRI, Curtin University, January 2008
!
  use control,   only : lmodco
  use current
  use element
  use iochannels
  use molecule
  use parallel
  use reallocate
  use spatial,   only : lspatialok, xinbox, yinbox, zinbox
  use species
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: ntrialatom
  integer(i4), intent(in)                      :: nptrtrialatom(ntrialatom)
  logical,     intent(in)                      :: ltrialatom(numat)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ic
  integer(i4)                                  :: ierror
  integer(i4)                                  :: ii
  integer(i4)                                  :: iimin
  integer(i4)                                  :: iii
  integer(i4), dimension(:), pointer,     save :: iimptr
  integer(i4)                                  :: iio
  integer(i4)                                  :: im
  integer(i4)                                  :: ind
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjo
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kko
  integer(i4)                                  :: na
  integer(i4)                                  :: nact
  integer(i4)                                  :: nassign
  integer(i4)                                  :: nasslast
  integer(i4)                                  :: nbon
  integer(i4)                                  :: nbond
  integer(i4)                                  :: ni
  integer(i4)                                  :: ninc
  integer(i4)                                  :: nj
  integer(i4)                                  :: nj1
  integer(i4)                                  :: nk
  integer(i4)                                  :: nm
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmlind
  integer(i4)                                  :: nmlindo
  integer(i4)                                  :: nmoldo
  integer(i4)                                  :: nno
  integer(i4)                                  :: npn
  integer(i4)                                  :: nptr
  integer(i4)                                  :: npy
  integer(i4)                                  :: nt
  integer(i4), dimension(:), allocatable       :: nmlist
  integer(i4), dimension(:), allocatable       :: nmoldolist
  integer(i4), dimension(:), allocatable       :: npno
  integer(i4), dimension(:), allocatable       :: npyes
  integer(i4)                                  :: nyes
  integer(i4)                                  :: status
  logical                                      :: lassigned
  logical                                      :: lfound
  logical                                      :: lxcross
  logical                                      :: lycross
  logical                                      :: lzcross
  logical,                                save :: firstcall = .true.
  logical,     dimension(:), allocatable       :: lAlreadyUsed
  real(dp)                                     :: r2
  real(dp)                                     :: r2min
  real(dp)                                     :: rcut
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rj
  real(dp)                                     :: rjk
  real(dp)                                     :: rk
  real(dp)                                     :: rmin
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
!  Nullify pointers on first call
!
  if (firstcall) then
    nullify(iimptr)
    firstcall = .false.
  endif
!
!  Allocate local memory
!
  call realloc(iimptr,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('molindtrial','iimptr')
  allocate(nmlist(numat),stat=status)
  if (status/=0) call outofmemory('molindtrial','nmlist')
  allocate(npno(numat),stat=status)
  if (status/=0) call outofmemory('molindtrial','npno')
  allocate(npyes(numat),stat=status)
  if (status/=0) call outofmemory('molindtrial','npyes')
  allocate(nmoldolist(nmol),stat=status)
  if (status/=0) call outofmemory('molindtrial','nmoldolist')
  if (nconnect.gt.0) then
    allocate(lAlreadyUsed(nconnect),stat=status)
    if (status/=0) call outofmemory('molindtrial','lAlreadyUsed')
  endif
!
!  Find molecules that we need to handle due to trial atoms
!
  nmoldo = 0
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    nmi = natmol(i)
!
!  See if this molecule is already in the list
!
    lfound = .false.
    nm = 0
    do while (.not.lfound.and.nm.lt.nmoldo)
      nm = nm + 1
      lfound = (nmi.eq.nmoldolist(nm))
    enddo
    if (.not.lfound) then
      nmoldo = nmoldo + 1
      nmoldolist(nmoldo) = nmi
    endif
  enddo
!
!  Handle periodic wrapping of connection indices
!
  if (nconnect.gt.0.and.lmodco) call connectwraptrial(ntrialatom,nptrtrialatom,ltrialatom)
!
!  Note - higher tolerance can be used here than in initial search
!  for bonds, as the atoms in the molecule have been identified
!  already and this allows for greater distortions during energy
!  minimisation.
!
!***********************************************
!  Obtain cell indices for each relevant atom  *
!***********************************************
  do im = 1,nmoldo
    i = nmoldolist(im)
!
!  Store pointer to molecule atoms
!
    ninc = 0
    do j = 1,numat
      if (natmol(j).eq.i) then
        ninc = ninc + 1
        nmlist(ninc) = j
        nmolind(j) = 0
      endif
    enddo
!
!  lxcross etc are logicals indicating whether a boundary
!  crossing has occur so far for this molecule.
!
    lxcross = .false.
    lycross = .false.
    lzcross = .false.
    if (ninc.gt.0) then
!
!  Check whether connections have been specified across
!  cell boundaries between the same atom - this implies
!  periodicity.
!
      if (nconnect.gt.0) then
        do ic = 1,nconnect
          if (nconnectcfg(ic).eq.ncf) then
            do j = 1,ninc
              if (n1connect(ic).eq.j) then
                if (n2connect(ic).eq.j) then
                  call mindtoijk(nconnectind(ic),ii,jj,kk)
                  if (ii.ne.0) lxcross = .true.
                  if (jj.ne.0) lycross = .true.
                  if (kk.ne.0) lzcross = .true.
                endif
              endif
            enddo
          endif
        enddo
      endif
!
!  Locate most central atom of molecule
!
      do j = 1,ninc
        na = nmlist(j)
        if (ndim.eq.3) then
          rij = (xfrac(na)-0.5)**2 + (yfrac(na)-0.5)**2 + (zfrac(na)-0.5)**2
        elseif (ndim.eq.2) then
          rij = (xfrac(na)-0.5)**2 + (yfrac(na)-0.5)**2
        elseif (ndim.eq.1) then
          rij = (xfrac(na)-0.5)**2
        endif
        if (j.eq.1) then
          nptr = 1
          rmin = rij
        elseif (rij.lt.rmin) then
          nptr = j
          rmin = rij
        endif
      enddo
!
!  Recursive search to locate cell index for each atom of molecule
!
      nact = nmlist(nptr)
      nmolind(nact) = 555
      nyes = 1
      nassign = 1
      npyes(1) = nptr
      nno = 0
      do j = 1,ninc
        if (j.ne.nptr) then
          nno = nno + 1
          npno(nno) = j
        endif
      enddo
 10   nasslast = nassign
      do j = 1,nno
        npn = nmlist(npno(j))
        if (lspatialok) then
          xal = xinbox(npn)
          yal = yinbox(npn)
          zal = zinbox(npn)
        else
          xal = xclat(npn)
          yal = yclat(npn)
          zal = zclat(npn)
        endif
        nj = nat(npn)
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
        lassigned = .false.
        nmlindo = 0
        do k = 1,nyes
          npy = nmlist(npyes(k))
          nk = nat(npy)
          if (nk.gt.maxele) nk = nk - maxele
          rk = rcov(nk)
          rcut = rtol*(rj+rk)
          rcut = rcut*rcut
          ind = nmolind(npy)
          iz = (ind/100) - 5
          ind = ind - 100*(iz+5)
          iy = (ind/10) - 5
          ind = ind - 10*(iy+5)
          ix = ind - 5
!
!  Check for input-driven connectivity first
!
          if (nconnect.gt.0) then
            do ic = 1,nconnect
              if (nconnectcfg(ic) .eq. ncf) then
                if (n1connect(ic).eq.npy.and.n2connect(ic).eq.npn) then
                  if (nconnectind(ic).gt.0) then
                    call mindtoijk(nconnectind(ic),ii,jj,kk)
                  else
!
!  Find nearest image
!
                    if (lspatialok) then
                      xcdi = xal - xinbox(npy)
                      ycdi = yal - yinbox(npy)
                      zcdi = zal - zinbox(npy)
                    else
                      xcdi = xal - xclat(npy)
                      ycdi = yal - yclat(npy)
                      zcdi = zal - zclat(npy)
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
                  endif
                  nmlind = (ii+ix+5)+10*(jj+iy+5)+100*(kk+iz+5)
                  if (lassigned.and.nmlind.ne.nmlindo) then
                    if (iio.ne.(ii+ix)) lxcross = .true.
                    if (jjo.ne.(jj+iy)) lycross = .true.
                    if (kko.ne.(kk+iz)) lzcross = .true.
                  elseif (.not.lassigned) then
                    nmolind(npn) = nmlind
                    nassign = nassign + 1
                    lassigned = .true.
                    nmlindo = nmlind
                    iio = ii + ix
                    jjo = jj + iy
                    kko = kk + iz
                  endif
                elseif (n1connect(ic).eq.npn.and.n2connect(ic).eq.npy) then
                  if (nconnectind(ic).gt.0) then
                    call mindtoijk(1110_i4-nconnectind(ic),ii,jj,kk)
                  else
!
!  Find nearest image
!
                    if (lspatialok) then
                      xcdi = xal - xinbox(npy)
                      ycdi = yal - yinbox(npy)
                      zcdi = zal - zinbox(npy)
                    else
                      xcdi = xal - xclat(npy)
                      ycdi = yal - yclat(npy)
                      zcdi = zal - zclat(npy)
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
                  endif
                  nmlind = (ii+ix+5) + 10*(jj+iy+5) + 100*(kk+iz+5)
                  if (lassigned.and.nmlind.ne.nmlindo) then
                    if (iio.ne.(ii+ix)) lxcross = .true.
                    if (jjo.ne.(jj+iy)) lycross = .true.
                    if (kko.ne.(kk+iz)) lzcross = .true.
                  elseif (.not.lassigned) then
                    nmolind(npn) = nmlind
                    nassign = nassign + 1
                    lassigned = .true.
                    nmlindo = nmlind
                    iio = ii + ix
                    jjo = jj + iy
                    kko = kk + iz
                  endif
                endif
              endif
            enddo
          endif
          if (.not.lnoautobond) then
!
!  Now use interatomic distance checks
!
            if (lspatialok) then
              xcdi = xal - xinbox(npy)
              ycdi = yal - yinbox(npy)
              zcdi = zal - zinbox(npy)
            else
              xcdi = xal - xclat(npy)
              ycdi = yal - yclat(npy)
              zcdi = zal - zclat(npy)
            endif
            do iii = 1,iimax
              xcrd = xcdi + xvec1cell(iii)
              ycrd = ycdi + yvec1cell(iii)
              zcrd = zcdi + zvec1cell(iii)
              rjk = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
              if (rjk.lt.rcut.and.rjk.gt.0.0_dp) then
                call lintoijk(ii,jj,kk,iii,imaxl,jmaxl,kmaxl)
                nmlind = (ii+ix+5) + 10*(jj+iy+5) + 100*(kk+iz+5)
                if (lassigned.and.nmlind.ne.nmlindo) then
                  if (iio.ne.(ii+ix)) lxcross = .true.
                  if (jjo.ne.(jj+iy)) lycross = .true.
                  if (kko.ne.(kk+iz)) lzcross = .true.
                elseif (.not.lassigned) then
                  nmolind(npn) = nmlind
                  nassign = nassign + 1
                  lassigned = .true.
                  nmlindo = nmlind
                  iio = ii + ix
                  jjo = jj + iy
                  kko = kk + iz
                endif
                if (lassigned.and.lxcross.and.lycross.and.lzcross) goto 20
              endif
            enddo
          endif
        enddo
 20     continue
!
!  End loop over unindexed sites
!
      enddo
      if (ninc.eq.1.and..not.lnoautobond) then
!
!  Check to see whether unassigned atoms are bonded to periodic
!  replications of themselves
!
        npn = nmlist(1)
        if (lspatialok) then
          xal = xinbox(npn)
          yal = yinbox(npn)
          zal = zinbox(npn)
        else
          xal = xclat(npn)
          yal = yclat(npn)
          zal = zclat(npn)
        endif
        nj = nat(npn)
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
        lassigned = .false.
        rcut = 2.0*rtol*rj
        rcut = rcut*rcut
        xcdi = 0.0_dp
        ycdi = 0.0_dp
        zcdi = 0.0_dp
        do iii = 1,iimax
          xcrd = xcdi + xvec1cell(iii)
          ycrd = ycdi + yvec1cell(iii)
          zcrd = zcdi + zvec1cell(iii)
          rjk = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
          if (rjk.lt.rcut.and.rjk.gt.0.0_dp.and.iii.ne.iimid) then
            call lintoijk(ii,jj,kk,iii,imaxl,jmaxl,kmaxl)
            if (ii.ne.0) lxcross = .true.
            if (jj.ne.0) lycross = .true.
            if (kk.ne.0) lzcross = .true.
            if (lxcross.and.lycross.and.lzcross) goto 30
          endif
        enddo
 30     continue
!
!  Check to see that more assignments have been made - if not
!  this means that the a bond has increased beyond the tolerance.
!
      else
!
!  Set up
!
        nno = 0
        nyes = 0
        do j = 1,ninc
          if (nmolind(nmlist(j)).eq.0) then
            nno = nno + 1
            npno(nno) = j
          else
            nyes = nyes + 1
            npyes(nyes) = j
          endif
        enddo
        if (nassign.eq.nasslast) then
          call outerror('Bond length in molecule has exceeded tolerance',0_i4)
          if (ioproc) then
            write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
            write(ioout,'(''!!  Configuration number = '',i4)') ncf
            write(ioout,'(''!!  Molecule      number = '',i4)') i
            write(ioout,'(''!!  Atoms in molecule with missing bonds:'')')
            write(ioout,'(''!!  '',10(2x,i4))')(nmlist(npno(j)),j=1,nno)
            write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
            call outed
          endif
          call stopnow('molind')
        endif
      endif
!
!  Latest round of assignments made
!  If there are any unassigned sites repeat loop
!
      if (nassign.ne.ninc.and.nassign.ne.nasslast) goto 10
    endif
!
!  Assign dimensionality
!
    if (lxcross) then
      if (lycross) then
        if (lzcross) then
          moldim(i) = 3
        else
          moldim(i) = 2
          moldimi(i) = 1
        endif
      elseif (lzcross) then
        moldim(i) = 2
        moldimi(i) = 2
      else
        moldim(i) = 1
        moldimi(i) = 1
      endif
    elseif (lycross) then
      if (lzcross) then
        moldim(i) = 2
        moldimi(i) = 3
      else
        moldim(i) = 1
        moldimi(i) = 2
      endif
    elseif (lzcross) then
      moldim(i) = 1
      moldimi(i) = 3
    endif
!
!  End of loop over molecules
!
  enddo
!******************************
!  Find cell index for bonds  *
!******************************
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    if (lspatialok) then
      xal = xinbox(i)
      yal = yinbox(i)
      zal = zinbox(i)
    else
      xal = xclat(i)
      yal = yclat(i)
      zal = zclat(i)
    endif
    ni = nat(i)
    if (ni.gt.maxele) ni = ni - maxele
    ri = rcov(ni)
    nbond = 1
    if (nconnect.gt.0) then
      j = nbonded(nbond,i)
      lAlreadyUsed(1:nconnect) = .false.
      do while (j.ne.0.and.nbond.le.nbonds(i))
        lfound = .false.
        ic = 0
        do while (ic.lt.nconnect.and..not.lfound)
          ic = ic + 1
          if (nconnectcfg(ic).eq.ncf.and..not.lAlreadyUsed(ic)) then
            if (n1connect(ic).eq.i.and.n2connect(ic).eq.j) then
              lfound = .true.
              lAlreadyUsed(ic) = .true.
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
                nbondind(nbond,i) = (ii+5) + 10*(jj+5) + 100*(kk+5)
              endif
              iimptr(nbond) = 0
            elseif (n2connect(ic).eq.i.and.n1connect(ic).eq.j) then
              lfound = .true.
              lAlreadyUsed(ic) = .true.
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
                nbondind(nbond,i) = (ii+5)+10*(jj+5)+100*(kk+5)
              endif
              iimptr(nbond) = 0
            endif
          endif
        enddo
        nbond = nbond + 1
        if (nbond.gt.maxbond) then
          maxbond = nbond + 2
          call changemaxbond
          call realloc(iimptr,maxbond,ierror)
          if (ierror.ne.0) call outofmemory('molind','iimptr')
        endif
        j = nbonded(nbond,i)
      enddo
    endif
    if (.not.lnoautobond.and.ri.ne.0.0_dp) then
!
!  Loop over bonds
!
      j = nbonded(nbond,i)
      do while (j.gt.0.and.nbond.le.nbonds(i))
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
        nj = nj1
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
        rcut = rtol*(ri+rj)
        rcut = rcut*rcut
        if (rj.ne.0.0_dp) then
          xcdi = xcd - xal
          ycdi = ycd - yal
          zcdi = zcd - zal
!
!  Set starting point in cell vector loop to avoid same index
!  as previous occurance of the same atom
!
          iimin = 1
          lfound = .false.
          nbon = nbond - 1
          do while (.not.lfound.and.nbon.gt.0) 
            if (nbonded(nbon,i).eq.j) then
              iimin = iimptr(nbon) + 1
              lfound = .true.
            endif
            nbon = nbon - 1
          enddo
          lfound = .false.
          ii = iimin
          do while (ii.le.iimax.and..not.lfound)
            xcrd = xcdi + xvec1cell(ii)
            ycrd = ycdi + yvec1cell(ii)
            zcrd = zcdi + zvec1cell(ii)
            rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (rij.le.rcut.and.rij.gt.0.0_dp.and.(i.ne.j.or.ii.ne.iimid)) then
!
!  Valid bond
!
              lfound = .true.
              iimptr(nbond) = ii
              if (ii.eq.iimid) then
                nbondind(nbond,i) = 555
              else
                call lintoijk(ixx,iyy,izz,ii,imaxl,jmaxl,kmaxl)
                ixx = ixx + 5
                iyy = iyy + 5
                izz = izz + 5
                nbondind(nbond,i) = ixx + 10*iyy + 100*izz
              endif
            endif
            ii = ii + 1
          enddo
        endif
        nbond = nbond + 1
        j = nbonded(nbond,i)
      enddo
    endif
  enddo
!
!  Free local memory
!
  if (nconnect.gt.0) then
    deallocate(lAlreadyUsed,stat=status)
    if (status/=0) call deallocate_error('molindtrial','lAlreadyUsed')
  endif
  deallocate(nmoldolist,stat=status)
  if (status/=0) call deallocate_error('molindtrial','nmoldolist')
  deallocate(npyes,stat=status)
  if (status/=0) call deallocate_error('molindtrial','npyes')
  deallocate(npno,stat=status)
  if (status/=0) call deallocate_error('molindtrial','npno')
  deallocate(nmlist,stat=status)
  if (status/=0) call deallocate_error('molindtrial','nmlist')
  call realloc(iimptr,0_i4,ierror)
!
  return
  end
