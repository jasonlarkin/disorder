  subroutine molindfixtrial(ntrialatom,nptrtrialatom,ltrialatom)
!
!  Determines cell indices for molecules from a trial subset
!
!   1/08 Created from molindfix
!   8/12 Check added for whether a trial atom has a molecule associated with it.
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
!  Julian Gale, NRI, Curtin University, August 2012
!
  use control,   only : lmodco
  use current
  use element
  use molecule
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
  integer(i4)                                  :: im
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
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
  integer(i4)                                  :: nm
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmlind
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
  logical                                      :: lassign
  logical                                      :: lbonded
  logical                                      :: lfound
  logical,     dimension(:), allocatable       :: lAlreadyUsed
  real(dp)                                     :: r2
  real(dp)                                     :: r2min
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rj
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
!  Allocate local memory
!
  call realloc(iimptr,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('molindfixtrial','iimptr')
  allocate(nmlist(numat),stat=status)
  if (status/=0) call outofmemory('molindfixtrial','nmlist')
  allocate(npno(numat),stat=status)
  if (status/=0) call outofmemory('molindfixtrial','npno')
  allocate(npyes(numat),stat=status)
  if (status/=0) call outofmemory('molindfixtrial','npyes')
  allocate(nmoldolist(nmol),stat=status)
  if (status/=0) call outofmemory('molindfixtrial','nmoldolist')
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
!  Check whether this trial atom has a molecule associated with it
!
    if (nmi.gt.0) then
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
    endif
  enddo
!
!  Handle periodic wrapping of connection indices
!
  if (nconnect.gt.0.and.lmodco) call connectwraptrial(ntrialatom,nptrtrialatom,ltrialatom)
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
    j = nbonded(1,i)
    if (ri.ne.0.0_dp.or.nconnect.ne.0) then
!
!  Loop over bonds
!
      do while (j.gt.0.and.nbond.le.nbonds(i))
        lfound = .false.
        if (nconnect.gt.0) then
          lAlreadyUsed(1:nconnect) = .false.
          ic = 0
          do while (ic.lt.nconnect.and..not.lfound)
            ic = ic + 1
            if (nconnectcfg(ic).eq.ncf.and..not.lAlreadyUsed(ic)) then
              if (n1connect(ic).eq.i.and.n2connect(ic).eq.j) then
                nbonded(nbond,i) = n2connect(ic)
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
                lfound = .true.
                lAlreadyUsed(ic) = .true.
                iimptr(nbond) = 0
              elseif (n2connect(ic).eq.i.and.n1connect(ic).eq.j) then
                nbonded(nbond,i) = n1connect(ic)
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
                lfound = .true.
                lAlreadyUsed(ic) = .true.
              endif
            endif
          enddo
        endif
        if (.not.lfound) then
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
          if (rj.ne.0.0_dp) then
            xcdi = xcd - xal
            ycdi = ycd - yal
            zcdi = zcd - zal
!
!  Find shortest distance to bonded atom image
!  excluding those already located
!
            rmin = 1.0d10
            do ii = 1,iimax
              xcrd = xcdi + xvec1cell(ii)
              ycrd = ycdi + yvec1cell(ii)
              zcrd = zcdi + zvec1cell(ii)
              rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
              if (rij.le.rmin.and.rij.gt.0.0_dp.and.(i.ne.j.or.ii.ne.iimid)) then
!
!  Check to see whether this image has already been found
!
                lfound = .false.
                nbon = nbond - 1
                do while (.not.lfound.and.nbon.gt.0) 
                  if (nbonded(nbon,i).eq.j.and.iimptr(nbon).eq.ii) then
                    lfound = .true.
                  endif
                  nbon = nbon - 1
                enddo
!
!  Valid bond
!
                if (.not.lfound) then
                  iimptr(nbond) = ii
                  rmin = rij
                endif
              endif
            enddo
            ii = iimptr(nbond)
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
        endif
        nbond = nbond + 1
        if (nbond.gt.maxbond) then
          maxbond = nbond + 2
          call changemaxbond
          call realloc(iimptr,maxbond,ierror)
          if (ierror.ne.0) call outofmemory('molindfix','iimptr')
        endif
        j = nbonded(nbond,i)
      enddo
    endif
  enddo
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
    if (ninc.gt.0) then
!
!  Locate most central atom of molecule
!
      do j = 1,ninc
        na = nmlist(j)
        if (ndim.eq.3) then
          rij = (xfrac(na) - 0.5)**2 + (yfrac(na)-0.5)**2 + (zfrac(na)-0.5)**2
        elseif (ndim.eq.2) then
          rij = (xfrac(na) - 0.5)**2 + (yfrac(na)-0.5)**2
        elseif (ndim.eq.1) then
          rij = (xfrac(na) - 0.5)**2
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
10    nasslast = nassign
      do j = 1,nno
        npn = nmlist(npno(j))
        lassign = .false.
        k = 0
        do while (k.lt.nyes.and..not.lassign)
          k = k + 1
          npy = nmlist(npyes(k))
!
!  Are atoms bonded?
!
          lbonded = .false.
          nbond = 1
          do while (nbonded(nbond,npn).gt.0..and.nbond.le.nbonds(npn).and..not.lbonded)
            if (nbonded(nbond,npn).eq.npy) then
              lbonded = .true.
            endif
            nbond = nbond + 1
          enddo
          if (lbonded) then
            nbond = nbond - 1
!
!  If bonded find index number
!
            nmlind = nmolind(npy) - nbondind(nbond,npn) + 555
            nmolind(npn) = nmlind
            nassign = nassign + 1
            lassign = .true.
          endif
        enddo
!
!  End loop over unindexed sites
!
      enddo
!
!  Latest round of assignments made
!  If there are any unassigned sites repeat loop
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
      if (nassign.ne.ninc.and.nassign.ne.nasslast) goto 10
    endif
!
!  End of loop over molecules
!
  enddo
!
!  Free local memory
!
  if (nconnect.gt.0) then
    deallocate(lAlreadyUsed,stat=status)
    if (status/=0) call deallocate_error('molindtrial','lAlreadyUsed')
  endif
  deallocate(nmoldolist,stat=status)
  if (status/=0) call deallocate_error('molindfixtrial','nmoldolist')
  deallocate(npyes,stat=status)
  if (status/=0) call deallocate_error('molindfixtrial','npyes')
  deallocate(npno,stat=status)
  if (status/=0) call deallocate_error('molindfixtrial','npno')
  deallocate(nmlist,stat=status)
  if (status/=0) call deallocate_error('molindfixtrial','nmlist')
  call realloc(iimptr,0_i4,ierror)
!
  return
  end
