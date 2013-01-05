  subroutine GULP_bond
!
!  Subroutine for calculating bondlengths
!
!   4/01 Modified to use the information in bonding arrays.
!        This was done to allow for the effects of the
!        nobond and connect options which were previously
!        ignored for this print out.
!   9/04 Modified to use inbox coordinates if lspatialok is true
!   7/05 Allocations/deallocations cleaned up
!   3/07 Name changed to GULP_bond for Chemshell 
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
!   5/07 Referencing of nbonds changed from i to ifull to be formally
!        correct
!  11/07 Unused variables cleaned up
!   9/10 Format of write statement for atom number changed
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
!  Julian Gale, NRI, Curtin University, September 2010
!
  use control
  use current
  use element
  use general
  use iochannels
  use molecule
  use spatial,   only : lspatialok, xinbox, yinbox, zinbox
  use species
  implicit none
!
!  Local variables
!
  character(len=5)                             :: labi
  character(len=5)                             :: labj
  character(len=5)                             :: stype(2)
  integer(i4)                                  :: i
  integer(i4)                                  :: ib
  integer(i4)                                  :: ifull
  integer(i4)                                  :: ind
  integer(i4)                                  :: isp
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: m
  integer(i4)                                  :: mspec
  integer(i4)                                  :: n
  integer(i4), dimension(:), allocatable       :: naver
  integer(i4)                                  :: nbondtot
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nline
  integer(i4)                                  :: nodd
  integer(i4)                                  :: nptr
  integer(i4)                                  :: nsptr1
  integer(i4)                                  :: nsptr2
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntj
  integer(i4)                                  :: nttype
  integer(i4)                                  :: ntttype
  integer(i4)                                  :: ntype
  integer(i4)                                  :: numt
  integer(i4), dimension(:), allocatable       :: numtype
  integer(i4)                                  :: status
  logical                                      :: lfout
  logical                                      :: lfound
  real(dp)                                     :: diff
  real(dp)                                     :: r
  real(dp),    dimension(:), allocatable       :: raver
  real(dp)                                     :: rmean
  real(dp)                                     :: rmin
  real(dp)                                     :: rtmp
  real(dp),    dimension(:), allocatable       :: rtype
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
  allocate(numtype(maxbond),stat=status)
  if (status/=0) call outofmemory('GULP_bond','numtype')
  allocate(rtype(maxbond),stat=status)
  if (status/=0) call outofmemory('GULP_bond','rtype')
!
  stype(1) = 'core '
  stype(2) = 'shell'
  if (laver) then
    mspec = nspec*(nspec+1)/2
    allocate(naver(mspec),stat=status)
    if (status/=0) call outofmemory('GULP_bond','naver')
    allocate(raver(mspec),stat=status)
    if (status/=0) call outofmemory('GULP_bond','raver')
    do i = 1,mspec
      naver(i) = 0
      raver(i) = 0.0_dp
    enddo
  endif
!
!  Output header
!
  write(ioout,'(/,''  Bond calculation :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Asymmetric unit site  Full lattice sites    No.  Distance      No.  Distance'')')
  write(ioout,'(''                                                    (Angs)             (Angs) '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Loop over sites
!
  ntttype = 0
  nbondtot = 0
  do i = 1,nasym
    if (lspatialok) then
      xcd = xinbox(i)
      ycd = yinbox(i)
      zcd = zinbox(i)
    else
      xcd = xalat(i)
      ycd = yalat(i)
      zcd = zalat(i)
    endif
    ni = iatn(i)
    nti = natype(i)
    ifull = nrel2(i)
!
!  Identify species type
!
    if (laver) then
      isp = 0
      lfound = .false.
      do while (.not.lfound.and.isp.lt.nspec)
        isp = isp + 1
        if (ni.eq.natspec(isp).and.nti.eq.ntypspec(isp)) lfound = .true.
      enddo
    endif
    if (ni.gt.maxele) then
      ni = ni - maxele
      nsptr1 = 2
    else
      nsptr1 = 1
    endif
    call label(ni,nti,labi)
    lfout = .true.
    nttype = 0
!
!  Loop over second site
!
    do j = 1,nspec
      ntype = 0
      nj = natspec(j)
      ntj = ntypspec(j)
      call label(nj,ntj,labj)
      if (nj.gt.maxele) then
        nsptr2 = 2
      else
        nsptr2 = 1
      endif
      if (laver) then
        if (j.gt.isp) then
          ind = j*(j-1)/2 + isp
        else
          ind = isp*(isp-1)/2 + j
        endif
      endif
!
!  Loop over bonds of i
!
      ib = 1
      k = nbonded(ib,ifull)
      do while (ib.le.nbonds(ifull).and.k.gt.0)
!
!  Is this the right species?
!
        if (nat(k).eq.nj.and.nftype(k).eq.ntj) then
!
!  Find bond length
!
          if (lspatialok) then
            xcdi = xinbox(k) - xcd
            ycdi = yinbox(k) - ycd
            zcdi = zinbox(k) - zcd
          else
            xcdi = xclat(k) - xcd
            ycdi = yclat(k) - ycd
            zcdi = zclat(k) - zcd
          endif
!
!  Generate correct image to work out distance
!
          call mindtoijk(nbondind(ib,ifull),ixx,iyy,izz)
          xcrd = xcdi + r1x*ixx + r2x*iyy + r3x*izz
          ycrd = ycdi + r1y*ixx + r2y*iyy + r3y*izz
          zcrd = zcdi + r1z*ixx + r2z*iyy + r3z*izz
          r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
          r = sqrt(r)
!
!  Check to see if distance is equivalent to any that have
!  already been characterised
!
          if (ntype.eq.0) then
            ntype = ntype + 1
            numtype(ntype) = 1
            rtype(ntype) = r
          else
            do n = 1,ntype
              diff = abs(r-rtype(n))
              if (diff.lt.1d-4)then
                numtype(n) = numtype(n) + 1
                goto 20
              endif
            enddo
            ntype = ntype + 1
            numtype(ntype) = 1
            rtype(ntype) = r
20           continue
          endif
        endif
        ib = ib + 1
        k = nbonded(ib,ifull)
      enddo
      if (laver) then
        do k = 1,ntype
          raver(ind) = raver(ind) + dble(numtype(k))*rtype(k)
          naver(ind) = naver(ind) + numtype(k)
        enddo
      endif
!
!  Add to total number of bonds
!
      do k = 1,ntype
        nbondtot = nbondtot + numtype(k)
      enddo
!
!  Shuffle distances into ascending order
!
      do k = 1,ntype
        rmin = 10000.0_dp
        nptr = 1
        do m = k,ntype
          if (rtype(m).lt.rmin) then
            rmin = rtype(m)
            nptr = m
          endif
        enddo
        if (nptr.ne.k) then
          rtmp = rtype(k)
          rtype(k) = rtype(nptr)
          rtype(nptr) = rtmp
          numt = numtype(k)
          numtype(k) = numtype(nptr)
          numtype(nptr) = numt
        endif
      enddo
      nttype = nttype + ntype
!
!  Output details
!
      if (ntype.gt.0) then
        if (lfout) then
          lfout = .false.
          nline = (ntype-1)/2 + 1
          nodd = ntype - 2*(nline-1)
          if (nline.gt.0) then
            if (ntype.ge.2) then
              write(ioout,'(1x,i6,4x,a5,1x,a5,7x,a5,1x,a5,1x,2(i7,2x,f8.4,2x))') &
                i,labi,stype(nsptr1),labj,stype(nsptr2),numtype(1),rtype(1),numtype(2),rtype(2)
            else
              write(ioout,'(1x,i6,4x,a5,1x,a5,7x,a5,1x,a5,1x,i7,2x,f8.4)') &
                i,labi,stype(nsptr1),labj,stype(nsptr2),numtype(1),rtype(1)
            endif
            if (nline.gt.1) then
              if (nodd.eq.1) then
                ind = 1
                do m = 2,nline-1
                  ind = ind + 2
                  write(ioout,'(41x,2(i7,2x,f8.4,2x))') numtype(ind),rtype(ind),numtype(ind+1),rtype(ind+1)
                enddo
                ind = ind + 2
                write(ioout,'(41x,i7,2x,f8.4)')numtype(ind),rtype(ind)
              else
                ind = 1
                do m = 2,nline
                  ind = ind + 2
                  write(ioout,'(41x,2(i7,2x,f8.4,2x))')numtype(ind),rtype(ind),numtype(ind+1),rtype(ind+1)
                enddo
              endif
            endif
          endif
        else
          nline = (ntype - 1)/2 + 1
          nodd = ntype - 2*(nline - 1)
          if (nline.gt.0) then
            if (ntype.ge.2) then
              write(ioout,'(29x,a5,1x,a5,1x,2(i7,2x,f8.4,2x))') &
                labj,stype(nsptr2),numtype(1),rtype(1),numtype(2),rtype(2)
            else
              write(ioout,'(29x,a5,1x,a5,1x,i7,2x,f8.4)') labj,stype(nsptr2),numtype(1),rtype(1)
            endif
            if (nline.gt.1) then
              if (nodd.eq.1) then
                ind = 1
                do m = 2,nline-1
                  ind = ind + 2
                  write(ioout,'(41x,2(i7,2x,f8.4,2x))') numtype(ind),rtype(ind),numtype(ind+1),rtype(ind+1)
                enddo
                ind=ind+2
                write(ioout,'(41x,i7,2x,f8.4)') numtype(ind),rtype(ind)
              else
                ind = 1
                do m = 2,nline
                  ind = ind + 2
                  write(ioout,'(41x,2(i7,2x,f8.4,2x))') numtype(ind),rtype(ind),numtype(ind+1),rtype(ind+1)
                enddo
              endif
            endif
          endif
        endif
      endif
!
!  Zero arrays for next species type
!
      do m = 1,ntype
        numtype(m) = 0
        rtype(m) = 0.0_dp
      enddo
!
!  End loop over types of species
!
    enddo
    if (nttype.gt.0) write(ioout,'(''--------------------------------------------------------------------------------'')')
    ntttype = ntttype + nttype
!
!  End of asymmetric atom loop
!
  enddo
  if (ntttype.eq.0) then
    write(ioout,'(''  No bonds found for any atoms in the system'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  else
    write(ioout,'(''  Total number of bonds = '',i8)') nbondtot/2
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  write(ioout,'(/)')
!**********************************
!  Average distances if required  *
!**********************************
  if (laver) then
    write(ioout,'(''  Average bond lengths : '',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''      Species 1    Species 2    Mean bond length (Angstroms)'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    ind = 0
    do i = 1,nspec
      ni = natspec(i)
      nti = ntypspec(i)
      call label(ni,nti,labi)
      if (ni.gt.maxele) then
        nsptr1 = 2
      else
        nsptr1 = 1
      endif
      do j = 1,i
        ind = ind + 1
        if (naver(ind).gt.0) then
          nj = natspec(j)
          ntj = ntypspec(j)
          call label(nj,ntj,labj)
          if (nj.gt.maxele) then
            nsptr2 = 2
          else
            nsptr2 = 1
          endif
          rmean=raver(ind)/dble(naver(ind))
          write(ioout,'(6x,a5,1x,a5,2x,a5,1x,a5,4x,f14.6)')labi,stype(nsptr1),labj,stype(nsptr2),rmean
        endif
      enddo
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
    deallocate(raver,stat=status)
    if (status/=0) call deallocate_error('GULP_bond','raver')
    deallocate(naver,stat=status)
    if (status/=0) call deallocate_error('GULP_bond','naver')
  endif
!
  deallocate(rtype,stat=status)
  if (status/=0) call deallocate_error('GULP_bond','rtype')
  deallocate(numtype,stat=status)
  if (status/=0) call deallocate_error('GULP_bond','numtype')
!
  return
  end
