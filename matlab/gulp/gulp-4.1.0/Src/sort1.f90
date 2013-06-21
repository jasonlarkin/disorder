  subroutine sort1(lnosort,lrestore)
!
!  Sort atoms within region 1
!
!  NB: idopt on entry to sort1 must contain a series of
!      integer flags for each of the 3*nreg1 atoms.
!
!   3/13 Created from setdef
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use control
  use current
  use defects
  use element, only : maxele
  use energies
  use general
  use molecule
  use parallel
  use reallocate
  use shell
  use species
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,                   intent(in)        :: lnosort
  logical,                   intent(in)        :: lrestore
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4), dimension(:), allocatable       :: itmp2
  integer(i4)                                  :: j
  integer(i4)                                  :: jptr
  integer(i4)                                  :: k
  integer(i4)                                  :: nco
  integer(i4)                                  :: ndp
  integer(i4)                                  :: noff
  integer(i4)                                  :: nqmc
  integer(i4)                                  :: nqms
  integer(i4)                                  :: nqmcr1
  integer(i4)                                  :: nqmsr1
  integer(i4)                                  :: ns
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: ltmp
  logical                                      :: lfound
  real(dp)                                     :: r2max
  real(dp),    dimension(:), allocatable       :: tmp
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
!
!  Allocate memory for local region 1 related variables 
!
  allocate(itmp(3*nreg1),stat=status)
  if (status/=0) call outofmemory('setdef','itmp')
!********************
!  Sort by QM type  *
!********************
  nqmcr1 = 0
  nqmsr1 = 0
  do i = 1,nreg1
    if (ldqmatom(i)) then
      if (natdefe(i).le.maxele) then
        nqmcr1 = nqmcr1 + 1
      else
        nqmsr1 = nqmsr1 + 1
      endif
    endif
  enddo
  nqmc = 0
  nqms = nqmcr1
  do i = 1,nreg1
    if (ldqmatom(i)) then
      if (natdefe(i).le.maxele) then
        nqmc = nqmc + 1
        itmp(nqmc) = i
      else
        nqms = nqms + 1
        itmp(nqms) = i
      endif
    endif
  enddo
!****************************************
!  Sort region 1 into cores and shells  *
!****************************************
  ncoreg1 = 0
  nshreg1 = 0
!
!  Find number of each species type
!
  do i = 1,nreg1
    if (.not.ldqmatom(i)) then
      if (natdefe(i).le.maxele) then
        ncoreg1 = ncoreg1 + 1
      else
        nshreg1 = nshreg1 + 1
      endif
    endif
  enddo
!
!  Create pointer to old positions
!
  noff = nqmcr1 + nqmsr1
  nco = noff
  ns = ncoreg1 + nco
  do i = 1,nreg1
    if (.not.ldqmatom(i)) then
      if (natdefe(i).le.maxele) then
        nco = nco + 1
        itmp(nco) = i
      else
        ns = ns + 1
        itmp(ns) = i
      endif
    endif
  enddo
!
!  Skip sorting if not needed
!
  if (lnosort) then
    do i = 1,nreg1
      nptrr1(i) = itmp(i)
    enddo
    goto 20
  endif
!********************************
!  Sort QM species by distance  *
!********************************
  if (noff.gt.0) then
    allocate(tmp(nqmc),stat=status)
    if (status/=0) call outofmemory('sort1','tmp')
    do i = 1,nqmc
      xd = xdefe(itmp(i)) - xdc
      yd = ydefe(itmp(i)) - ydc
      zd = zdefe(itmp(i)) - zdc
      tmp(i) = xd*xd + yd*yd + zd*zd
    enddo
    do i = 1,nqmc
      r2max = 100000.0_dp
      do j = 1,nqmc
        if (tmp(j).lt.r2max) then
          jptr = j
          r2max = tmp(j)
        endif
      enddo
      tmp(jptr) = 200000.0_dp
      nptrr1(i) = itmp(jptr)
    enddo
!
!  Shells
!
    do i = nqmc + 1,nqms
      xd = xdefe(itmp(i)) - xdc
      yd = ydefe(itmp(i)) - ydc
      zd = zdefe(itmp(i)) - zdc
      tmp(i-nqmc) = xd*xd + yd*yd + zd*zd
    enddo
    do i = nqmc + 1,nqms
      r2max = 100000.0_dp
      do j = nqmc + 1,nqms
        if (tmp(j-nqmc).lt.r2max) then
          jptr = j
          r2max = tmp(j-nqmc)
        endif
      enddo
      tmp(jptr-nqmc) = 200000.0_dp
      nptrr1(i) = itmp(jptr)
    enddo
    deallocate(tmp,stat=status)
    if (status/=0) call deallocate_error('sort1','tmp')
  endif
!**************************************************
!  Order region 1 by distance from defect centre  *
!**************************************************
!
!  Cores
!
  allocate(tmp(nco),stat=status)
  if (status/=0) call outofmemory('sort1','tmp')
  do i = noff + 1,nco
    xd = xdefe(itmp(i)) - xdc
    yd = ydefe(itmp(i)) - ydc
    zd = zdefe(itmp(i)) - zdc
    tmp(i-noff) = xd*xd + yd*yd + zd*zd
  enddo
  do i = noff + 1,nco
    r2max = 100000.0_dp
    do j = noff + 1,nco
      if (tmp(j-noff).lt.r2max) then
        jptr = j
        r2max = tmp(j-noff)
      endif
    enddo
    tmp(jptr-noff) = 200000.0_dp
    nptrr1(i) = itmp(jptr)
  enddo
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('sort1','tmp')
!
!  Shells
!
  allocate(tmp(ns),stat=status)
  if (status/=0) call outofmemory('sort1','tmp')
  do i = nco + 1,ns
    xd = xdefe(itmp(i)) - xdc
    yd = ydefe(itmp(i)) - ydc
    zd = zdefe(itmp(i)) - zdc
    tmp(i-nco) = xd*xd + yd*yd + zd*zd
  enddo
  do i = nco + 1,ns
    r2max = 100000.0_dp
    do j = nco + 1,ns
      if (tmp(j-nco).lt.r2max) then
        jptr = j
        r2max = tmp(j-nco)
      endif
    enddo
    tmp(jptr-nco) = 200000.0_dp
    nptrr1(i) = itmp(jptr)
  enddo
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('sort1','tmp')
!
!  Perform shuffle
!
20 continue
  allocate(ltmp(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1','ltmp')
  allocate(itmp2(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1','itmp2')
  allocate(tmp(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1','tmp')
  call icollect(nreg1,natdefe,itmp,nptrr1)
  call icollect(nreg1,ntypdefe,itmp,nptrr1)
  call icollect(nreg1,ndefmol,itmp,nptrr1)
  call icollect(nreg1,ndefind,itmp,nptrr1)
  call icollect(nreg1,nreldef,itmp,nptrr1)
  call icollect(nreg1,inddfix,itmp,nptrr1)
  call collect(nreg1,xdefe,tmp,nptrr1)
  call collect(nreg1,ydefe,tmp,nptrr1)
  call collect(nreg1,zdefe,tmp,nptrr1)
  call collect(nreg1,qdefe,tmp,nptrr1)
  call collect(nreg1,occdefe,tmp,nptrr1)
  call collect(nreg1,radefe,tmp,nptrr1)
  call lcollect(nreg1,ldefbsmat,ltmp,nptrr1)
  call lcollect(nreg1,ldqmatom,ltmp,nptrr1)
  call lcollect(nreg1,ldfix,ltmp,nptrr1)
!
!  Change around idopt
!
  do i = 1,3
    do j = 1,nreg1
      itmp2(j) = idopt(3*(j-1) + i)
    enddo
    call icollect(nreg1,itmp2,itmp,nptrr1)
    do j = 1,nreg1
      idopt(3*(j-1) + i) = itmp2(j)
    enddo
  enddo
!
!  Change around bonding list and adjust bonded atom numbers
!
  call icollect(nreg1,nbondsdef,itmp,nptrr1)
  do i = 1,maxbond
    do j = 1,nreg1
      itmp2(j) = nbondeddef(i,j)
    enddo
    call icollect(nreg1,itmp2,itmp,nptrr1)
    do j = 1,nreg1
      if (itmp2(j).ne.0) then
        k = 1
        lfound = .false.
        do while (.not.lfound.and.k.le.nreg1)
          if (itmp2(j).eq.nptrr1(k)) then
            nbondeddef(i,j) = k
            lfound = .true.
          endif
          k = k + 1
        enddo
      else
        nbondeddef(i,j) = 0
      endif
    enddo
  enddo
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('sort1','tmp')
  deallocate(itmp2,stat=status)
  if (status/=0) call deallocate_error('sort1','itmp2')
  deallocate(ltmp,stat=status)
  if (status/=0) call deallocate_error('sort1','ltmp')
!
!  End sort
!
  if (lrestore) then
!
!  Need to sort deflist pointer if qm atoms are present
!
    if ((nqmcr1 + nqmsr1).gt.0) then
      do i = 1,ninte
        ndp = ndintptr(i)
        lfound = .false.
        j = 0
        do while (.not.lfound.and.j.lt.nreg1)
          j = j + 1
          lfound = (nptrr1(j).eq.ndp)
        enddo
        if (lfound) then
          ndintptr(i) = j
        endif
      enddo
    endif
  endif
!
!  Free local memory
!
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('sort1','itmp')
!
  return
  end
