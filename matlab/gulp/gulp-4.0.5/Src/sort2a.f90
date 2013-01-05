  subroutine sort2a
!
!  Sort region 2a so that all 2a atoms for harmonic relaxation come
!  first and all atoms only used for potential terms second.
!  Then sort all region 2a atoms into order of distance.
!
!   7/05 Style updated and memory deallocation cleaned
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use current
  use defects
  use region2a
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jptr
  integer(i4)                                  :: n2a2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nlreg2
  integer(i4)                                  :: nw1
  integer(i4)                                  :: nw2
  integer(i4), dimension(:), allocatable       :: nwrong1
  integer(i4), dimension(:), allocatable       :: nwrong2
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: ltmp
  real(dp)                                     :: r
  real(dp)                                     :: r2
  real(dp)                                     :: r22
  real(dp)                                     :: r2max
  real(dp)                                     :: rtmp
  real(dp),    dimension(:), allocatable       :: tmp
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
!
  r2 = reg2(ncf)
  r22 = r2*r2
!***************************************************************
!  First sort region 2a so that potential only terms are last  *
!***************************************************************
  nlreg2 = max(ntreg2,npreg2)
!
!  Allocate local memory
!
  allocate(nwrong1(max(nreg2,nlreg2)),stat=status)
  if (status/=0) call outofmemory('sort2a','nwrong1')
  allocate(nwrong2(max(nreg2,nlreg2)),stat=status)
  if (status/=0) call outofmemory('sort2a','nwrong2')
  allocate(tmp(max(nreg2,nlreg2)),stat=status)
  if (status/=0) call outofmemory('sort2a','tmp')
  allocate(ltmp(max(nreg2,nlreg2)),stat=status)
  if (status/=0) call outofmemory('sort2a','ltmp')
!
  if (nlreg2.gt.nreg2) then
    nw1 = 0
    nw2 = 0
!
!  Identify incorrectly ordered pairs
!
    do i = 1,nreg2
      xi = xr2a(i)
      yi = yr2a(i)
      zi = zr2a(i)
      xd = xi - xdc
      yd = yi - ydc
      zd = zi - zdc
      r = xd*xd + yd*yd + zd*zd
      if (r.gt.r22) then
        nw1 = nw1 + 1
        nwrong1(nw1) = i
      endif
    enddo
    do i = nreg2+1,nlreg2
      xi = xr2a(i)
      yi = yr2a(i)
      zi = zr2a(i)
      xd = xi - xdc
      yd = yi - ydc
      zd = zi - zdc
      r = xd*xd + yd*yd + zd*zd
      if (r.le.r22) then
        nw2 = nw2 + 1
        nwrong2(nw2) = i
      endif
    enddo
!
!  Swap pairs around
!
    do i = 1,nw1
      ii = nwrong1(i)
      jj = nwrong2(i)
      ni = nr2a(ii)
      nr2a(ii) = nr2a(jj)
      nr2a(jj) = ni
      ni = ntr2a(ii)
      ntr2a(ii) = ntr2a(jj)
      ntr2a(jj) = ni
      ni = nmr2a(ii)
      nmr2a(ii) = nmr2a(jj)
      nmr2a(jj) = ni
      ni = nmir2a(ii)
      nmir2a(ii) = nmir2a(jj)
      nmir2a(jj) = ni
      ni = nps(ii)
      nps(ii) = nps(jj)
      nps(jj) = ni
      rtmp = xr2a(ii)
      xr2a(ii) = xr2a(jj)
      xr2a(jj) = rtmp
      rtmp = yr2a(ii)
      yr2a(ii) = yr2a(jj)
      yr2a(jj) = rtmp
      rtmp = zr2a(ii)
      zr2a(ii) = zr2a(jj)
      zr2a(jj) = rtmp
      rtmp = qr2a(ii)
      qr2a(ii) = qr2a(jj)
      qr2a(jj) = rtmp
      rtmp = or2a(ii)
      or2a(ii) = or2a(jj)
      or2a(jj) = rtmp
      rtmp = rr2a(ii)
      rr2a(ii) = rr2a(jj)
      rr2a(jj) = rtmp
      ltmp(1) = ldbr2a(ii)
      ldbr2a(ii) = ldbr2a(jj)
      ldbr2a(jj) = ltmp(1)
    enddo
  endif
!********************************************
!  Sort region 2a ions into distance order  *
!********************************************
  do i = 1,nreg2
    xd = xr2a(i) - xdc
    yd = yr2a(i) - ydc
    zd = zr2a(i) - zdc
    tmp(i) = xd*xd + yd*yd + zd*zd
  enddo
  do i = 1,nreg2
    r2max = 100000.0_dp
    do j = 1,nreg2
      if (tmp(j).lt.r2max) then
        jptr = j
        r2max = tmp(j)
      endif
    enddo
    tmp(jptr) = 200000.0_dp
    nwrong1(i) = jptr
  enddo
  call icollect(nreg2,nr2a,nwrong2,nwrong1)
  call icollect(nreg2,ntr2a,nwrong2,nwrong1)
  call icollect(nreg2,nmr2a,nwrong2,nwrong1)
  call icollect(nreg2,nmir2a,nwrong2,nwrong1)
  call icollect(nreg2,nps,nwrong2,nwrong1)
  call collect(nreg2,xr2a,tmp,nwrong1)
  call collect(nreg2,yr2a,tmp,nwrong1)
  call collect(nreg2,zr2a,tmp,nwrong1)
  call collect(nreg2,qr2a,tmp,nwrong1)
  call collect(nreg2,or2a,tmp,nwrong1)
  call collect(nreg2,rr2a,tmp,nwrong1)
  call lcollect(nreg2,ldbr2a,ltmp,nwrong1)
!
  if (nlreg2.le.nreg2) goto 999
!*********************************************************
!  Sort remainder of region 2a ions into distance order  *
!*********************************************************
!
!  Needed for efficient evalution of 1-2a potential terms
!
  n2a2 = nlreg2 - nreg2
  do i = 1,n2a2
    xd = xr2a(i+nreg2) - xdc
    yd = yr2a(i+nreg2) - ydc
    zd = zr2a(i+nreg2) - zdc
    tmp(i) = xd*xd + yd*yd + zd*zd
  enddo
  do i = 1,n2a2
    r2max = 100000.0_dp
    do j = 1,n2a2
      if (tmp(j).lt.r2max) then
        jptr = j
        r2max = tmp(j)
      endif
    enddo
    tmp(jptr) = 200000.0_dp
    nwrong1(i) = jptr
  enddo
  call icollect(n2a2,nr2a(nreg2+1),nwrong2,nwrong1)
  call icollect(n2a2,ntr2a(nreg2+1),nwrong2,nwrong1)
  call icollect(n2a2,nmr2a(nreg2+1),nwrong2,nwrong1)
  call icollect(n2a2,nmir2a(nreg2+1),nwrong2,nwrong1)
  call icollect(n2a2,nps(nreg2+1),nwrong2,nwrong1)
  call collect(n2a2,xr2a(nreg2+1),tmp,nwrong1)
  call collect(n2a2,yr2a(nreg2+1),tmp,nwrong1)
  call collect(n2a2,zr2a(nreg2+1),tmp,nwrong1)
  call collect(n2a2,qr2a(nreg2+1),tmp,nwrong1)
  call collect(n2a2,or2a(nreg2+1),tmp,nwrong1)
  call collect(n2a2,rr2a(nreg2+1),tmp,nwrong1)
  call lcollect(n2a2,ldbr2a(nreg2+1),ltmp,nwrong1)
!**************
!  Exit point *
!**************
999 continue
!
!  Free local memory
!
  deallocate(ltmp,stat=status)
  if (status/=0) call deallocate_error('sort2a','ltmp')
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('sort2a','tmp')
  deallocate(nwrong2,stat=status)
  if (status/=0) call deallocate_error('sort2a','nwrong2')
  deallocate(nwrong1,stat=status)
  if (status/=0) call deallocate_error('sort2a','nwrong1')
!
  return
  end
