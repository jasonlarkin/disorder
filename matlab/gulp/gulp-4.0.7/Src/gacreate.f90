  subroutine gacreate(nga,ngacfg,nvar,ndiscret,xmin,xmax,iseed,ldiff)
!
!  Generate nga random initial configurations for ga method
!  NB. if nga<>ngacfg the last nga parents are randomised
!  Can be called by gafit, gaopt or gaexpd.
!
!   6/98 real*8 -> none (smw)
!   6/98 allow nga rather than ngacfg random configs (smw)
!   9/98 encourage ions to sit on different grid points (smw)
!   1/08 random -> GULP_random
!
!  Julian Gale (1993) and Scott Woodley (1998)
!     Royal Institution of Great Britain.
!
  use gaconf, only : xconf
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: nga,nvar
  integer(i4) :: ngacfg
  integer(i4) :: iseed
  integer(i4) :: ndiscret(*)
  logical     :: ldiff
  real(dp)    :: xmin(*),xmax(*)
!
!  Local variables
!
  integer(i4) :: i,nd,j,ip,k,ngridptl,nset,nle,ix,iy,iz
  integer(i4) :: iplocal,ngridpt
  real(dp)    :: rnd,xmi,xma,xdiff,xint,rn,GULP_random
  real(dp)    :: rxnd,rynd,rznd,rzynd
!
  if (ldiff) goto 1
!
!  Loop over variables
!
  do i = 1,nvar
    nd = ndiscret(i)
    rnd = 2.0_dp**nd - 1.0_dp
! new definition 24/9/98 smw
!       rnd = 2.0_dp**nd
! new definition 24/9/98 smw
    xmi = xmin(i)
    xma = xmax(i)
    xdiff = xma - xmi
    xint = xdiff/rnd
!
!  Loop over configurations
!
    do j = 1+ngacfg-nga,ngacfg
!
!  Generate random number
!
      rn = GULP_random(iseed,1_i4)
      ip = (rn*rnd)
      xconf(i,j) = xmi + xint*dble(ip)
    enddo
  enddo
  return

1     continue
!
! 24/9/98 smw
! old definition  rnd = 2.0_dp**nd-1.0_dp
! new definition  rnd = 2.0_dp**nd
!
  nd = ndiscret(1)
  rxnd = 2.0_dp**nd - 1.0_dp
  nd = ndiscret(2)
  rynd = 2.0_dp**nd - 1.0_dp
  nd = ndiscret(3)
  rznd = 2.0_dp**nd - 1.0_dp
  ngridpt = rxnd*rynd*rznd
  do i = 4,nvar,3
    nd = ndiscret(i)
    rxnd = 2.0_dp**nd - 1.0_dp
    nd = ndiscret(i+1)
    rynd = 2.0_dp**nd - 1.0_dp
    nd = ndiscret(i+2)
    rznd = 2.0_dp**nd - 1.0_dp
    if (ngridpt.ne.rxnd*rynd*rznd) then
      if (ioproc) then
        write(ioout,*)'total #(grid points) for variables not fixed'
      endif
      call stopnow('gacreate')
    endif
  enddo
!
!  Loop over configurations
!
  do j = 1+ngacfg-nga,ngacfg
    ngridptl = ngridpt
    do i = 1,nvar,3
      nset = 0
!
!  Generate random number
!
      rn = GULP_random(iseed,1_i4)
      ip = (rn*ngridptl)
      iplocal = ip
3         continue
      nle = 0
      do k = 1,i-1,3
        if (nint(xconf(k,j)).le.ip) nle = nle + 1
      enddo
      if (nle.gt.nset) then
        nset = nle
        ip = iplocal + nle
        goto 3
      endif
      xconf(i,j) = dble(ip)
      ngridptl = ngridptl - 1
      if (ngridptl.eq.0.and.i.lt.nvar-2) then
        if (ioproc) then
          write(ioout,*)'number of grid points < number of ions'
        endif
        call stopnow('gacreate')
      endif
    enddo
! 24/9/98 smw
! old definition  rnd = 2.0_dp**nd-1.0_dp
! new definition  rnd = 2.0_dp**nd
    do i = 1,nvar,3
      nd = ndiscret(i)
      rxnd = 2.0_dp**nd - 1.0_dp
      nd = ndiscret(i+1)
      rynd = 2.0_dp**nd - 1.0_dp
      nd = ndiscret(i+2)
      rznd = 2.0_dp**nd - 1.0_dp
      rzynd = rznd*rynd
      ip = nint(xconf(i,j))
      ix = ip/rzynd
      ip = ip - ix*rzynd
      iy = ip/(rznd)
      iz = ip - iy*rznd
      if (ix.ge.rxnd) then
        if (ioproc) then
          write(*,*)'ix = ',ix,'rxnd=',rxnd
        endif
        call stopnow('gacreate')
      endif
      if (iy.ge.rynd) then
        if (ioproc) then
          write(*,*)'iy = ',iy,'rynd=',rynd
        endif
        call stopnow('gacreate')
      endif
      if (iz.ge.rznd) then
        if (ioproc) then
          write(*,*)'iz = ',iz,'rznd=',rznd
        endif
        call stopnow('gacreate')
      endif
      xmi = xmin(i)
      xma = xmax(i)
      xdiff = xma - xmi
      xint = xdiff/rxnd
      xconf(i,j) = xmi + xint*dble(ix)
      xmi = xmin(i+1)
      xma = xmax(i+1)
      xdiff = xma - xmi
      xint = xdiff/rynd
      xconf(i+1,j) = xmi + xint*dble(iy)
      xmi = xmin(i+2)
      xma = xmax(i+2)
      xdiff = xma - xmi
      xint = xdiff/rznd
      xconf(i+2,j) = xmi + xint*dble(iz)
    enddo
  enddo
  return
  end
