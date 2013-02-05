  subroutine gamutat(ngacfg,mcfg,nvar,ndiscret,pmuta,xmin,xmax,iseed,nspar,same,ibest,mbest,nc)
!
!  Perform mutation in genetic algorithm with probability pmuta.
!  If same>999  then make a note of configuration which change.
!  Called from gaopt.
!
!  Scott Woodley  Royal Institution   September 1997
!   Julian Gale   of Great Britain    September 1993
!
!   6/98 real*8 -> none and nspar added (smw)
!   6/98 gamuta -> gamutat (smw)
!   7/00 gridpoints start at 0 and end at (ngridpts-1) (smw)
!   1/08 random -> GULP_random
!
  use datatypes
  use gaconf, only : xconf
  implicit none
!
!  Passed arrays
!
  integer(i4) :: ngacfg
  integer(i4) :: mcfg
  integer(i4) :: nvar
  integer(i4) :: ndiscret(*)
  integer(i4) :: iseed
  integer(i4) :: nspar
  integer(i4) :: same
  integer(i4) :: ibest(*)
  integer(i4) :: nc
  integer(i4) :: mbest
  real(dp)    :: pmuta
  real(dp)    :: xmin(*)
  real(dp)    :: xmax(*)
!
!  Local arrays
!
  integer(i4) :: ibin(30)
  logical     :: lmuta(30)
!
!  Local variables
!
  integer(i4) i,j,nd,k,gridpt
  logical     ldomuta
  real(dp)    rn,GULP_random,ngridpts,xmi,xlength,xint,xc1
!
  if (same.lt.1000) then
    call gamuta(ngacfg,mcfg,nvar,ndiscret,pmuta,xmin,xmax,iseed,nspar)
    return
  endif
!
!  Loop over configurations
!
  do i = 1+mcfg+nspar,ngacfg+mcfg
!
!  Loop over variables
!
    do j = 1,nvar
      nd = ndiscret(j)
!
!  Loop over bits to see if any are to be flipped
!
      ldomuta = .false.
      do k = 1,nd
        rn = GULP_random(iseed,1_i4)
        if (rn.lt.pmuta) then
          lmuta(k) = .true.
          ldomuta = .true.
!
!  If middle bit flipped are we in another local min?
!
          if (k.ne.1.and.k.ne.nd.and.nc.lt.mbest) then
             nc = nc + 1
             ibest(nc) = i
          endif
        else
          lmuta(k) = .false.
        endif
      enddo
      if (ldomuta) then
!
!  Do mutation
!
! 1.1       rnd = 2.0_dp**nd - 1.0_dp
        ngridpts = 2.0_dp**nd
        xmi = xmin(j)
        xlength = xmax(j) - xmi
        xint = xlength/ngridpts
        xc1 = xconf(j,i)
! 1.1       nval1 = int((xc1-xmi)/xint)+1
        gridpt = int((xc1-xmi)/xint)
        call inttobin(gridpt,ibin,nd)
        do k = 1,nd
          if (lmuta(k)) then
            if (ibin(k).eq.1) then
              ibin(k) = 0
            else
              ibin(k) = 1
            endif
          endif
        enddo
        call bintoint(gridpt,ibin,nd)
! 1.1       xconf(j,i) = xmi + xint*(gridpt-1)
        xconf(j,i) = xmi + xint*gridpt
      endif
!
!  End loop over variables
!
    enddo
!
!  End loop over configurations
!
  enddo
  return
  end

  subroutine gamuta(ngacfg,mcfg,nvar,ndiscret,pmuta,xmin,xmax,iseed,nspar)
!
!  Perform mutation in genetic algorithm with probability pmuta.
!  Called from gafit and gamutat.
!
!  Scott Woodley  Royal Institution   September 1997
!   Julian Gale   of Great Britain    September 1993
!
!  6/98 real*8->none and nspar added (smw)
!
  use datatypes
  use gaconf, only : xconf
  implicit none
!
!  Passed variables
!
  integer(i4) :: ngacfg
  integer(i4) :: mcfg
  integer(i4) :: nvar
  integer(i4) :: ndiscret(*)
  integer(i4) :: iseed
  integer(i4) :: nspar
  real(dp)    :: pmuta
  real(dp)    :: xmin(*)
  real(dp)    :: xmax(*)
!
!  Local arrays
!
  integer(i4) :: ibin(30)
  integer(i4) :: i
  integer(i4) :: j
  integer(i4) :: nd
  integer(i4) :: k
  integer(i4) :: gridpt
  logical     :: lmuta(30)
  logical     :: ldomuta
  real(dp)    :: rn
  real(dp)    :: GULP_random
  real(dp)    :: ngridpts
  real(dp)    :: xmi
  real(dp)    :: xlength
  real(dp)    :: xint
  real(dp)    :: xc1
!
!  Loop over configurations
!
  do i = 1+mcfg+nspar,ngacfg+mcfg
!
!  Loop over variables
!
    do j = 1,nvar
      nd = ndiscret(j)
!
!  Loop over bits to see if any are to be flipped
!
      ldomuta = .false.
      do k = 1,nd
        rn = GULP_random(iseed,1_i4)
        if (rn.lt.pmuta) then
          lmuta(k) = .true.
          ldomuta = .true.
        else
          lmuta(k) = .false.
        endif
      enddo
      if (ldomuta) then
!
!  Do mutation
!
! 1.1       rnd = 2.0_dp**nd - 1.0_dp
        ngridpts = 2.0_dp**nd
        xmi = xmin(j)
        xlength = xmax(j) - xmi
        xint = xlength/ngridpts
        xc1 = xconf(j,i)
! 1.1       nval1 = int((xc1-xmi)/xint) + 1
        gridpt = int((xc1-xmi)/xint)
        if (gridpt.eq.-1) gridpt = 0
        call inttobin(gridpt,ibin,nd)
        do k = 1,nd
          if (lmuta(k)) then
            if (ibin(k).eq.1) then
              ibin(k) = 0
            else
              ibin(k) = 1
            endif
          endif
        enddo
        call bintoint(gridpt,ibin,nd)
! 1.1       xconf(j,i) = xmi + xint*(nval1-1)
        xconf(j,i) = xmi + xint*gridpt
      endif
!
!  End loop over variables
!
    enddo
!
!  End loop over configurations
!
  enddo
  return
  end
