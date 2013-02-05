  subroutine optshe(istep,etot,iter,spring,bspring)
!
!  Subroutine for optimizing the positions of shells during an MD run.
!
!  Assumes that all shell positions are variable.
!
!   2/97 Modified for breathing shells (JDG)
!   1/05 Logical to compute interatomic vector table added in call
!        to mdfunct. This table only really needs setting once. (JDG)
!   4/05 Extrapolation of previous shell positions added to improve optimisation (JDG)
!   7/05 Use of ratiom replaced by ladiabatic logical
!   5/07 nbsmptr replaced by nbsptr
!  10/08 New default algorithm added in which the core-shell vector is used to initialise
!        the shell positions for the new step
!  10/08 Extrapolation is now made on core-shell vector
!   8/11 Call to mdfunct modified to pass step number
!   8/11 Arguments into routine now include step number 
!
!  Initially written by J.R. Hill, January 1996
!
!  Last modified to by Julian Gale, August 2011
!
  use control
  use current
  use derivatives
  use iochannels
  use mdlogic,            only : ladiabatic
  use moldyn
  use optimisation
  use parallel
  use partial,            only : nbsptr
  use shell
  use shellextrapolation
  use velocities
  implicit none
!
!  Passed variables
!
  integer(i4),     intent(in)    :: istep           ! Step number of MD run
  integer(i4),     intent(out)   :: iter            ! Number of iterations 
  real(dp),        intent(in)    :: bspring(*)      ! Spring constants for breathing shells
  real(dp),        intent(out)   :: etot            ! Final energy
  real(dp),        intent(in)    :: spring(*)       ! Spring constants for shells
!
!  Local variables
!
  integer(i4)                    :: i
  integer(i4)                    :: ibp
  integer(i4)                    :: icp
  integer(i4)                    :: isp
  integer(i4)                    :: m
  integer(i4),              save :: nextrapol = 0
  integer(i4)                    :: status
  real(dp)                       :: rrmi
  real(dp),    allocatable       :: tmp1(:)
  real(dp),    allocatable       :: tmp2(:)
!
  if (lextrapolateshells.and.nextrapol.gt.3) then
!
!  Extrapolate shell positions using rational functions 
!  only start once 3 points are already known though
!
    m = min(nextrapol,maxextrapol)
    allocate(tmp1(m),stat=status)
    if (status/=0) call outofmemory('optshe','tmp1')
    allocate(tmp2(m),stat=status)
    if (status/=0) call outofmemory('optshe','tmp2')
!
!  Extrapolate forward core-shell vector
!
    do i = 1,nshell
      isp = nshptr(i)
      icp = ncsptr(isp)
      call ratfn(m,xshellsave(1,i),xalat(isp),tmp1,tmp2)
      call ratfn(m,yshellsave(1,i),yalat(isp),tmp1,tmp2)
      call ratfn(m,zshellsave(1,i),zalat(isp),tmp1,tmp2)
!
!  Add on core position
!
      xalat(isp) = xalat(isp) + xalat(icp)
      yalat(isp) = yalat(isp) + yalat(icp)
      zalat(isp) = zalat(isp) + zalat(icp)
    enddo
!
    deallocate(tmp2,stat=status)
    if (status/=0) call deallocate_error('optshe','tmp2')
    deallocate(tmp1,stat=status)
    if (status/=0) call deallocate_error('optshe','tmp1')
  elseif (nextrapol.eq.1) then
!
!  Position shell based on previous core-shell vector
!
    do i = 1,nshell
      isp = nshptr(i)
      icp = ncsptr(isp)
      xalat(isp) = xalat(icp) + xshellsave(1,i)
      yalat(isp) = yalat(icp) + yshellsave(1,i)
      zalat(isp) = zalat(icp) + zshellsave(1,i)
    enddo
  endif
!
!  Optimise shell/radii positions
!
  gnorm = 1.0_dp
  iter = 0
  do while (gnorm.gt.sgtol.and.iter.le.moptit)
    iter = iter + 1
    call mdfunct(0_i4,etot,.false.,iter.eq.1,.true.)
    gnorm = 0.0_dp
    do i = 1,nshell
      isp = nshptr(i)
      if (.not.lfix(isp)) then
        xalat(isp) = xalat(isp) - spring(i)*xdrv(isp)
        yalat(isp) = yalat(isp) - spring(i)*ydrv(isp)
        zalat(isp) = zalat(isp) - spring(i)*zdrv(isp)
        gnorm = gnorm + xdrv(isp)*xdrv(isp) + ydrv(isp)*ydrv(isp) + zdrv(isp)*zdrv(isp)
      endif
    enddo
    if (nbsmat.ne.0) then
!
!  Strictly speaking we should re-calculate the
!  radial derivatives at this point, but convergence
!  can be achieved without it in those cases tried.
!
      do i = 1,nbsmat
        ibp = nbsptr(i)
        if (.not.lfix(ibp)) then
          radf(ibp) = radf(ibp) - bspring(i)*raderv(ibp)
          gnorm = gnorm + raderv(ibp)*raderv(ibp)
        endif
      enddo
    endif
  enddo
  if (index(keyword,'debu').ne.0.and.ioproc) then
    write(ioout,'(''  Shell opt: Iter = '',i2,'' Gnorm = '',g12.6)') iter,gnorm
  endif

  if (.not.ladiabatic) then
    do i = 1,numat
      if (lopf(i).and..not.lfix(i)) then
        rrmi = rmass(i)
        x2(i) = - rrmi*xdrv(i)*stpsqh
        y2(i) = - rrmi*ydrv(i)*stpsqh
        z2(i) = - rrmi*zdrv(i)*stpsqh
      endif
    enddo
    return
  endif
  if (lextrapolateshells) then
!
!  Store shell positions for future extrapolation
!
    nextrapol = nextrapol + 1
    if (nextrapol.gt.maxextrapol) then
      do m = 2,maxextrapol
        do i = 1,nshell
          xshellsave(m-1,i) = xshellsave(m,i)
          yshellsave(m-1,i) = yshellsave(m,i)
          zshellsave(m-1,i) = zshellsave(m,i)
        enddo
      enddo
    endif
    m = min(nextrapol,maxextrapol)
    do i = 1,nshell
      isp = nshptr(i)
      icp = ncsptr(isp)
      xshellsave(m,i) = xalat(isp) - xalat(icp)
      yshellsave(m,i) = yalat(isp) - yalat(icp)
      zshellsave(m,i) = zalat(isp) - zalat(icp)
    enddo
  else
!
!  Store position of shell relative to its core
!
    nextrapol = 1
    do i = 1,nshell
      isp = nshptr(i)
      icp = ncsptr(isp)
      xshellsave(1,i) = xalat(isp) - xalat(icp)
      yshellsave(1,i) = yalat(isp) - yalat(icp)
      zshellsave(1,i) = zalat(isp) - zalat(icp)
    enddo
  endif
!
!  Update forces
!
  call mdfunct(istep,etot,.false.,iter.eq.0,.true.)
!
  return
  end
