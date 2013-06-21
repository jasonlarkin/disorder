  subroutine anneal(nvar,ifail,xc,fc)
!
!  Subroutine for performing globle optimisation ( (a search for
!  reasonable local minima) using a simulated annealing routine 
!  from numerical recipes. Scott Woodley R.I.G.B. October 1997.
!
!   1/08 random -> GULP_random
!
!  mvar     = max #variables and thus used in dimensions of arrays
!  nvar     = actual #variables and thus used in do loops of calcs
!  ngacjg   = #iterations in ga per call to conjugate minimiser
!  fconf    = value of trial function for configuration
!  xconf    = trial configuration of simplex stored in dynamic array
!
!  Scott Woodley, Royal Institution of G.B., October 1997
!  Julian Gale, NRI, Curtin University, January 2008
!
  use control
  use gaconf
  use general
  use genetic
  use iochannels
  use parallel
  implicit none
!
  integer nvar,ifail
  real(dp) :: xc(*)
  real(dp) :: fc,ftol,fac,temp,tlimit
  real(dp) :: pb,yb,t2,flow,fhigh,l
  real(dp) :: gcd(1),tmax,temptr,GULP_random,t1,cputime
  integer iflag,j,i,io,jo,iter
!     parameter(temp=100.0_dp,ftol=0.000000_dp)
!     parameter(fac=0.9_dp,tlimit=0.01_dp)
!
!  Check whether to compute cost function rather than energy
!
  if (index(keyword,'cost').ne.0) then
!  Evaluate Pannetier type cost function
    lgacost = .true.
  else
!  Evaluate function i.e. energy of system
    lgacost = .false.
  endif
!
!  Initialise parameters not set by user
!
  yb = 1.0d20
  tmax = 0.0_dp
  temptr = temp
!
!  Output parameters for annealing algorithm
!
!     call outsapar
!
!  Generate random starting configuration
!
  iflag = 0
  fc = 1000.0_dp
  j = 0
  do while (fc.gt.500.0_dp.and.j.lt.50)
    do i = 1,nvar
      xc(i) = GULP_random(iseed,1_i4)
    enddo
    call funct(iflag,nvar,xc,fc,gcd)
    j = j + 1
  enddo
!
!  Generate the initial simplex p(nvar+1,nvar)
!
  l = 0.2_dp
  do i = 1,nvar
    xconf(i,nvar+1) = xc(i)
  enddo
  fconf(nvar+1) = fc
  xc(1) = xc(1)+l
  call funct(iflag,nvar,xc,fc,gcd)
  do i = 1,nvar
    xconf(i,1) = xc(i)
  enddo
  fconf(1) = fc
  do i = 2,nvar
    xc(i-1) = xc(i-1)-l
    xc(i) = xc(i)+l
    call funct(iflag,nvar,xc,fc,gcd)
    do j = 1,nvar
      xconf(j,i) = xc(j)
    enddo
    fconf(i) = fc
  enddo
!
!  Start of iterative loop
!
  io = 0
  jo = 0
  do while (temptr.gt.0.0_dp)
    t1 = cputime()
    jo = jo + 1
!
!  Update parameters - lower annealing temperature
!
    iter = 10
    temptr = fac*temptr
    if (temptr.lt.tlimit) temptr = 0.0_dp
!
!  Downhill Simplex Annealing
!
    call amebsa(p,y,mvar,nvar,xc,yb,ftol,iter,temptr)
!
!  Conjugate Gradient Minimiser for odd top ten
!  implemented on certain iteration numbers
!  Need to save contents of xc since it rep best config
!       io = io + 1
!       lgaconjg = (io.eq.ngacjg)
!       if (lgaconjg) then
!        call precj(io,nvar,xc,p,y,mvar,ithbest,nvar+1)
!       endif
!
!  Check remaining cputime
!
    t2 = cputime()
    tdel = t2 - t1
    if (tdel.gt.tmax) tmax = tdel
!
!  Print out status of annealing process
!
    if (ioproc) then
      write(ioout,'(''           Annealing Temperature ='',f7.3, &
          ''  Lowest configuration found = '',f7.3)')temptr,yb
      if (iter.gt.0) then
        write(ioout,'(''Early exit             iter = '',i5)')iter
      endif
    endif
    call gasort(ithbest,1_i4,fconf,nvar+1_i4,0.0_dp,1_i4)
    flow=fconf(ithbest(1))
    call gasort(ithbest,1_i4,fconf,nvar+1_i4,0.0_dp,-1_i4)
    fhigh=fconf(ithbest(1))
    if (ioproc) then
     if (lgacost) then
      write(ioout,'(''Cycle:'',i4,'' Current Cost fn : Min ='',f10.3 &
      ,''   Max  ='',f10.3,''    CPU:'',f8.2)')jo,flow,fhigh,t2
     else
      write(ioout,'(''Cycle:'',i4,'' Current Energies: Min ='',f10.3 &
      ,''   Max  ='',f10.3,''    CPU:'',f8.2)')jo,flow,fhigh,t2
     endif
     call gflush(ioout)
    endif
    if ((timmax-t2).lt.tmax) then
      ifail = -1
      goto 10
    endif
  enddo
  ifail = 5
!
!  End of iterative loop
!
!  Reset lgacost flag such that energy can now be minimised
!
10 lgacost = .false.
  lgaconjg = .false.
  return
  end
