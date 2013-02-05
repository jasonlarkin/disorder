module m_simplex

  use datatypes
  use interupt,    only : controlC, sigint
  use iochannels
  use parallel,    only : ioproc
  use general,     only : time0

  implicit none

  public :: subplx

  integer(i4),  private :: nsmin,nsmax,irepl,ifxsw,nfstop,nfxe
  real(dp),     private :: alpha,beta,gamma,delta,psi,omega,bonus,fstop,fxstat(4),ftest
  logical,      private :: minf,initx
! This variable is not needed at present unless the line that sets it is uncommented
!  logical,      private :: newx
!
  real(dp),     private :: fbonus,sfstop,sfbest
  logical,      private :: new

CONTAINS

  subroutine subplx(n,tol,maxnfe,maxcyc,ncycd,idump,mode,scale,x,fx,nfe,iflag)
 
  use observables, only : fcalc, fcalcoriginal, nobs

  integer(i4), intent(in)    :: n
  integer(i4), intent(out)   :: iflag
  integer(i4), intent(in)    :: idump
  integer(i4), intent(in)    :: maxcyc
  integer(i4), intent(in)    :: maxnfe
  integer(i4), intent(in)    :: mode
  integer(i4), intent(in)    :: ncycd
  integer(i4), intent(out)   :: nfe
  real(dp),    intent(in)    :: scale(*)
  real(dp),    intent(in)    :: tol
  real(dp),    intent(inout) :: x(n)
  real(dp),    intent(out)   :: fx
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! subplx uses the subplex method to solve unconstrained
! optimization problems.  The method is well suited for
! optimizing objective functions that are noisy or are
! discontinuous at the solution.
!
! subplx sets default optimization options by calling the
! subroutine subopt.  The user can override these defaults
! by calling subopt prior to calling subplx, changing the
! appropriate common variables, and setting the value of
! mode as indicated below.
!
! By default, subplx performs minimization.
!
! input
!
!   n      - problem dimension
!
!   tol    - relative error tolerance for x (tol .ge. 0.)
!
!   maxnfe - maximum number of function evaluations
!
!   mode   - integer mode switch with binary expansion
!                  = 0 : use default options
!                  = 1 : user set options
!
!   scale  - scale and initial stepsizes for corresponding
!            components of x
!            (If scale(1) .lt. 0.,
!            abs(scale(1)) is used for all components of x,
!            and scale(2),...,scale(n) are not referenced.)
!
!   x      - starting guess for optimum
!
!   work   - real(dp) work array of dimension .ge.
!            2*n + nsmax*(nsmax+4) + 1
!            (nsmax is set in subroutine subopt.
!            default: nsmax = min(5,n))
!
!   iwork  - integer work array of dimension .ge.
!            n + int(n/nsmin)
!            (nsmin is set in subroutine subopt.
!            default: nsmin = min(2,n))
!
! output
!
!   x      - computed optimum
!
!   fx     - value of f at x
!
!   nfe    - number of function evaluations
!
!   iflag  - error flag
!            = -2 : invalid input
!            = -1 : maxnfe exceeded
!            =  0 : tol satisfied
!            =  1 : limit of machine precision
!            =  2 : fstop reached (fstop usage is determined
!                   by values of options minf, nfstop, and
!                   irepl. default: f(x) not tested against
!                   fstop)
!            iflag should not be reset between calls to
!            subplx.
!
! local variables
!
  integer(i4)                    :: i
  integer(i4)                    :: ifsptr
  integer(i4)                    :: ins
  integer(i4)                    :: insfnl
  integer(i4)                    :: insptr
  integer(i4)                    :: ipptr
  integer(i4)                    :: isptr
  integer(i4)                    :: istep
  integer(i4)                    :: istptr
  integer(i4)                    :: j
  integer(i4)                    :: ns
  integer(i4)                    :: nsubs
  integer(i4)                    :: ntest
  integer(i4)                    :: jcyc
  integer(i4)                    :: niwork
  integer(i4)                    :: nwork
  real(dp),                 save :: bnsfac(3,2)
  real(dp)                       :: cputime
  real(dp)                       :: scl
  real(dp)                       :: sfx
  real(dp)                       :: time1
  real(dp)                       :: xpscl
  integer(i4), allocatable, save :: iwork(:)
  real(dp),    allocatable, save :: work(:)
  real(dp),    allocatable, save :: dum(:)
  logical                        :: cmode
!
  external                       :: trap_sigint
!
! data
!
  data ((bnsfac(i,j),i=1,3),j=1,2) /-1.0_dp,-2.0_dp,0.0_dp,1.0_dp,0.0_dp,2.0_dp/
!-----------------------------------------------------------
!
!  Call sig handler
!
  call csignal(sigint,trap_sigint)
!
!       first call, check input
!
  if (n.lt.1) go to 120
  if (tol.lt.0.0_dp) go to 120
  if (maxnfe.lt.1) go to 120
  if (scale(1) .ge. 0.0_dp) then
    do i = 1,n
      xpscl = x(i) + scale(i)
      if (xpscl.eq.x(i)) go to 120
    enddo
  else
    scl = abs(scale(1))
    do i = 1,n
      xpscl = x(i) + scl
      if (xpscl.eq.x(i)) go to 120
    enddo
  endif
  if (mode.eq.0) then
    call subopt(n)
  else
    if (alpha .le. 0.0_dp) go to 120
    if (beta .le. 0.0_dp .or. beta .ge. 1.0_dp) go to 120
    if (gamma .le. 1.0_dp) go to 120
    if (delta .le. 0.0_dp .or. delta .ge. 1.0_dp) go to 120
    if (psi .le. 0.0_dp .or. psi .ge. 1.0_dp) go to 120
    if (omega .le. 0.0_dp .or. omega .ge. 1.0_dp) go to 120
    if (nsmin .lt. 1 .or. nsmax .lt. nsmin .or. n .lt. nsmax) go to 120
    if (n .lt. ((n-1)/nsmax+1)*nsmin) go to 120
    if (irepl .lt. 0 .or. irepl .gt. 2) go to 120
    if (ifxsw .lt. 1 .or. ifxsw .gt. 3) go to 120
    if (bonus .lt. 0.0_dp) go to 120
    if (nfstop .lt. 0) go to 120
  endif
!
!  Allocate workspace
!
  nwork = 2*n + nsmax*(nsmax+4) + 1
  niwork = n + int(n/nsmin)
  allocate(work(nwork))
  allocate(iwork(niwork))
  allocate(dum(n))
!
!  initialization
!
  istptr = n + 1
  isptr = istptr + n
  ifsptr = isptr + nsmax*(nsmax+3)
  insptr = n + 1
  if (scale(1) .gt. 0.0_dp) then
    call dcopy(n,scale,1_i4,work,1_i4)
    call dcopy(n,scale,1_i4,work(istptr),1_i4)
  else
    call dcopy(n,scl,0_i4,work,1_i4)
    call dcopy(n,scl,0_i4,work(istptr),1_i4)
  endif
  do i = 1,n
    iwork(i) = i
  enddo
  nfe = 0
  nfxe = 1
  if (irepl .eq. 0) then
    fbonus = 0.0_dp
  elseif (minf) then
    fbonus = bnsfac(ifxsw,1)*bonus
  else
    fbonus = bnsfac(ifxsw,2)*bonus
  endif
  if (nfstop .eq. 0) then
    sfstop = 0.0_dp
  elseif (minf) then
    sfstop = fstop
  else
    sfstop = -fstop
  endif
  ftest = 0.0_dp
  cmode = .false.
  new = .true.
  initx = .true.
  call evalf(0_i4,iwork,dum,n,x,sfx,nfe)
  initx = .false.
!
!  Initial evaluation
!
  call fitfun(n,x,fx)
!
!  Copy original fit quality data to saved array for later use
!
  fcalcoriginal(1:nobs) = fcalc(1:nobs)
!
  time1 = cputime() - time0
  write(ioout,'(''  Cycle:      0'',''  Sum sqs: '',f20.6,''  CPU:'',f9.3)') fx,time1
!
!  subplex loop
!
  jcyc = 0
40 continue
  jcyc = jcyc + 1
  do i = 1,n
    work(i) = abs(work(i))
  enddo
  call sortd(n,work,iwork)
  call partx(n,iwork,work,nsubs,iwork(insptr))
  call dcopy(n,x,1_i4,work,1_i4)
  ins = insptr
  insfnl = insptr + nsubs - 1
  ipptr = 1
!
!  simplex loop
!
60 continue
  ns = iwork(ins)
  call simplx(n,work(istptr),ns,iwork(ipptr), &
              maxnfe,cmode,x,sfx,nfe,work(isptr), &
              work(ifsptr),iflag)
  cmode = .false.
  if (iflag .ne. 0.or.controlC) go to 110
  if (ins .lt. insfnl) then
    ins = ins + 1
    ipptr = ipptr + ns
    go to 60
  endif
!
!  end simplex loop
!
  do i = 1,n
    work(i) = x(i) - work(i)
  enddo
!
!  check termination
!
  istep = istptr
  time1 = cputime() - time0
  write(ioout,'(''  Cycle: '',i6,''  Sum sqs: '',f20.6,''  CPU:'',f9.3)') jcyc,sfx,time1
!
!  Write out intermediate dumpfile if necessary
!
  ntest = jcyc/ncycd
  ntest = jcyc - ntest*ncycd
  if (ntest.eq.0.and.idump.gt.0.and.ioproc) then
    call dumpdur(idump,jcyc)
  endif
!
  if (jcyc.ge.maxcyc) then
    iflag = -1
    goto 110
  endif
  do i = 1,n
    if (max(abs(work(i)),abs(work(istep))*psi)/max(abs(x(i)),1.0_dp) .gt. tol) then
      call setstp(nsubs,n,work,work(istptr))
      go to 40
    endif
    istep = istep + 1
  enddo
!
!  end subplex loop
!
  iflag = 0
110 continue
  if (minf) then
    fx = sfx
  else
    fx = -sfx
  endif
!
!  Deallocate workspace
!
  deallocate(dum)
  deallocate(iwork)
  deallocate(work)
  return
!
!  invalid input
!
120 continue
  iflag = -2

  end subroutine subplx

  subroutine calcc(ns,s,ih,inew,updatc,c)
!
  integer(i4) :: ns,ih,inew
  real(dp)    :: s(ns,ns+3),c(ns)
  logical     :: updatc
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! calcc calculates the centroid of the simplex without the
! vertex with highest function value.
!
! input
!
!   ns     - subspace dimension
!
!   s      - real(dp) work space of dimension .ge.
!            ns*(ns+3) used to store simplex
!
!   ih     - index to vertex with highest function value
!
!   inew   - index to new point
!
!   updatc - logical switch
!            = .true.  : update centroid
!            = .false. : calculate centroid from scratch
!
!   c      - centroid of the simplex without vertex with
!            highest function value
!
! output
!
!   c      - new centroid
!
! local variables
!
  integer(i4) :: i,j
!
!-----------------------------------------------------------
!
  if (updatc) then
    if (ih .eq. inew) return
    do i = 1,ns
      c(i) = c(i) + (s(i,inew)-s(i,ih))/ns
    enddo
  else
    call dcopy(ns,0.0_dp,0_i4,c,1_i4)
    do j = 1,ns+1
      if (j.ne.ih) call daxpy(ns,1.0_dp,s(1,j),1_i4,c,1_i4)
    enddo
    call dscal(ns,1.0_dp/ns,c,1_i4)
  endif
  return
  end subroutine calcc

  real(dp) function dist (n,x,y)
!
  integer(i4) :: n
  real(dp)    :: x(n)
  real(dp)    :: y(n)
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! dist calculates the distance between the points x,y.
!
! input
!
!   n      - number of components
!
!   x      - point in n-space
!
!   y      - point in n-space
!
! local variables
!
  integer(i4) :: i
  real(dp)    :: absxmy
  real(dp)    :: scale
  real(dp)    :: sum
!
!-----------------------------------------------------------
!
  absxmy = abs(x(1)-y(1))
  if (absxmy.le.1.0_dp) then
    sum = absxmy*absxmy
    scale = 1.0_dp
  else
    sum = 1.0_dp
    scale = absxmy
  end if
  do i = 2,n
    absxmy = abs(x(i)-y(i))
    if (absxmy.le.scale) then
      sum = sum+(absxmy/scale)**2
    else
      sum = 1.0_dp + sum*(scale/absxmy)**2
      scale = absxmy
    endif
  enddo
  dist = scale*sqrt(sum)
  return
  end function dist
!
  subroutine fstats(fx,ifxwt,reset)
!
  integer(i4) :: ifxwt
  real(dp)    :: fx
  logical     :: reset
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! fstats modifies the variables nfxe,fxstat.
!
! input
!
!   fx     - most recent evaluation of f at best x
!
!   ifxwt  - integer weight for fx
!
!   reset  - logical switch
!            = .true.  : initialize nfxe,fxstat
!            = .false. : update nfxe,fxstat
!
! local variables
!
  integer(i4) :: nsv
  real(dp)    :: fscale
  real(dp)    :: f1sv
!
  save
!
!-----------------------------------------------------------
!
  if (reset) then
    nfxe = ifxwt
    fxstat(1) = fx
    fxstat(2) = fx
    fxstat(3) = fx
    fxstat(4) = 0.0_dp
  else
    nsv = nfxe
    f1sv = fxstat(1)
    nfxe = nfxe + ifxwt
    fxstat(1) = fxstat(1) + ifxwt*(fx-fxstat(1))/nfxe
    fxstat(2) = max(fxstat(2),fx)
    fxstat(3) = min(fxstat(3),fx)
    fscale = max(abs(fxstat(2)),abs(fxstat(3)),1.0_dp)
    fxstat(4) = fscale*sqrt(((nsv-1)*(fxstat(4)/fscale)**2+ &
    nsv*((fxstat(1)-f1sv)/fscale)**2+ifxwt*((fx-fxstat(1))/fscale)**2)/(nfxe-1))
  endif
  return
  end subroutine fstats
!
  subroutine newpt(ns,coef,xbase,xold,new,xnew,small)
!
  integer(i4) :: ns
  real(dp)    :: coef,xbase(ns),xold(ns),xnew(*)
  logical     :: new,small
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! newpt performs reflections, expansions, contractions, and
! shrinkages (massive contractions) by computing:
!
! xbase + coef * (xbase - xold)
!
! The result is stored in xnew if new .eq. .true.,
! in xold otherwise.
!
! use :  coef .gt. 0 to reflect
!        coef .lt. 0 to expand, contract, or shrink
!
! input
!
!   ns     - number of components (subspace dimension)
!
!   coef   - one of four simplex method coefficients
!
!   xbase  - real(dp) ns-vector representing base
!            point
!
!   xold   - real(dp) ns-vector representing old
!            point
!
!   new    - logical switch
!            = .true.  : store result in xnew
!            = .false. : store result in xold, xnew is not
!                        referenced
!
! output
!
!   xold   - unchanged if new .eq. .true., contains new
!            point otherwise
!
!   xnew   - real(dp) ns-vector representing new
!            point if  new .eq. .true., not referenced
!            otherwise
!
!   small  - logical flag
!            = .true.  : coincident points
!            = .false. : otherwise
!
! local variables
!
  integer(i4) :: i
  real(dp) xoldi
  logical eqbase,eqold
!
!-----------------------------------------------------------
!
  eqbase = .true.
  eqold = .true.
  if (new) then
    do i = 1,ns
      xnew(i) = xbase(i) + coef*(xbase(i)-xold(i))
      eqbase = eqbase .and. (dble(xnew(i)).eq.dble(xbase(i)))
      eqold = eqold .and. (dble(xnew(i)).eq.dble(xold(i)))
    enddo
  else
    do i = 1,ns
      xoldi = xold(i)
      xold(i) = xbase(i) + coef*(xbase(i)-xold(i))
      eqbase = (eqbase.and.(dble(xold(i)).eq.dble(xbase(i))))
      eqold = (eqold.and.(dble(xold(i)).eq.dble(xoldi)))
    enddo
  endif
  small = (eqbase.or.eqold)
  return
  end subroutine newpt
!
  subroutine order(npts,fs,il,is,ih)
!
  integer(i4) :: npts,il,is,ih
  real(dp)    :: fs(npts)
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! order determines the indices of the vertices with the
! lowest, second highest, and highest function values.
!
! input
!
!   npts   - number of points in simplex
!
!   fs     - real(dp) vector of function values of
!            simplex
!
!   il     - index to vertex with lowest function value
!
! output
!
!   il     - new index to vertex with lowest function value
!
!   is     - new index to vertex with second highest
!            function value
!
!   ih     - new index to vertex with highest function value
!
! local variables
!
  integer(i4) :: i,il0,j
!
!-----------------------------------------------------------
!
  il0 = il
  j = mod(il0,npts) + 1
  if (fs(j).ge.fs(il)) then
    ih = j
    is = il0
  else
    ih = il0
    is = j
    il = j
  endif
  do i = il0+1,il0+npts-2
    j = mod(i,npts) + 1
    if (fs(j) .ge. fs(ih)) then
      is = ih
      ih = j
    elseif (fs(j).gt.fs(is)) then
      is = j
    elseif (fs(j).lt.fs(il)) then
      il = j
    endif
  enddo
  return
  end subroutine order
!
  subroutine partx(n,ip,absdx,nsubs,nsvals)
!
  integer(i4), intent(in)    :: n
  integer(i4), intent(inout) :: nsvals(*)
  integer(i4), intent(in)    :: ip(n)
  real(dp),    intent(in)    :: absdx(n)
  integer(i4), intent(out)   :: nsubs
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! partx partitions the vector x by grouping components of
! similar magnitude of change.
!
! input
!
!   n      - number of components (problem dimension)
!
!   ip     - permutation vector
!
!   absdx  - vector of magnitude of change in x
!
!   nsvals - integer array dimensioned .ge. int(n/nsmin)
!
! output
!
!   nsubs  - number of subspaces
!
!   nsvals - integer array of subspace dimensions
!
! local variables
!
  integer(i4) :: i,nleft,ns1,ns2,nused
  real(dp)    :: asleft,as1,as1max,as2,gap,gapmax
!
  save
!
!-----------------------------------------------------------
!
  nsubs = 0
  nused = 0
  nleft = n
  asleft = absdx(1)
  do i = 2,n
    asleft = asleft + absdx(i)
  enddo
20 continue
  if (nused.lt.n) then
    nsubs = nsubs + 1
    as1 = 0.0_dp
    do i = 1,nsmin-1
      as1 = as1 + absdx(ip(nused+i))
    enddo
    gapmax = -1.0_dp
    do ns1 = nsmin,min(nsmax,nleft)
      as1 = as1 + absdx(ip(nused+ns1))
      ns2 = nleft - ns1
      if (ns2.gt.0) then
        if (ns2.ge.((ns2-1)/nsmax+1)*nsmin) then
          as2 = asleft - as1
          gap = as1/ns1 - as2/ns2
          if (gap.gt.gapmax) then
            gapmax = gap
            nsvals(nsubs) = ns1
            as1max = as1
          endif
        endif
      else
        if (as1/ns1.gt.gapmax) then
          nsvals(nsubs) = ns1
          return
        endif
      endif
    enddo
    nused = nused + nsvals(nsubs)
    nleft = n - nused
    asleft = asleft - as1max
    goto 20
  endif
  return
  end subroutine partx
!
  subroutine setstp(nsubs,n,deltax,step)
!
  integer(i4) :: nsubs,n
  real(dp)    :: deltax(n),step(n)
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! setstp sets the stepsizes for the corresponding components
! of the solution vector.
!
! input
!
!   nsubs  - number of subspaces
!
!   n      - number of components (problem dimension)
!
!   deltax - vector of change in solution vector
!
!   step   - stepsizes for corresponding components of
!            solution vector
!
! output
!
!   step   - new stepsizes
!
! local variables
!
  integer(i4) :: i
  real(dp)    :: dasum
  real(dp)    :: stpfac
!
!-----------------------------------------------------------
!
!     set new step
!
  if (nsubs .gt. 1) then
    stpfac = min(max(dasum(n,deltax,1)/dasum(n,step,1),omega),1.0_dp/omega)
  else
    stpfac = psi
  endif
  call dscal(n,stpfac,step,1_i4)
!
! reorient simplex
!
  do i = 1,n
    if (deltax(i) .ne. 0.) then
      step(i) = sign(step(i),deltax(i))
    else
      step(i) = -step(i)
    endif
  enddo
  return
  end subroutine setstp
!
  subroutine simplx(n,step,ns,ips,maxnfe,cmode,x,fx,nfe,s,fs,iflag)
!
  integer(i4) :: n,ns,maxnfe,nfe,iflag
  integer(i4) :: ips(ns)
  real(dp)    :: step(n),x(n),fx,s(ns,ns+3),fs(ns+1)
  logical     :: cmode
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! simplx uses the Nelder-Mead simplex method to minimize the
! function f on a subspace.
!
! input
!
!   n      - problem dimension
!
!   step   - stepsizes for corresponding components of x
!
!   ns     - subspace dimension
!
!   ips    - permutation vector
!
!   maxnfe - maximum number of function evaluations
!
!   cmode  - logical switch
!            = .true.  : continuation of previous call
!            = .false. : first call
!
!   x      - starting guess for minimum
!
!   fx     - value of f at x
!
!   nfe    - number of function evaluations
!
!   s      - real(dp) work array of dimension .ge.
!            ns*(ns+3) used to store simplex
!
!   fs     - real(dp) work array of dimension .ge.
!            ns+1 used to store function values of simplex
!            vertices
!
! output
!
!   x      - computed minimum
!
!   fx     - value of f at x
!
!   nfe    - incremented number of function evaluations
!
!   iflag  - error flag
!            = -1 : maxnfe exceeded
!            =  0 : simplex reduced by factor of psi
!            =  1 : limit of machine precision
!            =  2 : reached fstop
!
! local variables
!
  integer(i4)                 :: i
  integer(i4)                 :: icent
  integer(i4)                 :: ih
  integer(i4)                 :: il
  integer(i4)                 :: inew
  integer(i4)                 :: is
  integer(i4)                 :: itemp
  integer(i4)                 :: j
  integer(i4)                 :: npts
  real(dp),              save :: fc,fe,fr
  real(dp)                    :: tol
  real(dp), allocatable, save :: dum(:)
  logical                     :: small
  logical                     :: updatc
!
! subroutines and functions
!
  external fitfun
!
!-----------------------------------------------------------
!
  allocate(dum(n))
  if (cmode) go to 50
  npts  = ns + 1
  icent = ns + 2
  itemp = ns + 3
  updatc = .false.
  call start(n,x,step,ns,ips,s,small)
  if (small) then
    iflag = 1
    return
  endif
  if (irepl .gt. 0) then
    new = .false.
    call evalf(ns,ips,s(1,1),n,x,fs(1),nfe)
  else
    fs(1) = fx
  endif
  new = .true.
  do j = 2,npts
    call evalf(ns,ips,s(1,j),n,x,fs(j),nfe)
  enddo
  il = 1
  call order(npts,fs,il,is,ih)
  tol = psi*dist(ns,s(1,ih),s(1,il))
!
! main loop
!
20 continue
  call calcc(ns,s,ih,inew,updatc,s(1,icent))
  updatc = .true.
  inew = ih
!
! reflect
!
  call newpt(ns,alpha,s(1,icent),s(1,ih),.true.,s(1,itemp),small)
  if (small) go to 40
  call evalf(ns,ips,s(1,itemp),n,x,fr,nfe)
  if (fr.lt.fs(il)) then
!
! expand
!
    call newpt(ns,-gamma,s(1,icent),s(1,itemp),.true.,s(1,ih),small)
    if (small) go to 40
    call evalf(ns,ips,s(1,ih),n,x,fe,nfe)
    if (fe .lt. fr) then
      fs(ih) = fe
    else
      call dcopy(ns,s(1,itemp),1_i4,s(1,ih),1_i4)
      fs(ih) = fr
    endif
  elseif (fr.lt.fs(is)) then
!
! accept reflected point
!
    call dcopy(ns,s(1,itemp),1_i4,s(1,ih),1_i4)
    fs(ih) = fr
  else
!
! contract
!
    if (fr.gt.fs(ih)) then
      call newpt(ns,-beta,s(1,icent),s(1,ih),.true.,s(1,itemp),small)
    else
      call newpt(ns,-beta,s(1,icent),s(1,itemp),.false.,dum,small)
    endif
    if (small) go to 40
    call evalf(ns,ips,s(1,itemp),n,x,fc,nfe)
    if (fc.lt.min(fr,fs(ih))) then
      call dcopy(ns,s(1,itemp),1_i4,s(1,ih),1_i4)
      fs(ih) = fc
    else
!
! shrink simplex
!
      do j = 1,npts
        if (j.ne.il) then
          call newpt(ns,-delta,s(1,il),s(1,j),.false.,dum,small)
          if (small) go to 40
          call evalf(ns,ips,s(1,j),n,x,fs(j),nfe)
        endif
      enddo
    endif
    updatc = .false.
  endif
  call order(npts,fs,il,is,ih)
!
! check termination
!
40 continue
  if (irepl .eq. 0) then
    fx = fs(il)
  else
    fx = sfbest
  endif
50 continue
  if (nfstop .gt. 0 .and. fx .le. sfstop .and.nfxe .ge. nfstop) then
    iflag = 2
  elseif (nfe .ge. maxnfe) then
    iflag = -1
  elseif (dist(ns,s(1,ih),s(1,il)) .le. tol .or.small) then
    iflag = 0
  else
    go to 20
  endif
!
! end main loop, return best point
!
  do i = 1,ns
    x(ips(i)) = s(i,il)
  enddo
!
  deallocate(dum)
  return
  end subroutine simplx
!
  subroutine sortd(n,xkey,ix)
!
  integer(i4) :: n
  integer(i4) :: ix(n)
  real(dp)    :: xkey(n)
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! sortd uses the shakersort method to sort an array of keys
! in decreasing order. The sort is performed implicitly by
! modifying a vector of indices.
!
! For nearly sorted arrays, sortd requires O(n) comparisons.
! for completely unsorted arrays, sortd requires O(n**2)
! comparisons and will be inefficient unless n is small.
!
! input
!
!   n      - number of components
!
!   xkey   - real(dp) vector of keys
!
!   ix     - integer vector of indices
!
! output
!
!   ix     - indices satisfy xkey(ix(i)) .ge. xkey(ix(i+1))
!            for i = 1,...,n-1
!
! local variables
!
  integer(i4) :: i,ifirst,ilast,iswap,ixi,ixip1
!
!-----------------------------------------------------------
!
  ifirst = 1
  iswap = 1
  ilast = n - 1
10 continue
  if (ifirst.le.ilast) then
    do i = ifirst,ilast
      ixi = ix(i)
      ixip1 = ix(i+1)
      if (xkey(ixi).lt.xkey(ixip1)) then
        ix(i) = ixip1
        ix(i+1) = ixi
        iswap = i
      endif
    enddo
    ilast = iswap - 1
    do i = ilast,ifirst,-1
      ixi = ix(i)
      ixip1 = ix(i+1)
      if (xkey(ixi).lt.xkey(ixip1)) then
        ix(i) = ixip1
        ix(i+1) = ixi
        iswap = i
      endif
    enddo
    ifirst = iswap + 1
    goto 10
  endif
  return
  end subroutine sortd
!
  subroutine start(n,x,step,ns,ips,s,small)
!
  integer(i4) :: n
  integer(i4) :: ns
  integer(i4) :: ips(n)
  real(dp)    :: x(n)
  real(dp)    :: step(n)
  real(dp)    :: s(ns,ns+3)
  logical     :: small
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! start creates the initial simplex for simplx minimization.
!
! input
!
!   n      - problem dimension
!
!   x      - current best point
!
!   step   - stepsizes for corresponding components of x
!
!   ns     - subspace dimension
!
!   ips    - permutation vector
!
!
! output
!
!   s      - first ns+1 columns contain initial simplex
!
!   small  - logical flag
!            = .true.  : coincident points
!            = .false. : otherwise
!
! local variables
!
  integer(i4) :: i,j
!
!-----------------------------------------------------------
!
  do i = 1,ns
    s(i,1) = x(ips(i))
  enddo
  do j = 2,ns+1
    call dcopy(ns,s(1,1),1_i4,s(1,j),1_i4)
    s(j-1,j) = s(j-1,1) + step(ips(j-1))
  enddo
!
! check for coincident points
!
  do j = 2,ns+1
    if (dble(s(j-1,j)).eq.dble(s(j-1,1))) goto 40
  enddo
  small = .false.
  return
!
! coincident points
!
40 continue
  small = .true.
  return
  end subroutine start
!
  subroutine subopt(n)
!
  integer(i4), intent(in) :: n
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! subopt sets options for subplx.
!
! input
!
!   n      - problem dimension
!
!-----------------------------------------------------------
!
!***********************************************************
! simplex method strategy parameters
!***********************************************************
!
! alpha  - reflection coefficient
!          alpha .gt. 0
!
  alpha = 1.0_dp
!
! beta   - contraction coefficient
!          0 .lt. beta .lt. 1
!
  beta = 0.5_dp
!
! gamma  - expansion coefficient
!          gamma .gt. 1
!
  gamma = 2.0_dp
!
! delta  - shrinkage (massive contraction) coefficient
!          0 .lt. delta .lt. 1
!
  delta = 0.5_dp
!
!***********************************************************
! subplex method strategy parameters
!***********************************************************
!
! psi    - simplex reduction coefficient
!          0 .lt. psi .lt. 1
!
  psi = 0.25_dp
!
! omega  - step reduction coefficient
!          0 .lt. omega .lt. 1
!
  omega = 0.1_dp
!
! nsmin and nsmax specify a range of subspace dimensions.
! In addition to satisfying  1 .le. nsmin .le. nsmax .le. n,
! nsmin and nsmax must be chosen so that n can be expressed
! as a sum of positive integers where each of these integers
! ns(i) satisfies   nsmin .le. ns(i) .ge. nsmax.
! Specifically,
!     nsmin*ceil(n/nsmax) .le. n   must be true.
!
! nsmin  - subspace dimension minimum
!
  nsmin = min(2_i4,n)
!
! nsmax  - subspace dimension maximum
!
  nsmax = min(5_i4,n)
!
!***********************************************************
! subplex method special cases
!***********************************************************
! nelder-mead simplex method with periodic restarts
!   nsmin = nsmax = n
!***********************************************************
! nelder-mead simplex method
!   nsmin = nsmax = n, psi = small positive
!***********************************************************
!
! irepl, ifxsw, and bonus deal with measurement replication.
! Objective functions subject to large amounts of noise can
! cause an optimization method to halt at a false optimum.
! An expensive solution to this problem is to evaluate f
! several times at each point and return the average (or max
! or min) of these trials as the function value.  subplx
! performs measurement replication only at the current best
! point. The longer a point is retained as best, the more
! accurate its function value becomes.
!
! The common variable nfxe contains the number of function
! evaluations at the current best point. fxstat contains the
! mean, max, min, and standard deviation of these trials.
!
! irepl  - measurement replication switch
! irepl  = 0, 1, or 2
!        = 0 : no measurement replication
!        = 1 : subplx performs measurement replication
!        = 2 : user performs measurement replication
!              (This is useful when optimizing on the mean,
!              max, or min of trials is insufficient. Common
!              variable initx is true for first function
!              evaluation. newx is true for first trial at
!              this point. The user uses subroutine fstats
!              within his objective function to maintain
!              fxstat. By monitoring newx, the user can tell
!              whether to return the function evaluation
!              (newx = .true.) or to use the new function
!              evaluation to refine the function evaluation
!              of the current best point (newx = .false.).
!              The common variable ftest gives the function
!              value that a new point must beat to be
!              considered the new best point.)
!
  irepl = 0
!
! ifxsw  - measurement replication optimization switch
! ifxsw  = 1, 2, or 3
!        = 1 : retain mean of trials as best function value
!        = 2 : retain max
!        = 3 : retain min
!
  ifxsw = 1
!
! Since the current best point will also be the most
! accurately evaluated point whenever irepl .gt. 0, a bonus
! should be added to the function value of the best point
! so that the best point is not replaced by a new point
! that only appears better because of noise.
! subplx uses bonus to determine how many multiples of
! fxstat(4) should be added as a bonus to the function
! evaluation. (The bonus is adjusted automatically by
! subplx when ifxsw or minf is changed.)
!
! bonus  - measurement replication bonus coefficient
!          bonus .ge. 0 (normally, bonus = 0 or 1)
!        = 0 : bonus not used
!        = 1 : bonus used
!
  bonus = 1.0_dp
!
! nfstop = 0 : f(x) is not tested against fstop
!        = 1 : if f(x) has reached fstop, subplx returns
!              iflag = 2
!        = 2 : (only valid when irepl .gt. 0)
!              if f(x) has reached fstop and
!              nfxe .gt. nfstop, subplx returns iflag = 2
!
  nfstop = 0
!
! fstop  - f target value
!          Its usage is determined by the value of nfstop.
  fstop = 0.0_dp
!
! minf   - logical switch
!        = .true.  : subplx performs minimization
!        = .false. : subplx performs maximization
!
  minf = .true.
  return
  end subroutine subopt
!
  subroutine evalf(ns,ips,xs,n,x,sfx,nfe)
!
  integer(i4), intent(in)    :: ns
  integer(i4), intent(in)    :: n
  integer(i4), intent(inout) :: nfe
  integer(i4), intent(in)    :: ips(*)
  real(dp),    intent(in)    :: xs(*)
  real(dp),    intent(out)   :: x(n)
  real(dp),    intent(out)   :: sfx
!
!                                         Coded by Tom Rowan
!                            Department of Computer Sciences
!                              University of Texas at Austin
!
! evalf evaluates the function f at a point defined by x
! with ns of its components replaced by those in xs.
!
! input
!
!   f      - user supplied function f(n,x) to be optimized
!
!   ns     - subspace dimension
!
!   ips    - permutation vector
!
!   xs     - double precision ns-vector to be mapped to x
!
!   n      - problem dimension
!
!   x      - double precision n-vector
!
!   nfe    - number of function evaluations
!
! output
!
!   sfx    - signed value of f evaluated at x
!
!   nfe    - incremented number of function evaluations
!
! local variables
!
  integer(i4) :: i
  real(dp)    :: fx
  logical     :: newbst
!
! subroutines and functions
!
  external fitfun
!
!-----------------------------------------------------------
!
  do i = 1,ns
    x(ips(i)) = xs(i)
  enddo
! Not needed unless used for monitoring
!  newx = (new.or.irepl.ne.2)
  call fitfun(n,x,fx)
!
  if (irepl.eq.0) then
    if (minf) then
      sfx = fx
    else
      sfx = -fx
    endif
  elseif (new) then
    if (minf) then
      sfx = fx
      newbst = (fx.lt.ftest)
    else
      sfx = -fx
      newbst = fx .gt. ftest
    endif
    if (initx.or.newbst) then
      if (irepl.eq.1) call fstats(fx,1_i4,.true.)
      ftest = fx
      sfbest = sfx
    endif
  else
    if (irepl .eq. 1) then
      call fstats(fx,1_i4,.false.)
      fx = fxstat(ifxsw)
    endif
    ftest = fx + fbonus*fxstat(4)
    if (minf) then
      sfx = ftest
      sfbest = fx
    else
      sfx = -ftest
      sfbest = -fx
    endif
  endif
  nfe = nfe + 1
  return
  end subroutine evalf

end module m_simplex
