  subroutine gaopt(nvar,ifail,xc,fc)
!
!  Subroutine for performing optimisation using the genetic algorithm
!  Performs a search for reasonable local minima.
!
!  ngacfg    = no. of configurations to be used in ga method
!  xmin      = lower bound for parameter
!  xmax      = upper bound for parameter
!  ngabest   = no. of best configurations to store
!  nbsl      = total binary number length
!  ndiscret  = discretisation interval as a power of two
!  pts       = probability for tournament selection
!  pcross    = probability for crossover
!  pmuta     = probability for mutation
!  fconf     = value of trial function for configuration
!
!  Stored configurations are overlaid on tmat in common second.
!
!  07/08/97 call to costfn hidden within funct.f (smw)
!  07/08/97 common gaflags (logicals) added (smw)
!  30/09/97 all gaspp features finally installed (smw)
!  06/10/97 prob(9) added for genetic annealing (smw)
!  02/06/98 create gasubs.f to make main loop clearer (smw)
!  09/06/98 allow some sort of restart file (smw)
!  29/01/99 generate movie file (smw)
!  27/07/00 modified for multiple configs (smw)
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
!  Scott Woodley, Royal Institution of G.B., June 1997
!  Julian Gale, NRI, Curtin University, July 2005
!
  use control
  use gaconf
  use general
  use genetic
  use iochannels
  use optimisation
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: ifail
  integer(i4) :: nvar
  real(dp)    :: fc
  real(dp)    :: xc(*)
!
!  Local variables
!
  integer(i4) :: ibest
  integer(i4) :: ic
  integer(i4) :: iflag
  integer(i4) :: i
  integer(i4) :: ij
  integer(i4) :: iworst
  integer(i4) :: j
  integer(i4) :: ji
  integer(i4) :: k
  integer(i4) :: md
  integer(i4) :: nbest
  integer(i4) :: nbsl
  integer(i4) :: nc
  integer(i4) :: ne
  integer(i4) :: pb
  integer(i4) :: same
  logical     :: lann
  logical     :: ldiff
  logical     :: lgrid
  real(dp)    :: cputime
  real(dp)    :: f
  real(dp)    :: fhigh
  real(dp)    :: flow
  real(dp)    :: gcd
  real(dp)    :: pcross
  real(dp)    :: pmuta
  real(dp)    :: pts
  real(dp)    :: t1
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
!  Set genetic annealing logical flag + probabilities
!
  if (ndi.eq.0) then
    if (ioproc) write(ioout,'(''Expected to change parameters after 0 iterations'')')
    call stopnow('gaopt')
  endif
  if (nd.eq.ndmax.and.ndi.ne.-1) then
    same = - ndi
    lann = .true.
  else
    same = - 1
    lann = .false.
  endif
  pts = prob(1)
  pcross = prob(4)
  pmuta = prob(7)
!
!  Set grid changing flag (lgrid = T => change grid)
!
  if (nd.ne.ndmax) then
    lgrid = .true.
    md = nd
  else
    lgrid = .false.
  endif
!
!  Initialise parameters not set by user
!
  nc = 0
  pb = 0
  nbest = 0
  f = 1.0d20
  if (ngabest.gt.0) then
    do i = 1,ngabest
      fconf(mgacfg+i) = 1.0d20
    enddo
    ibest = mgacfg + 1
    iworst = mgacfg + 1
  endif
  if (lgabest) nbest = ngabest
!
!  Set maximum and minimum limits
!
  call gamaxmin(xc,xmax,xmin,2_i4)
!
!  Output parameters for genetic algorithm
!
  if (ioproc) call outgapar(2_i4)
!
!  Create grid and find DNA binary string length
!
  nd = nd - 1
  call gagrid(nd,ndiscret,nvar,nbsl,pmuta,lann)
!
!  Read-in stored configurations if requested
!
  ne = 0
  call gahard(ne,nvar,-1_i4)
!
!  Generate ngacfg random starting points
!
  ldiff = .false.
  call gacreate(ngacfg-ne,ngacfg,nvar,ndiscret,xmin,xmax,iseed,ldiff)
!
!  Now evaluate the cost function for each of the new configurations
!
  iflag = 0
  call gaeval(1_i4,ngacfg,iflag,nvar,xc)
!
!  Start of iterative loop
!
  do ic = 1,maxgacyc
    t1 = cputime()
!
!  Reproduction
!
    call garepro(ngacfg,mgacfg,nvar,lgaexpw,ithbest,maxbcfg,pts,iseed,udif,nspar)
!
!  Crossover
!
    call gacross(ngacfg,mgacfg,nvar,ndiscret,nbsl,pcross,xc,xmin,xmax,iseed,l2pxo,nspar)
!
!  Mutation
!
    call gamutat(ngacfg,mgacfg,nvar,ndiscret,pmuta,xmin,xmax,iseed,nspar,same,ithbest,maxbcfg,nc)
!
!  Local optimise if minimum stuck
!
    if (nc.gt.0) then
!1.1      lgaconjg = .true.
!1.1      call precj(nc,nvar,xc,ithbest,ngacfg,mgacfg)
!1.1      lgaconjg = .false.
      nc = 0
    endif
!
!  Generation of new parents
!
    call gaexpd(ngacfg,mgacfg,nvar,nspar,maxbcfg,ithbest,xc,gcd,xmin,xmax,ndiscret,iseed)
!
!  Evaluate best/worst cost function or energy of system
!
    flow = fconf(ithbest(1))
    fhigh = fconf(ithbest(2))
!
!  Conjugate Gradient Minimiser for odd top ten
!  implemented on certain iteration numbers
!
!1.1    lgaconjg = (mod(ic,ngacjg).eq.0.and.ngacjg.ne.-1)
!1.1    if (lgaconjg) then
!1.1      call precj(10,nvar,xc,ithbest,ngacfg,0)
!1.1    endif
!
!  Store optimal configurations if requested
!
    if (ngabset.gt.0) then
      pb = 1
      if (mod(ic,ngabset).eq.0)then
        call gasort(ithbest,ngabest,fconf,ngacfg,udif,1_i4)
        if (lgabest) then
          call gastore(ngabest,ibest,iworst,ithbest,mgacfg,nvar)
        else
          do ij = 1,ngabest
            nbest = nbest + 1
            fconf(mgacfg+nbest) = fconf(ithbest(ij))
            do ji = 1,nvar
             xbest(ji,nbest) = xconf(ji,ithbest(ij))
            enddo
          enddo
        endif
      endif
    endif
!
!  Change probabilities if simulated annealing required
!
    if (lann) then
      call gaprob(same,pts,pcross,pmuta,prob,nbsl,ndi,lgaexpw,lann)
    endif
!
!  Change grid size (DNA length) and set pmuta to 1.0_dp/nbsl
!
    if (mod(ic,ndi).eq.0.and.nd.lt.ndmax) then
      call gagrid(nd,ndiscret,nvar,nbsl,pmuta,lann)
    endif
!
!  Check whether or not global minimiser stuck in a local minimum
!
    if (nd.eq.ndmax.and..not.lann) then
      call gastuck(same,f,flow,prob,md,nd,pmuta,nbsl,lann,lgrid)
    endif
!
!  Check remaining cputime and output info
!
    if (ioproc) call gatime(t1,ic,nbest,mgacfg,flow,fhigh,pb,ifail)
    if (ifail.eq.-1) goto 10
  enddo
!
!  End of iterative loop
!
  ifail = 5
10 continue

  if (ngabset.eq.0) then
    call gasort(ithbest,ngabest,fconf,ngacfg,udif,1_i4)
    do ic = 1,ngabest
      k = ithbest(ic)
      fc = fconf(k)
      fconf(mgacfg+ic) = fc
      do j = 1,nvar
        xbest(j,ic) = xconf(j,k)
      enddo
    enddo
  endif
!
!  Output
!
  if (ngabest.gt.0) then
    call gahard(ngabest,nvar,1_i4)
    call outgaopt
  else
    call gahard(ngacfg,nvar,1_i4)
    call outgaopt
  endif
!
!  Reset lgacost flag such that energy can now be minimised
!
  lgacost = .false.
  lgaconjg = .false.
  nd = md
!
  return
  end
