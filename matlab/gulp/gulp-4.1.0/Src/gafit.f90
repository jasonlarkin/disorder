  subroutine gafit(xc,fsumsq,ifail)
!
!  Subroutine for performing fitting using the genetic algorithm
!  Allows a reasonable set of initial parameters to be obtained
!  for subsequent refinement by conventional means.
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
!   1/09 Integer datatypes all explicitly declared
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use fitting
  use gaconf
  use general
  use genetic
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                                    :: ifail
  real(dp)                                       :: fsumsq
  real(dp)                                       :: xc(*)
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ibestloc
  integer(i4)                                    :: ic
  integer(i4)                                    :: ilow
  integer(i4)                                    :: iworst
  integer(i4)                                    :: j
  integer(i4)                                    :: nbsl
  integer(i4)                                    :: status
  logical                                        :: lchanged
  logical                                        :: ldiff
  real(dp)                                       :: cputime
  real(dp)                                       :: fbest
  real(dp)                                       :: fhigh
  real(dp)                                       :: flow
  real(dp)                                       :: fworst
  real(dp)                                       :: pcross
  real(dp)                                       :: pmuta
  real(dp)                                       :: pts
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp)                                       :: tmax
  real(dp),    dimension(:),   allocatable       :: xctmp
!
!  Allocate local memory
!
  allocate(xctmp(nfit),stat=status)
  if (status/=0) call outofmemory('gafit','xctmp')
!******************************************
!  Initialise parameters not set by user  *
!******************************************
  pts = prob(1)
  pcross = prob(4)
  pmuta = prob(7)
  if (ngabest.gt.0) then
    if (maxfcal*ngacfg.lt.ngabest) ngabest=maxfcal*ngacfg
    do i = 1,ngabest
      fconf(2*ngacfg+i) = 1.0d20
    enddo
    fworst = 1.0d20
    iworst = 1
  endif
  if (ngacfg.eq.0) ngacfg = 10
  if (pts.eq.-1.0_dp) pts = 0.8_dp
  if (pcross.eq.-1.0_dp) pcross = 0.4_dp
!
!  Check array dimensions
!
  lchanged = .false.
  if (ngacfg.gt.maxgcfg) then
    maxgcfg = ngacfg
    lchanged = .true.
  endif
  if (ngabest.gt.maxbcfg) then
    maxbcfg = ngabest
    lchanged = .true.
  endif
  if (nfitt.gt.maxmvar) then
    maxmvar = nfitt
    lchanged = .true.
  endif
  if (lchanged) then
    call changemaxgbcfg
  endif
!
!  Set maximum and minimum limits
!
  call gamaxmin(xc,xmax,xmin,1_4)
  do i = 1,nfitt
    if (ndiscret(i).eq.0) ndiscret(i) = 6
  enddo
  tmax = 0.0_dp
!
!  Find binary string length, nbsl
!
  nbsl = 0
  do i = 1,nfitt
    nbsl = nbsl + ndiscret(i)
  enddo
  if (pmuta.eq.-1.0_dp) pmuta = 1.0_dp/nbsl
!
!  Output parameters for genetic algorithm
!
  if (ioproc) call outgapar(1_i4)
!*******************************************
!  Generate ngacfg random starting points  *
!*******************************************
  ldiff = .false.
  call gacreate(ngacfg,ngacfg,nfitt,ndiscret,xmin,xmax,iseed,ldiff)
  do i = 1,ngacfg
    do j = 1,nfitt
      xc(j) = xconf(j,i)
    enddo
    call fitfun(nfitt,xc,fsumsq)
    fconf(i) = fsumsq
  enddo
!****************************
!  Start of iterative loop  *
!****************************
  do ic = 1,maxfcal
    t1 = cputime()
!
!  Reproduction
!
    call gafrepro(ngacfg,nfitt,pts,iseed)
!
!  Crossover
!
    call gafcross(ngacfg,nfitt,ndiscret,nbsl,pcross,xc,xmin,xmax,iseed)
!
!  Mutation
!
    call gafmuta(ngacfg,nfitt,ndiscret,pmuta,xmin,xmax,iseed)
!
!  Evaluate sums of squares
!
    flow = 1.0d20
    fhigh = -1.0d20
    do i = 1,ngacfg
      do j = 1,nfitt
        xc(j) = xconf(j,i)
      enddo
      call fitfun(nfitt,xc,fsumsq)
      fconf(i) = fsumsq
      if (fsumsq.lt.flow) then
        flow = fsumsq
        ilow = i
      endif
      if (fsumsq.gt.fhigh) then
        fhigh = fsumsq
      endif
    enddo
!*********************************
!  Store optimal configurations  *
!*********************************
    if (ngabest.gt.0) then
      do i = 1,ngacfg
        if (fconf(i).lt.fworst) then
!
!  Switch in new configuration
!
          fconf(2*ngacfg+iworst)=fconf(i)
          do j = 1,nfitt
            xbest(j,iworst) = xconf(j,i)
          enddo
!
!  Find new worst configuration
!
          fworst = -1.0d20
          do j = 1,ngabest
            if (fconf(2*ngacfg+j).gt.fworst) then
              fworst = fconf(2*ngacfg+j)
              iworst = j
            endif
          enddo
        endif
      enddo
    endif
!
!  Check convergence
!
! ????? How ??????
!
!  Check remaining cputime
!
    t2 = cputime()
    tdel = t2 - t1
    if (tdel.gt.tmax) tmax = tdel
    if (ioproc) then
      write(ioout,'(''  Cycle: '',i5,'' Sum squares: Min '',f14.6,'' Max '',f14.6,'' CPU:'',f9.3)') &
        ic,flow,fhigh,t2
      call gflush(ioout)
    endif
    if (timmax.gt.0.0_dp) then
      if ((timmax-t2).lt.tmax) then
        ifail=-1
        goto 10
      endif
    endif
  enddo
!**************************
!  End of iterative loop  *
!**************************
  ifail = 2
10 continue
  if (ngabest.gt.0) then
!
!  Find best point and move to xc
!
    fbest = 1.0d20
    ibestloc = 1
    do i = 1,ngabest
      if (fconf(2*ngacfg+i).lt.fbest) then
        fbest = fconf(2*ngacfg+i)
        ibestloc = i
      endif
    enddo
    do i = 1,nfitt
      xc(i) = xbest(i,ibestloc)
    enddo
    fsumsq = fbest
  else
    do i = 1,nfitt
      xc(i) = xconf(i,ilow)
    enddo
    fsumsq = flow
  endif
!
!  Convert xmin and xmax to input values
!
  do i = 1,nfitt
    xmax(i) = xmax(i)*scale(i)
    if (xmin(i).ne.0.0_dp) then
      xmin(i) = xmin(i)*scale(i)
    endif
  enddo
  if (pmuta.eq.1.0_dp/nbsl) pmuta = -1.0_dp
!**********************
!  Expand parameters  *
!**********************
  if (nfcon.gt.0) then
    if (ngabest.gt.0) then
      do i = 1,ngabest
        do j = 1,nfitt
          xctmp(nfitptr(j)) = xbest(j,i)
        enddo
        call fitcon(xctmp)
        do j = 1,nfit
          xbest(j,i) = xctmp(j)
        enddo
      enddo
    else
      do i = 1,ngacfg
        do j = 1,nfitt
          xctmp(nfitptr(j)) = xconf(j,i)
        enddo
        call fitcon(xctmp)
        do j = 1,nfit
          xconf(j,i) = xctmp(j)
        enddo
      enddo
    endif
  endif
!
!  Output
!
  if (ioproc) call outgafit
!
!  Free local memory
!
  deallocate(xctmp,stat=status)
  if (status/=0) call deallocate_error('gafit','xctmp')
  return
  end
!
  subroutine gafrepro(ngacfg,nvar,pts,iseed)
!
!  Reproduction step of genetic algorithm by tournament selection
!  with a probability pts.
!
!   1/08 random -> GULP_random
!
  use datatypes
  use gaconf, only : fconf, xconf
  implicit none
!
!  Passed variables
!
  integer(i4)         :: iseed
  integer(i4)         :: ngacfg
  integer(i4)         :: nvar
  real(dp)            :: pts
!
!  Local variables
!
  integer(i4)         :: i
  integer(i4)         :: j
  integer(i4)         :: n1
  integer(i4)         :: n2
  integer(i4)         :: nsel
  real(dp)            :: GULP_random
  real(dp)            :: rn1
  real(dp)            :: rn2
  real(dp)            :: rnts
!
!  Loop over number of configurations
!
  do i = 1,ngacfg
!
!  Select two different configurations at random
!
    rn1 = GULP_random(iseed,1_i4)
    n1 = rn1*dble(ngacfg) + 1
    n2 = n1
    do while (n2.eq.n1)
      rn2 = GULP_random(iseed,1_i4)
      n2 = rn2*dble(ngacfg) + 1
    enddo
!
!  Select which one is to pass through to next cycle
!
    rnts = GULP_random(iseed,1_i4)
    if (rnts.lt.pts) then
      if (fconf(n1).lt.fconf(n2)) then
        nsel = n1
      else
        nsel = n2
      endif
    else
      if (fconf(n1).gt.fconf(n2)) then
        nsel = n1
      else
        nsel = n2
      endif
    endif
!
!  Store selected configuration
!
    do j = 1,nvar
      xconf(j,i+ngacfg) = xconf(j,nsel)
    enddo
    fconf(i+ngacfg) = fconf(nsel)
  enddo
!
!  Overwrite current configurations with new ones
!
  do i = 1,ngacfg
    do j = 1,nvar
      xconf(j,i) = xconf(j,i+ngacfg)
    enddo
    fconf(i) = fconf(i+ngacfg)
  enddo
  return
  end
!
  subroutine gafcross(ngacfg,nvar,ndiscret,nbsl,pcross,xc,xmin,xmax,iseed)
!
!  Perform cross over step in genetic algorithm with probability pcross.
!  Called from gafit and gaopt.
!
!   1/08 random -> GULP_random
!
  use datatypes
  use gaconf, only : xconf
  implicit none
!
!  Passed variables
!
  integer(i4)                              :: iseed
  integer(i4)                              :: nbsl
  integer(i4)                              :: ndiscret(*)
  integer(i4)                              :: ngacfg
  integer(i4)                              :: nvar
  real(dp)                                 :: pcross
  real(dp)                                 :: xc(*)
  real(dp)                                 :: xmax(*)
  real(dp)                                 :: xmin(*)
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: ibin1(30)
  integer(i4)                              :: ibin2(30)
  integer(i4)                              :: ileft
  integer(i4)                              :: ind
  integer(i4)                              :: inds
  integer(i4)                              :: itemp(30)
  integer(i4)                              :: j
  integer(i4)                              :: n1
  integer(i4)                              :: n2
  integer(i4)                              :: ncp
  integer(i4)                              :: ncv
  integer(i4)                              :: nd
  integer(i4)                              :: nleft
  integer(i4)                              :: npair
  integer(i4)                              :: nsel1
  integer(i4)                              :: nsel2
  integer(i4)                              :: nval1
  integer(i4)                              :: nval2
  integer(i4)                              :: status
  logical                                  :: lfound
  logical, dimension(:), allocatable       :: lpair
  real(dp)                                 :: GULP_random
  real(dp)                                 :: rn1
  real(dp)                                 :: rn2
  real(dp)                                 :: rnc
  real(dp)                                 :: rncp
  real(dp)                                 :: rnd
  real(dp)                                 :: xc1
  real(dp)                                 :: xc2
  real(dp)                                 :: xdiff
  real(dp)                                 :: xint
  real(dp)                                 :: xmi
!
!  Initialisation
!
  allocate(lpair(ngacfg),stat=status)
  if (status/=0) call outofmemory('gafcross','lpair')
  do i = 1,ngacfg
    lpair(i) = .false.
  enddo
  npair = ngacfg/2
  nleft = ngacfg + 2
!********************
!  Loop over pairs  *
!********************
  do i = 1,npair
!
!  Select two configurations, nsel1 and nsel2, to pair
!
    nleft = nleft - 2
    rn1 = GULP_random(iseed,1_i4)
    rn2 = GULP_random(iseed,1_i4)
    n1  = rn1*dble(nleft) + 1
    n2  = rn2*dble(nleft-1) + 1
    ind = 0
    do j = 1,ngacfg
      if (.not.lpair(j)) then
        ind = ind + 1
        if (n1.eq.ind) then
          nsel1 = j
          lpair(j) = .true.
        endif
      endif
    enddo
    ind = 0
    do j = 1,ngacfg
      if (.not.lpair(j)) then
        ind = ind + 1
        if (n2.eq.ind) then
          nsel2 = j
          lpair(j) = .true.
        endif
      endif
    enddo
!
!  Decide whether to crossover
!
    rnc = GULP_random(iseed,1_i4)
    if (rnc.le.pcross) then
!
!  Select crossover point
!
      rncp = GULP_random(iseed,1_i4)
      ncp = rncp*dble(nbsl) + 1
!*******************
!  Make crossover  *
!*******************
      ind = 0
      lfound = .false.
      j = 0
!
!  Find variable which is split by crossover
!
      do while (j.lt.nvar.and..not.lfound)
        j = j + 1
        ind = ind + ndiscret(j)
        if (ind.gt.ncp) lfound = .true.
      enddo
      ncv = j
      if (ncv.gt.1) then
!
!  Exchange crossed parts
!
        do j = 1,ncv-1
          xc(j) = xconf(j,nsel1)
          xconf(j,nsel1) = xconf(j,nsel2)
          xconf(j,nsel2) = xc(j)
        enddo
      endif
!
!  Split variable crossover
!
      nd = ndiscret(ncv)
      rnd = 2.0_dp**nd - 1.0_dp
      inds = ind - nd
      ileft = ncp - inds
      xmi = xmin(j)
      xdiff = xmax(j) - xmi
      xint = xdiff/rnd
      xc1 = xconf(ncv,nsel1)
      xc2 = xconf(ncv,nsel2)
      nval1 = nint((xc1-xmi)/xint) + 1
      nval2 = nint((xc2-xmi)/xint) + 1
      call inttobin(nval1,ibin1,nd)
      call inttobin(nval2,ibin2,nd)
      do j = nd,nd-ileft+1,-1
        itemp(j) = ibin1(j)
        ibin1(j) = ibin2(j)
        ibin2(j) = itemp(j)
      enddo
      call bintoint(nval1,ibin1,nd)
      call bintoint(nval2,ibin2,nd)
      xconf(ncv,nsel1) = xmi + xint*(nval1-1)
      xconf(ncv,nsel2) = xmi + xint*(nval2-1)
    endif
!
!  End loop over pairs
!
  enddo
!
!  Free local memory
!
  deallocate(lpair,stat=status)
  if (status/=0) call deallocate_error('gafcross','lpair')
!
  return
  end
!
  subroutine gafmuta(ngacfg,nvar,ndiscret,pmuta,xmin,xmax,iseed)
!
!  Perform mutation in genetic algorithm with probability pmuta.
!  Called from gafit and gaopt.
!
!   1/08 random -> GULP_random
!
  use datatypes
  use gaconf, only : xconf
  implicit none
!
!  Passed variables
!
  integer(i4)                              :: iseed
  integer(i4)                              :: ndiscret(*)
  integer(i4)                              :: ngacfg
  integer(i4)                              :: nvar
  real(dp)                                 :: pmuta
  real(dp)                                 :: xmax(*)
  real(dp)                                 :: xmin(*)
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: ibin(30)
  integer(i4)                              :: j
  integer(i4)                              :: k
  integer(i4)                              :: nd
  integer(i4)                              :: nval1
  logical                                  :: lmuta(30)
  logical                                  :: ldomuta
  real(dp)                                 :: GULP_random
  real(dp)                                 :: rn
  real(dp)                                 :: rnd
  real(dp)                                 :: xc1
  real(dp)                                 :: xdiff
  real(dp)                                 :: xint
  real(dp)                                 :: xmi
!
!  Loop over configurations
!
  do i = 1,ngacfg
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
        rnd = 2.0_dp**nd - 1.0_dp
        xmi = xmin(j)
        xdiff = xmax(j) - xmi
        xint = xdiff/rnd
        xc1 = xconf(j,i)
        nval1 = int((xc1-xmi)/xint) + 1
        call inttobin(nval1,ibin,nd)
        do k = 1,nd
          if (lmuta(k)) then
            if (ibin(k).eq.1) then
              ibin(k) = 0
            else
              ibin(k) = 1
            endif
          endif
        enddo
        call bintoint(nval1,ibin,nd)
        xconf(j,i) = xmi + xint*(nval1-1)
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
