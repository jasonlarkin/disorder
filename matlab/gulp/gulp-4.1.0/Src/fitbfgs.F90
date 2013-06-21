  subroutine fitbfgs(xc,fc,gc,xtol,ftol,gtol,maxcal,hessian,ifail)
!
!  Minimises the sum of squares during a fit using the BFGS
!  update of the hessian matrix
!
!   5/02 Call to fitgrad substituted for fitfunct
!   6/05 Deallocation order corrected
!   5/06 fcalc saved from first call for later comparison
!  12/07 Unused variables removed
!  10/12 Control C handling added
!   3/13 Call to csignal wrapped with ifdef to make it optional
!   5/13 Control C trap for optimisation and fitting separated
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use control
  use dump
  use fitting
  use general
  use interupt,    only : controlC_fit, sigint
  use iochannels
  use observables, only : fcalc, fcalcoriginal, nobs
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                               :: ifail
  integer(i4)                               :: maxcal
  real(dp)                                  :: fc
  real(dp)                                  :: ftol
  real(dp)                                  :: gtol
  real(dp)                                  :: xtol
  real(dp)                                  :: gc(*)
  real(dp)                                  :: xc(*)
  real(dp)                                  :: hessian(*)
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: iflag
  integer(i4)                               :: ihdim
  integer(i4)                               :: ii
  integer(i4)                               :: irem
  integer(i4)                               :: irepet
  integer(i4)                               :: itry1
  integer(i4)                               :: j
  integer(i4)                               :: jcyc
  integer(i4)                               :: jnrst
  integer(i4)                               :: k
  integer(i4)                               :: lnstop
  integer(i4)                               :: maxline
  integer(i4)                               :: ncount
  integer(i4)                               :: nleft
  integer(i4)                               :: nline
  integer(i4)                               :: ntest
  integer(i4)                               :: status
  logical                                   :: dfp
  logical                                   :: lfrst
  logical                                   :: lonly
  logical                                   :: okf
  real(dp)                                  :: absmin
  real(dp)                                  :: alpha
  real(dp)                                  :: beta
  real(dp)                                  :: bsmvf
  real(dp)                                  :: cncadd
  real(dp)                                  :: cos
  real(dp)                                  :: cputime
  real(dp)                                  :: ddot
  real(dp)                                  :: delhof
  real(dp)                                  :: dott
  real(dp)                                  :: drop
  real(dp)                                  :: einc
  real(dp)                                  :: frepf
  real(dp)                                  :: funct1
  real(dp), dimension(:), allocatable       :: gg
  real(dp)                                  :: ggi
  real(dp), dimension(:), allocatable       :: glast
  real(dp)                                  :: gnorm
  real(dp), dimension(:), allocatable       :: gvar
  real(dp)                                  :: pnlast
  real(dp)                                  :: pnorm
  real(dp), dimension(:), allocatable       :: pvect
  real(dp)                                  :: rootv
  real(dp)                                  :: rst
  real(dp)                                  :: rsy
  real(dp)                                  :: ryhy
  real(dp)                                  :: smval
  real(dp)                                  :: sy
  real(dp)                                  :: t1
  real(dp)                                  :: t2
  real(dp)                                  :: tf
  real(dp)                                  :: time1
  real(dp)                                  :: tolerf
  real(dp)                                  :: ttdel
  real(dp)                                  :: tx
  real(dp), dimension(:), allocatable       :: xlast
  real(dp)                                  :: xn
  real(dp), dimension(:), allocatable       :: xvar
  real(dp)                                  :: xvari
  real(dp)                                  :: yhy
#ifdef CTRLC
  external                                  :: trap_sigint_fit
!
!  Call sig handler
!
  call csignal(sigint,trap_sigint_fit)
#endif
!
!  Allocate local memory
!
  allocate(gg(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','gg')
  allocate(glast(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','glast')
  allocate(gvar(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','gvar')
  allocate(pvect(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','pvect')
  allocate(xlast(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','xlast')
  allocate(xvar(nfitt),stat=status)
  if (status/=0) call outofmemory('fitbfgs','xvar')
!
!  Initialisation
!
  lonly = (index(keyword,'okf').eq.0)
  lfrst = .true.
  ttdel = 0.0_dp
  rst  = 0.05_dp
  tdel = 0.06_dp
!
!  Minimum energy decrease to continue and max no. of attempts
!
  einc = ftol
  maxline = 6
!
!  Option for DFP optimiser - not default
!
  dfp = index(keyword,'dfp').ne.0
!
!  Remaining tolerances
!
  rootv = sqrt(nfitt+1.d-5)
  delhof = ftol
  tolerf = 0.001_dp
!
!  Set to large arbitary value
!
  drop  = 1.d15
  frepf = 1.d15
!
!  Some final constants
!
  ihdim = (nfitt*(nfitt+1))/2
  cncadd = 1.0_dp/rootv
  if (cncadd.gt.0.15_dp) cncadd = 0.15_dp
!
!  End of initialisation part 1
!
!  Initialise variables
!
  absmin = 1.0d6
  itry1 = 0
  jcyc = 0
  lnstop = 1
  irepet = 1
  alpha = 1.0_dp
  pnorm = 0.25_dp
  jnrst = 0
  cos = 0.0_dp
  ncount = 1
!
!  Calculate initial function, gradients and necessary second derivatives
!
  if (lunit) then
    iflag = 1
    lfrst = .false.
  else
    iflag = 2
    do i = 1,ihdim
      hessian(i) = 0.0_dp
    enddo
  endif
  call fitfunct(iflag,nfitt,xc,fc,gc,hessian)
  gnorm = sqrt(ddot(nfitt,gc,1_i4,gc,1_i4))
!
!  Copy original fit quality data to saved array for later use
!
  fcalcoriginal(1:nobs) = fcalc(1:nobs)
!
!  Print out initial values
!
  time1 = cputime()
  time1 = time1 - time0
  if (ioproc) then
    write(ioout,'(''  Cycle: '',i6,''  Sum sqs: '',f14.6,''  Gnorm:'',f14.6,''  CPU:'',f9.3)') &
      jcyc,fc,gnorm,time1
    write(ioout,'(''  ** Hessian calculated **'')')
    call gflush(ioout)
  endif
!
  do i = 1,nfitt
    glast(i) = gc(i)
  enddo
  if (gnorm.lt.gtol) then
!
!  Gradient is already low enough
!
    ifail = 0
    goto 999
  endif
!
!  If number of cycles requested is zero then go straight to end
!
  if (maxcal.eq.0) then
    ifail = 2
    goto 999
  endif
!****************************
!  Start of iteration cycle *
!****************************
10 continue
  jcyc  = jcyc  + 1
  jnrst = jnrst + 1
  t1 = cputime()
  if (lnstop.eq.1.or.(cos.le.rst).or.(jnrst.ge.nfupdate)) then
!
!  Generate hessian
!
    if (.not.lfrst) hessian(1:ihdim) = 0.0_dp
!
!  Estimate Hessian
!
    if (lunit) then
      ii = 0
      do i = 1,nfitt
        ii = ii + i
        hessian(ii) = 1.0_dp
      enddo
    else
      if (.not.lfrst) then
        iflag = 2
        call fitfunct(iflag,nfitt,xc,funct1,gc,hessian)
        if (ioproc) then
          write(ioout,'(''  ** Hessian calculated **'')')
        endif
      endif
      lfrst = .false.
    endif
!
    ncount = ncount + 1
    jnrst = 0
  else
!*******************
!  Update Hessian  *
!*******************
    do i = 1,nfitt
      xvar(i) = xc(i) - xlast(i)
      gvar(i) = gc(i) - glast(i)
    enddo
    call nrstep(gg,hessian,gvar,nfitt,1_i4)
    yhy = ddot(nfitt,gg,1_i4,gvar,1_i4)
    sy  = ddot(nfitt,xvar,1_i4,gvar,1_i4)
    if (abs(sy).lt.1.0d-10) sy = sign(1.0d-10,sy)
    k = 0
    rsy = 1.0_dp/sy
!
!  Update using the DFP scheme
!
    if (dfp) then
      ryhy = 1.0_dp/yhy
      do i = 1,nfitt
        xvari = xvar(i)*rsy
        ggi = gg(i)*ryhy
        do j = 1,i
          hessian(k+j) = hessian(k+j) + xvar(j)*xvari - gg(j)*ggi
        enddo
        k = k + i
      enddo
!
!  Update using the BFGS scheme
!
    else
      yhy = 1.0_dp + yhy*rsy
      do i = 1,nfitt
        xvari = xvar(i)*rsy
        ggi = gg(i)*rsy
        do j = 1,i
          hessian(k+j) = hessian(k+j) - gg(j)*xvari - xvar(j)*ggi + yhy*xvar(j)*xvari
        enddo
        k = k + i
      enddo
    endif
  endif
!
!  Establish new search direction
!
  pnlast = pnorm
  call nrstep(pvect,hessian,gc,nfitt,1_i4)
  pnorm = sqrt(ddot(nfitt,pvect,1_i4,pvect,1_i4))
  if (pnorm.gt.1.5_dp*pnlast) then
!
!  Trim pvect back
!
    do i = 1,nfitt
      pvect(i) = pvect(i)*1.5_dp*pnlast/pnorm
    enddo
    pnorm = 1.5_dp*pnlast
  endif
  dott = - ddot(nfitt,pvect,1_i4,gc,1_i4)
  do i = 1,nfitt
    pvect(i) = - pvect(i)
  enddo
  lnstop = 0
  if (pnorm.lt.1.0d-12) then
    cos = 1.0_dp
  else
    cos = - dott/(pnorm*gnorm)
    alpha = alpha*pnlast/pnorm
  endif
  do i = 1,nfitt
    glast(i) = gc(i)
    xlast(i) = xc(i)
  enddo
  if (jnrst.eq.0) alpha = 1.0_dp
  drop = abs(alpha*dott)
  if (jnrst.ne.0.and.drop.lt.delhof) then
!
!   Herbert's Test: The predicted drop in energy is less than delhof
!   if passed, call funct to get a good set of eigenvectors, then exit
!
    ifail = 0
    goto 999
  endif
  beta = alpha
  smval = fc
  okf = .false.
  call fitmin(xc,alpha,pvect,nfitt,fc,okf)
!
!  Linmin does not generate any derivatives, therefore funct must be
!  called to end the line search
!
  iflag = 1
  call fitfunct(iflag,nfitt,xc,fc,gc,hessian)
  gnorm = sqrt(ddot(nfitt,gc,1_i4,gc,1_i4))
  ncount = ncount + 1
  if (.not.okf) then
    lnstop = 1
    fc = smval
    alpha = beta
    do i = 1,nfitt
      gc(i) = glast(i)
      xc(i) = xlast(i)
    enddo
    if (jnrst.eq.0)then
      ifail = 3
      goto 999
    endif
    cos = 0.0_dp
    goto 470
  endif
  xn = sqrt(ddot(nfitt,xc,1_i4,xc,1_i4))
  tx = abs(alpha*pnorm)
  if (xn.ne.0.0_dp) tx = tx/xn
  tf = abs(smval - fc)
  if (abs(absmin-smval).lt.1.d-7) then
    itry1 = itry1 + 1
    if (itry1.gt.10) then
      ifail = 3
      goto 460
    endif
  else
    itry1 = 0
    absmin = smval
  endif
  if (tx.le.xtol) then
    ifail = 0
    goto 400
  endif
  if (tf.le.tolerf) then
    ifail = 0
    goto 400
  endif
  if (gnorm.le.gtol*rootv) then
    ifail = 0
    goto 400
  endif
  goto 470
400 do i = 1,nfitt
    if (abs(gc(i)).gt.gtol) then
      irepet = irepet + 1
      if (irepet.gt.1) goto 410
      frepf = fc
      cos = 0.0_dp
410     ifail = 3
      if (abs(fc-frepf).gt.einc) irepet = 0
      if (irepet.gt.maxline) then
        ifail = 3
        goto 999
      else
        goto 470
      endif
    endif
  enddo
  ifail = 0
460 continue
  goto 999
!
!  All tests have failed, we need to do another cycle.
!
470 continue
  bsmvf = abs(smval-fc)
  if (bsmvf.gt.100.0_dp) cos = 0.0_dp
!
!  End of iteration loop, everything is still O.K. so go to
!  next iteration, if there is enough time left.
!
  time1 = cputime()
  time1 = time1 - time0
  if (ioproc) then
    write(ioout,'(''  Cycle: '',i6,''  Sum sqs: '',f14.6,''  Gnorm:'',f14.6,''  CPU:'',f9.3)') &
      jcyc,fc,gnorm,time1
  endif
  irem = jcyc/ncycp
  irem = jcyc - ncycp*irem
  if (irem.eq.0.and.ioproc) then
!
!  Output current parameters
!
    write(ioout,'(/,''  Current parameters :'')')
    nline = nfitt/6 + 1
    ntest = 0
    do i = 1,nline-1
      write(ioout,'(6f13.5)')(xc(nfitptr(ntest+j))*scale(nfitptr(ntest+j)),j = 1,6)
      ntest = ntest + 6
    enddo
    nleft = nfitt - 6*(nline-1)
    write(ioout,'(6f13.5)')(xc(nfitptr(ntest+j))*scale(nfitptr(ntest+j)),j = 1,nleft)
    write(ioout,'(/)')
    call gflush(ioout)
  endif
!
!  Write out intermediate dumpfile if necessary
!
  ntest = jcyc/ncycd
  ntest = jcyc - ntest*ncycd
  if (ntest.eq.0.and.idump.gt.0.and.ioproc) then
    call dumpdur(idump,jcyc)
  endif
  if (lonly.and.okf) cos = 1.0_dp
!
!  Check time
!
  t2 = cputime()
  tdel = t2 - t1
  if (tdel.gt.ttdel) ttdel = tdel
  if (timmax.gt.0.0_dp) then
    if (t2.gt.(timmax-ttdel)) iflag = - 1
  endif
  if (iflag.ge.0.and.jcyc.lt.maxcal.and..not.controlC_fit) goto 10
  if (iflag.lt.0) then
    ifail = - 1
  else
    ifail = 2
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(xvar,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','xvar')
  deallocate(xlast,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','xlast')
  deallocate(pvect,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','pvect')
  deallocate(gvar,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','gvar')
  deallocate(glast,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','glast')
  deallocate(gg,stat=status)
  if (status/=0) call deallocate_error('fitbfgs','gg')
!
  return
  end
