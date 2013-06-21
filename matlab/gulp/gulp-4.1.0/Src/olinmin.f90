  subroutine olinmin(xparam,step,pvect,nvar,nmin,funct1,okf,grad,imode)
!*********************************************************************
!
!  olinmin does a line minimisation.
!
!  on input:  xparam = starting coordinate of search.
!             step   = step size for initiating search.
!             pvect  = direction of search.
!             nvar   = number of variables in xparam.
!             funct1 = initial value of the function to be minimized.
!
!  on output: xparam = coordinate of minimum of function.
!             step   = new step size, used in next call of linmin.
!             pvect  = unchanged, or negated, depending on step.
!             funct1 = final, minimum value of the function.
!             okf    = true if linmin improved funct, false otherwise.
!
!  imode = 1 => bulk calculation
!        = 2 => defect calculation
!
!   3/07 Renamed from linmin to olinmin
!  10/11 Precision increased by default for reaxFF
!
!**********************************************************************
  use control
  use iochannels
  use molecule
  use optimisation
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
  integer(i4)                                  :: nmin
  integer(i4)                                  :: nvar
  logical                                      :: okf
  real(dp)                                     :: funct1
  real(dp)                                     :: grad(*)
  real(dp)                                     :: pvect(*)
  real(dp)                                     :: step
  real(dp)                                     :: xparam(*)
!
!  Local variables
!
  integer(i4),                            save :: icalcn = 0
  integer(i4)                                  :: center
  integer(i4)                                  :: i
  integer(i4)                                  :: ictr
  integer(i4)                                  :: iflag
  integer(i4)                                  :: iquit
  integer(i4)                                  :: left
  integer(i4), dimension(:), allocatable       :: nconindsave
  integer(i4)                                  :: nlvar
  integer(i4),                            save :: numcal = 1
  integer(i4)                                  :: right
  integer(i4)                                  :: status
  logical                                      :: print
  real(dp)                                     :: aabs
  real(dp)                                     :: alfs
  real(dp)                                     :: alpha
  real(dp)                                     :: beta
  real(dp)                                     :: drop
  real(dp)                                     :: eps
  real(dp)                                     :: fin
  real(dp)                                     :: fmax
  real(dp)                                     :: fmin
  real(dp)                                     :: funold
  real(dp)                                     :: gamma
  real(dp)                                     :: pabs
  real(dp)                                     :: phi(3)
  real(dp)                                     :: s
  real(dp)                                     :: sqstor
  real(dp)                                     :: ssqlst
  real(dp)                                     :: stlast
  real(dp)                                     :: tee
  real(dp)                                     :: tiny
  real(dp)                                     :: vt(3)
  real(dp)                                     :: xcrit
  real(dp)                                     :: xmaxm
  real(dp)                                     :: xminm
  real(dp),    dimension(:), allocatable       :: xsave
  real(dp),    dimension(:), allocatable       :: xstor
  real(dp)                                     :: xxm
  real(dp)                                     :: ymaxst
!
  if (icalcn.ne.numcal) then
    step   = 1.0_dp
    icalcn = numcal
  endif
!
!  Allocate local memory
!
  allocate(xsave(nvar),stat=status)
  if (status/=0) call outofmemory('olinmin','xsave')
  allocate(xstor(nvar),stat=status)
  if (status/=0) call outofmemory('olinmin','xstor')
  allocate(nconindsave(nconnect),stat=status)
  if (status/=0) call outofmemory('olinmin','nconindsave')
!
!  Store initial vector in case line minimisation fails
!
  xsave(nmin:nvar) = xparam(nmin:nvar)
  nconindsave(1:nconnect) = nconnectind(1:nconnect)
!
!  In original linmin the following variables were only set once
!  in the above section, however this fails on some machines
!  e.g. Hewlett-Packard workstation
!
  print = (index(keyword,'linmin').ne.0.and.lopprt.and.ioproc)
  drop = 0.00001_dp
  xcrit = 0.00001_dp
  eps = 0.00001_dp
  if (index(keyword,'vprec').ne.0.or.lreaxFF) then
    drop = drop*0.0001_dp
    eps = eps*0.0001_dp
    xcrit = xcrit*0.0001_dp
  elseif (index(keyword,'prec').ne.0) then
    drop = drop*0.01_dp
    eps = eps*0.01_dp
    xcrit = xcrit*0.01_dp
  endif
  ymaxst = 0.4_dp
  tee = eps
  nlvar = nvar - nmin + 1
!
  xmaxm = 0.0_dp
  do i = nmin,nvar
    pabs = abs(pvect(i))
    xmaxm = max(xmaxm,pabs)
  enddo
  xminm = xmaxm
  xmaxm = ymaxst/xmaxm
  if (xmaxm.gt.stepmax) xmaxm = stepmax
  fin = funct1
  ssqlst = funct1
  iquit = 0
  phi(1) = funct1
  vt(1) = 0.0_dp
  vt(2) = step/4.0_dp
  if (vt(2).gt.xmaxm) vt(2) = xmaxm
  fmax = funct1
  fmin = funct1
  step = vt(2)
  call daxpy(nlvar,step,pvect(nmin),1_i4,xparam(nmin),1_i4)
  iflag = 0
  if (imode.eq.1) then
    call funct(iflag,nvar,xparam,phi(2),grad)
  else
    call deffun(iflag,nvar,xparam,phi(2),grad)
  endif
  if (phi(2).gt.fmax) fmax = phi(2)
  if (phi(2).lt.fmin) fmin = phi(2)
  call linmin_exchng(phi(2),sqstor,xparam,xstor,step,alfs,nvar)
  if (phi(1).le.phi(2)) go to 30
  go to 40
30 vt(3) = - vt(2)
  left = 3
  center = 1
  right = 2
  go to 50
40 vt(3) = 2.0_dp*vt(2)
  left = 1
  center = 2
  right = 3
50 stlast = vt(3)
  step = stlast - step
  call daxpy(nlvar,step,pvect(nmin),1_i4,xparam(nmin),1_i4)
  iflag = 0
  if (imode.eq.1) then
    call funct(iflag,nvar,xparam,funct1,grad)
  else
    call deffun(iflag,nvar,xparam,funct1,grad)
  endif
  if (funct1.gt.fmax) fmax = funct1
  if (funct1.lt.fmin) fmin = funct1
  if (funct1.lt.sqstor) call linmin_exchng(funct1,sqstor,xparam,xstor,stlast,alfs,nvar)
  if (funct1.lt.fin) iquit = 1
  phi(3) = funct1
!
!  Output first three points
!
  if (print) then
    write (ioout,'(''  ** Opt : line minimisation'')')
    write (ioout,'(''left   ...'',f17.8,f17.8)') vt(1),phi(1)
    write (ioout,'(''centre ...'',f17.8,f17.8)') vt(2),phi(2)
    write (ioout,'(''right  ...'',f17.8,f17.8,/)') vt(3),phi(3)
  endif
  do 180 ictr = 3,nlinmin
     alpha = vt(2) - vt(3)
     beta = vt(3) - vt(1)
     gamma = vt(1) - vt(2)
     if (abs(alpha*beta*gamma).gt.1.d-20) then
       alpha = - (phi(1)*alpha+phi(2)*beta+phi(3)*gamma)/(alpha*beta*gamma)
     else
       goto 190
     endif
     beta = ((phi(1)-phi(2))/gamma) - alpha*(vt(1)+vt(2))
     if (alpha.le.0.0_dp) then 
       if (phi(right).gt.phi(left)) then
         step = 3.0_dp*vt(left)-2.0_dp*vt(center)
       else
         step = 3.0_dp*vt(right) - 2.0_dp*vt(center)
       endif
       s = step - stlast
       if (abs(s).gt.xmaxm) s = sign(xmaxm,s)
       step = s + stlast
     else
       step = - beta/(2.0_dp*alpha)
       s = step - stlast
       xxm = xmaxm
       if (abs(s).gt.xxm) s = sign(xxm,s)
       step = s + stlast
     endif
     if (ictr.le.3) go to 120
     aabs = abs(s*xminm)
     if (aabs.lt.xcrit) go to 190
120  continue
     call daxpy(nlvar,s,pvect(nmin),1_i4,xparam(nmin),1_i4)
     funold = funct1
     iflag = 0
     if (imode.eq.1) then
       call funct(iflag,nvar,xparam,funct1,grad)
     else
       call deffun(iflag,nvar,xparam,funct1,grad)
     endif
     if (funct1.gt.fmax) fmax = funct1
     if (funct1.lt.fmin) fmin = funct1
     if (funct1.lt.sqstor) call linmin_exchng(funct1,sqstor,xparam,xstor,step,alfs,nvar)
     if (funct1.lt.fin) iquit = 1
!
!  Output current three points
!
     if (print) then
       write (ioout,'(''left   ...'',f17.8,f17.8)') vt(left),phi(left)
       write (ioout,'(''centre ...'',f17.8,f17.8)') vt(center),phi(center)
       write (ioout,'(''right  ...'',f17.8,f17.8)') vt(right),phi(right)
       write (ioout,'(''new    ...'',f17.8,f17.8,/)') step,funct1
     endif
!
!  Test to exit from linmin if not dropping in value of function fast.
!
     tiny = max((ssqlst-fmin)*0.2_dp,drop)
     tiny = min(tiny,0.5_dp)
     if (print) write(ioout,'(''  Tiny'',f14.9)') tiny
     if (abs(funold-funct1).lt.tiny.and.iquit.eq.1) goto 190
     if ((abs(step-stlast).le.eps*abs(step+stlast)+tee).and.(iquit.eq.1)) go to 190
     stlast = step
     if ((step.gt.vt(right)).or.(step.gt.vt(center).and.funct1.lt.phi(center)).or.(step.gt.vt(left) &
          .and.step.lt.vt(center).and.funct1.gt.phi(center))) then
       vt(left) = step
       phi(left) = funct1
     else
       vt(right) = step
       phi(right) = funct1
     endif
     if (vt(center).lt.vt(right)) go to 160
     i = center
     center = right
     right = i
160  if (vt(left).lt.vt(center)) go to 170
     i = left
     left = center
     center = i
170  if (vt(center).lt.vt(right)) go to 180
     i = center
     center = right
     right = i
180 continue
190 continue
  call linmin_exchng(sqstor,funct1,xstor,xparam,alfs,step,nvar)
  okf = (funct1.lt.ssqlst)
  if (funct1.lt.ssqlst) then
    if (step.lt.0.0_dp) then
      step = - step
      do i = nmin,nvar
        pvect(i) = - pvect(i)
      enddo
    endif
  endif
!
!  If line minimisation has failed then return initial vector
!
  if (.not.okf) then
    xparam(nmin:nvar) = xsave(nmin:nvar)
    nconnectind(1:nconnect) = nconindsave(1:nconnect)
  endif
!
!  Free local memory
!
  deallocate(nconindsave,stat=status)
  if (status/=0) call deallocate_error('olinmin','nconindsave')
  deallocate(xstor,stat=status)
  if (status/=0) call deallocate_error('olinmin','xstor')
  deallocate(xsave,stat=status)
  if (status/=0) call deallocate_error('olinmin','xsave')
!
  return
  end
