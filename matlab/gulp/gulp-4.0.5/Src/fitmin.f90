  subroutine fitmin(xparam,step,pvect,nvar,funct1,okf)
!*********************************************************************
!
!  linmin does a line minimisation.
!
!  on input:  xparam = starting coordinate of search.
!             step   = step size for initiating search.
!             pvect  = direction of search.
!             nvar   = number of variables in xparam.
!             funct1 = initial value of the function to be minimized.
!
!  on output: xparam = coordinate of minimum of functi0n.
!             step   = new step size, used in next call of linmin.
!             pvect  = unchanged, or negated, depending on step.
!             funct1 = final, minimum value of the function.
!             okf    = true if linmin improved funct, false otherwise.
!
!**********************************************************************
  use control
  use fitting
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: nvar
  logical                                      :: okf
  real(dp)                                     :: funct1
  real(dp)                                     :: pvect(*)
  real(dp)                                     :: step
  real(dp)                                     :: xparam(*)
!
!  Local variables
!
  integer(i4)                                  :: center
  integer(i4)                                  :: i
  integer(i4)                                  :: ictr
  integer(i4)                                  :: iquit
  integer(i4)                                  :: left 
  integer(i4)                                  :: maxlin
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
  real(dp)                                     :: xminm      
  real(dp),    dimension(:), allocatable       :: xstor
  real(dp)                                     :: xxm  
!
!  Allocate local memory
!
  allocate(xstor(nvar),stat=status)
  if (status/=0) call outofmemory('fitmin','xstor')
!
!  In original linmin the following variables were only set once
!  in the above section, however this fails on some machines
!  e.g. Hewlett-Packard workstation
!
  print = (index(keyword,'linmin') .ne. 0.and.ioproc)
  drop = 0.00002_dp
  if (index(keyword,'prec').ne.0) drop = drop*0.01_dp
  maxlin = 10
  xcrit  = 0.00000005_dp
  eps = 0.001_dp
  tee = eps
!
  do i = 1,nvar
    pabs = abs(pvect(i))
    xminm = max(0.0_dp,pabs)
  enddo
  fin = funct1
  ssqlst = funct1
  iquit = 0
  phi(1) = funct1
  vt(1) = 0.0_dp
  vt(2) = step/4.0_dp
  if (vt(2).gt.fstepmx) vt(2) = fstepmx
  fmax = funct1
  fmin = funct1
  step = vt(2)
  do i = 1,nvar
    xparam(i) = xparam(i) + step*pvect(i)
  enddo
  call fitfun(nvar,xparam,phi(2))
  if (phi(2).gt.fmax) fmax = phi(2)
  if (phi(2).lt.fmin) fmin = phi(2)
!
!  The following line looks funny as xstor is 0!
  call linmin_exchng(phi(2),sqstor,xparam,xstor,step,alfs,nvar)
! 
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
  do i = 1,nvar
    xparam(i) = xparam(i) + step*pvect(i)
  enddo
  call fitfun(nvar,xparam,funct1)
  if(funct1.gt.fmax) fmax = funct1
  if(funct1.lt.fmin) fmin = funct1
  if (funct1.lt.sqstor) call linmin_exchng(funct1,sqstor,xparam,xstor,stlast,alfs,nvar)
  if (funct1.lt.fin) iquit = 1
  phi(3) = funct1
!
!  Output first three points
!
  if (print) then
    write (ioout,'(''  ** Fit : line minimisation'')')
    write (ioout,'(5x,''left   ...'',f17.8,f17.6)') vt(1),phi(1)
    write (ioout,'(5x,''centre ...'',f17.8,f17.6)') vt(2),phi(2)
    write (ioout,'(5x,''right  ...'',f17.8,f17.6,/)') vt(3),phi(3)
  endif
!*********************************************************
!  Start loop over number of allowed line minimisations  *
!*********************************************************
  do 180 ictr = 3,maxlin
     alpha = vt(2) - vt(3)
     beta  = vt(3) - vt(1)
     gamma = vt(1) - vt(2)
     if (abs(alpha*beta*gamma) .gt. 1.d-20)then
       alpha = - (phi(1)*alpha+phi(2)*beta + phi(3)*gamma)/(alpha*beta*gamma)
     else
       goto 190
     endif
     beta = ((phi(1) - phi(2))/gamma) - alpha*(vt(1) + vt(2))
     if (alpha.le.0.0_dp) then
       if (phi(right).gt.phi(left)) then
         step = 3.0_dp*vt(left) - 2.0_dp*vt(center)
       else
         step = 3.0_dp*vt(right) - 2.0_dp*vt(center)
       endif
       s = step - stlast
       if (abs(s).gt.fstepmx) s = sign(fstepmx,s)*(1 + 0.01*(fstepmx/s))
       step = s + stlast
     else
       step = - beta/(2.0_dp*alpha)
       s = step - stlast
       xxm = 2.0_dp*fstepmx
       if (abs(s).gt.xxm) s = sign(xxm,s)*(1 + 0.01*(xxm/s))
       step = s + stlast
     endif
     if (ictr.le.5) go to 120
     aabs = abs(s*xminm)
     if (aabs.lt.xcrit) go to 190
120  continue
     do i = 1,nvar
       xparam(i) = xparam(i) + s*pvect(i)
     enddo
     funold = funct1
     call fitfun(nvar,xparam,funct1)
     if (funct1.gt.fmax) fmax = funct1
     if (funct1.lt.fmin) fmin = funct1
     if (funct1.lt.sqstor) call linmin_exchng(funct1,sqstor,xparam,xstor,step,alfs,nvar)
     if (funct1.lt.fin) iquit = 1
!
!  Output current three points
!
     if (print) then
       write (ioout,'(5x,''left   ...'',f17.8,f17.6)') vt(left),phi(left)
       write (ioout,'(5x,''centre ...'',f17.8,f17.6)') vt(center),phi(center)
       write (ioout,'(5x,''right  ...'',f17.8,f17.6)') vt(right),phi(right)
       write (ioout,'(5x,''new    ...'',f17.8,f17.6,/)') step,funct1
     endif
!
! Test to exit from linmin if not dropping in value of function fast.
!
     tiny = max((ssqlst-fmin)*0.2_dp , drop)
     tiny = min(tiny,0.5_dp)
     if (print) write(ioout,'(''  Tiny'',f14.9)') tiny
     if (abs(funold-funct1) .lt. tiny .and. iquit .eq. 1) goto 190
     if ((abs(step-stlast).le.eps*abs(step+stlast)+tee).and.(iquit.eq.1)) go to 190
     stlast = step
     if ((step.gt.vt(right)).or.(step.gt.vt(center) &
          .and.funct1.lt.phi(center)).or.(step.gt.vt(left) &
          .and.step.lt.vt(center).and.funct1.gt.phi(center))) &
           goto 140
     vt(right) = step
     phi(right) = funct1
     go to 150
140  vt(left) = step
     phi(left) = funct1
150  if (vt(center).lt.vt(right)) go to 160
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
  if (sqstor.lt.funct1) then
    call linmin_exchng(sqstor,funct1,xstor,xparam,alfs,step,nvar)
  endif
  okf = (funct1.lt.ssqlst)
  if (funct1.lt.ssqlst) then
    if (step.lt.0.0_dp) then
      step = - step
      do i = 1,nvar
        pvect(i) = - pvect(i)
      enddo
    endif
  endif
!
!  Free local memory
!
  deallocate(xstor,stat=status)
  if (status/=0) call deallocate_error('fitmin','xstor')
!
  return
  end
