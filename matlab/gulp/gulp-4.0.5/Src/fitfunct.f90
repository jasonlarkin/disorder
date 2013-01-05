  subroutine fitfunct(iflag,n,xc,fsumsq,gc,hess)
!
!  Supplies the sum of squares and it's derivatives for
!  fitting using numerical derivatives. 
!
!   5/02 Created from functn 
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
!  Julian Gale, NRI, Curtin University, May 2005
!
  use control, only : lfirst, lfbfgs
  use fitting, only : delta
  use general
  use iochannels
  use parallel
  use times
  implicit none
!
!  Passed arrays
!
  integer(i4)                            :: iflag
  integer(i4)                            :: n
  real(dp)                               :: fsumsq
  real(dp)                               :: xc(*)
  real(dp)                               :: gc(*)
  real(dp)                               :: hess(*)
!
!  Local variables
!
  integer(i4)                            :: i
  integer(i4)                            :: ifcall
  integer(i4)                            :: ii
  integer(i4)                            :: info
  integer(i4)                            :: j
  integer(i4), dimension(:), allocatable :: kpvt
  integer(i4)                            :: status
  real(dp)                               :: cputime
  real(dp)                               :: fcb
  real(dp)                               :: fcf
  real(dp),    dimension(:), allocatable :: gcb
  real(dp),    dimension(:), allocatable :: gcf
  real(dp)                               :: t1
  real(dp)                               :: t2
  real(dp)                               :: t1i
  real(dp)                               :: t2i
  real(dp),                         save :: tdmax = 0.0_dp
  real(dp)                               :: xci
  real(dp),    dimension(:), allocatable :: wrk
!
  t1 = cputime()
  lfirst = .true.
  if (delta.eq.0.0) delta = 0.000001_dp
  if (lfbfgs) then
!
!  Derivative flags
!
    if (iflag.ge.2) then
      ifcall = 1_i4
    else
      ifcall = 0_i4
    endif
!
!  Allocate local arrays
!
    allocate(gcb(n),stat=status)
    if (status/=0) call outofmemory('fitfunct','gcb')
    allocate(gcf(n),stat=status)
    if (status/=0) call outofmemory('fitfunct','gcf')
!*************************************
!  Calculate full numerical Hessian  *
!*************************************
    ii = 0
    do i = 1,n
      xci = xc(i)
!
!  Forward
!
      xc(i) = xci + delta
      call fitgrad(ifcall,n,xc,fcf,gcf,hess)
!
!  Backward
!
      xc(i) = xci - delta
      call fitgrad(ifcall,n,xc,fcb,gcb,hess)
!
!  Calculate gradient
!
      gc(i) = (fcf - fcb)/(2.0_dp*delta)
      if (iflag.ge.2) then
!
!  Calculate hessian
!
        do j = 1,i
          hess(ii+j) = (gcf(j) - gcb(j))/(2.0_dp*delta) 
        enddo
        ii = ii + i
      endif
!
!  Restore xc
!
      xc(i) = xci
    enddo
!*****************************
!  Calculate sum of squares  *
!*****************************
    call fitfun(n,xc,fsumsq)
!
!  Invert Hessian matrix
!
    t1i = cputime()
    allocate(kpvt(n),stat=status)
    if (status/=0) call outofmemory('fitfunct','kpvt')
    call dsptrf('U',n,hess,kpvt,info)
!
!  Check for singularities
!
    if (info.gt.0) then
      if (ioproc) then
        write(ioout,'(''  ** Ill conditioned Hessian - using diagonal elements only **'')')
      endif
      do i = 1,n*(n+1)/2
        hess(i) = 0.0_dp
      enddo
      call fitgrad(iflag,n,xc,fsumsq,gc,hess)
    else
!
!  Complete inversion
!
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('fitfunct','wrk')
      call dsptri('U',n,hess,kpvt,wrk,info)
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('fitbfgs','wrk')
    endif
    t2i = cputime()
    tmati = tmati + t2i - t1i
!
!  Free local arrays
!
    deallocate(kpvt,stat=status)
    if (status/=0) call deallocate_error('fitbfgs','kpvt')
    deallocate(gcf,stat=status)
    if (status/=0) call deallocate_error('fitbfgs','gcf')
    deallocate(gcb,stat=status)
    if (status/=0) call deallocate_error('fitbfgs','gcb')
  else
!**********************************
!  Calculate numerical gradients  *
!**********************************
    call fitgrad(iflag,n,xc,fsumsq,gc,hess)
  endif
!*******************
!  CPU time check  *
!*******************
  t2 = cputime()
  tdmax = t2 - t1
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0_dp) then
    if ((timmax-t2).lt.tdmax) iflag = -1
  endif
!
  return
  end
