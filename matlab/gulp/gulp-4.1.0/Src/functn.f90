  subroutine functn(iflag,n,xc,fc,gc,hess,imode)
!
!  Supplies the function and first derivatives of the energy
!  using numerical derivatives. Main purpose is for debugging
!  analytical derivatives.
!
!   8/98 Created from fefunctn 
!   4/01 Numerical calculation of hessian added for debugging
!  12/03 imode flag added to select perfect or defect case
!   7/05 Deallocations cleaned
!   3/07 Flag for funct now passed as variable to avoid error
!  12/07 Unused variables removed
!   5/09 Intent of arguments added
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
!  Julian Gale, NRI, Curtin University, May 2009
!
  use control, only : lfirst, leprint
  use general
  use iochannels
  use parallel
  implicit none
!
!  Passed arrays
!
  integer(i4),            intent(inout) :: iflag
  integer(i4),            intent(in)    :: imode
  integer(i4),            intent(in)    :: n
  real(dp),               intent(out)   :: fc
  real(dp),               intent(inout) :: xc(*)
  real(dp),               intent(out)   :: gc(*)
  real(dp),               intent(out)   :: hess(*)
!
!  Local variables
!
  integer(i4)                           :: i
  integer(i4)                           :: ifcall
  integer(i4)                           :: ii
  integer(i4)                           :: ind
  integer(i4)                           :: j
  integer(i4)                           :: status
  logical                               :: lgrad2
  real(dp)                              :: cputime
  real(dp)                              :: fcb
  real(dp)                              :: fcf
  real(dp), dimension(:),   allocatable :: gcb
  real(dp), dimension(:),   allocatable :: gcf
  real(dp),                        save :: tdmax = 0.0_dp
  real(dp)                              :: t1
  real(dp)                              :: t2
  real(dp)                              :: xci
!
  t1 = cputime()
!
!  Derivative flag
!
  lgrad2 = (iflag.ge.2)
  if (lgrad2) then
    ifcall = 1_i4
  else
    ifcall = 0_i4
  endif
  lfirst = .true.
!
!  Allocate local arrays
!
  allocate(gcb(n),stat=status)
  if (status/=0) call outofmemory('functn','gcb')
  allocate(gcf(n),stat=status)
  if (status/=0) call outofmemory('functn','gcf')
!*********************
!  Calculate energy  *
!*********************
  if (imode.eq.2) then
    leprint = .false.
    call deffun(ifcall,n,xc,fc,gc)
  else
    call funct(ifcall,n,xc,fc,gc)
  endif
!**********************************
!  Calculate numerical gradients  *
!**********************************
  ii = 0
  do i = 1,n
    xci = xc(i)
!
!  Forward
!
    xc(i) = xci + findiff
    if (imode.eq.2) then
      call deffun(ifcall,n,xc,fcf,gcf)
    else
      call funct(ifcall,n,xc,fcf,gcf)
    endif
!
!  Backward
!
    xc(i) = xci - findiff
    if (imode.eq.2) then
      call deffun(ifcall,n,xc,fcb,gcb)
    else
      call funct(ifcall,n,xc,fcb,gcb)
    endif
!
!  Calculate gradient
!
    gc(i) = (fcf - fcb)/(2.0_dp*findiff)
    if (lgrad2) then
!
!  Calculate hessian
!
      do j = 1,i
        hess(ii+j) = (gcf(j) - gcb(j))/(2.0_dp*findiff) 
      enddo
      ii = ii + i
    endif
!
!  Restore xc
!
    xc(i) = xci
  enddo
!*********************
!  Calculate energy  *
!*********************
  ifcall = 0_i4
  if (imode.eq.2) then
    leprint = .true.
    call deffun(ifcall,n,xc,fc,gc)
  else
    call funct(ifcall,n,xc,fc,gc)
  endif
!
!  Free local arrays
!
  deallocate(gcf,stat=status)
  if (status/=0) call deallocate_error('functn','gcf')
  deallocate(gcb,stat=status)
  if (status/=0) call deallocate_error('functn','gcb')
  if (lgrad2.and.ioproc) then
    write(ioout,'(/,''  Numerical Hessian Matrix :'',/)')
    do i = 1,n
      ind = i*(i-1)/2
      write(ioout,'(2x,10(f12.4))')(hess(ind+j),j=1,i)
    enddo
  endif
!*******************
!  CPU time check  *
!*******************
  t2 = cputime()
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0_dp) then
    if ((timmax-t2).lt.tdmax) iflag = -1
  endif
!
  return
  end
