  subroutine fefunctn(iflag,n,xc,fc,gc,hessian)
!
!  Supplies the function and first derivatives of the free energy
!  using numerical derivatives Main purposes is for checking
!  analytical derivatives.
!
!   8/97 Created from funct to replace old routine from 
!        numerical version.
!   9/97 If iflag=2 then need to keep second derivatives for
!        hessian calculation
!  11/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use control
  use general
  implicit none
!
!  Passed arguments
!
  integer(i4)    :: iflag
  integer(i4)    :: n
  real(dp)       :: fc
  real(dp)       :: xc(*)
  real(dp)       :: gc(*)
  real(dp)       :: hessian(*)
!
!  Local variables
!
  integer(i4)    :: i
  real(dp)       :: cputime
  real(dp)       :: fcb
  real(dp)       :: fcf
  real(dp), save :: tdmax = 0.0_dp
  real(dp)       :: t1
  real(dp)       :: t2
  real(dp)       :: xci
!
  t1 = cputime()
!
!  Derivative flags
!
  lfirst = .true.
!**************************
!  Calculate free energy  *
!**************************
  call fefunct(0_i4,n,xc,fc,gc,hessian)
!**********************************
!  Calculate numerical gradients  *
!**********************************
  do i = 1,n
    xci = xc(i)
!
!  Forward
!
    xc(i) = xci + findiff
    call fefunct(0_i4,n,xc,fcf,gc,hessian)
!
!  Backward
!
    xc(i) = xci - findiff
    call fefunct(0_i4,n,xc,fcb,gc,hessian)
!
!  Calculate gradient
!
    gc(i) = (fcf-fcb)/(2.0_dp*findiff)
!
!  Restore xc
!
    xc(i) = xci
  enddo
!************************************
!  Calculate free energy for return *
!************************************
  call fefunct(0_i4,n,xc,fc,gc,hessian)
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
