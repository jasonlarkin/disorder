  subroutine fitgrad(iflag,n,xc,fsumsq,grad,hesinv)
!
!  Supplies the sum of the squares of the differences and
!  their first and second derivatives.
!
!  10/98 Setting of nfcf when nfpot=80 removed - codes changed
!   5/02 Argument m removed since it is not used
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use current
  use fitting
  use general
  implicit none
!
!  Passed variables
!
  integer(i4)       :: iflag
  integer(i4)       :: n
  real(dp)          :: fsumsq
  real(dp)          :: grad(*)
  real(dp)          :: xc(*)
  real(dp)          :: hesinv(*)
!
!  Local variables
!
  integer(i4)       :: i
  integer(i4)       :: ii
  real(dp)          :: cputime
  real(dp)          :: delt2
  real(dp)          :: fdelt
  real(dp)          :: fsumsq1
  real(dp)          :: fsumsq2
  real(dp)          :: oldxc
  real(dp)          :: rdelt
  real(dp)          :: t1
  real(dp)          :: t2
  real(dp)          :: ttdel
!
  if (delta.eq.0.0) delta = 0.000001_dp
  rdelt = 0.5_dp/delta
  delt2 = delta*delta
  t1 = cputime()
!
!  Evaluate residuals
!
  nfcf = 0
  call fitfun(n,xc,fsumsq)
!
!  Evaluate partial differentials
!
  ii = 0
  if (iflag.gt.0) then
    do i = 1,n
      oldxc = xc(i)
      xc(i) = xc(i) + delta
      call fitfun(n,xc,fsumsq2)
      xc(i) = oldxc - delta
      call fitfun(n,xc,fsumsq1)
      grad(i) = rdelt*(fsumsq2 - fsumsq1)
      xc(i) = oldxc
      if (iflag.ge.2) then
        ii = ii + i
        fdelt = fsumsq2 + fsumsq1 - 2.0_dp*fsumsq
        if (fdelt.gt.1.0d-8) then
          hesinv(ii) = abs(delt2/fdelt)
        else
          hesinv(ii) = 0.01_dp
        endif
      endif
    enddo
  endif
!
!  Reset nfcf to indicate all configurations
!
  nfcf = 0
!
!  Check to see if cputime is OK for another cycle
!
  t2 = cputime()
  ttdel = t2 - t1
  if (ttdel.gt.tdel) tdel = ttdel
  if (timmax.gt.0.0_dp) then
    if ((timmax-(t2-time0)).lt.tdel) iflag = - 1
  endif
!
  return
  end
