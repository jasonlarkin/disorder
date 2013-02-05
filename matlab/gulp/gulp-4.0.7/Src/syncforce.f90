  subroutine syncforce(nrp,n,xcrep,gc,tangent)
!
!  Projects out the perpendicular component of the derivatives 
!
!  On entry :
!
!  n             = number of variables per replica
!  nrp           = number of current replica
!  xcrep(n,*)    = variables for replicas
!  gc(n)         = uncorrected gradients for current replica
!
!  On return :
!
!  gc(n)         = corrected gradients for current replica
!
!  Workspace :
!
!  tangent(n)    = storage for tangent to path 
!
!  11/06 Created from nebforce
!  12/08 Option to fix tangent for each step added
!  11/12 Unused arguments nrep and niter removed
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use neb
  use parallel
  use synchro,        only : lfixtangent
  implicit none
!
!  Passed variables
!
  integer(i4),                  intent(in)    :: n
  integer(i4),                  intent(in)    :: nrp
  real(dp),                     intent(inout) :: gc(n)
  real(dp),                     intent(in)    :: xcrep(n,*)
  real(dp),                     intent(out)   :: tangent(n)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: nrpother
  real(dp)                                    :: fdot
  real(dp)                                    :: tnorm
  real(dp)                                    :: xdiff
!
!  Set pointer to other replica
!
  if (nrp.eq.1) then
    nrpother = 2
  else
    nrpother = 1
  endif
  if (.not.lfixtangent) then
!
!  Calculate tangent 
!
    tnorm = 0.0_dp
    do i = 1,n
      xdiff = xcrep(i,nrpother) - xcrep(i,nrp)
      tangent(i) = xdiff
      tnorm = tnorm + xdiff*xdiff
    enddo
!
!  Normalise tangent
!
    if (tnorm.gt.1.0d-12) then
      tnorm = 1.0_dp/sqrt(tnorm)
    else
      tnorm = 0.0_dp
    endif
    do i = 1,n
      tangent(i) = tnorm*tangent(i)
    enddo
  endif
!
!  Calculate projection of derivative vectors on to tangent
!
  fdot = 0.0_dp
  do i = 1,n
    fdot = fdot + gc(i)*tangent(i)
  enddo
  do i = 1,n
    gc(i) = gc(i) - fdot*tangent(i)
  enddo
!
  return
  end
