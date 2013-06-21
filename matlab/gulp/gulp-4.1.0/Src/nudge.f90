  subroutine nudge(n,xc,gc)
!
!  Projects out the perpendicular component of the derivatives 
!
!  On entry :
!
!  n             = number of variables 
!  xc(n)         = variables for current position
!  xcother(n)    = variables for other position
!  gc(n)         = uncorrected gradients 
!
!  On return :
!
!  gc(n)         = corrected gradients for current replica
!
!  Workspace :
!
!  tangent(n)    = storage for tangent to path 
!
!  11/06 Created from syncforce
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use datatypes
  use xcgc, only : xcother
  implicit none
!
!  Passed variables
!
  integer(i4),                  intent(in)    :: n
  real(dp),                     intent(inout) :: gc(n)
  real(dp),                     intent(in)    :: xc(n)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: status
  real(dp)                                    :: fdot
  real(dp)                                    :: tnorm
  real(dp)                                    :: xdiff
  real(dp),   allocatable, dimension(:)       :: tangent
!
  allocate(tangent(n),stat=status)
  if (status/=0) call outofmemory('nudge','tangent')
!
!  Calculate tangent 
!
  tnorm = 0.0_dp
  do i = 1,n
    xdiff = xcother(i) - xc(i)
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
  deallocate(tangent,stat=status)
  if (status/=0) call deallocate_error('nudge','tangent')
!
  return
  end
