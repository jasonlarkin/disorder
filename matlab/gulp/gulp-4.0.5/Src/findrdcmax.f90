  subroutine findrdcmax(rdcmax)
!
!  Find maximum distance of defect from centre
!
!   8/03 Error due to z*z being multipled instead of added fixed
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
  use defects
  implicit none
!
!  Passed variables
!
  real(dp)             :: rdcmax
!
!  Local variables
!
  integer(i4)          :: i
  integer(i4)          :: ii
  real(dp)             :: rdc
  real(dp)             :: xi
  real(dp)             :: yi
  real(dp)             :: zi
!
  rdcmax = 0.0_dp
!
!  Find largest distance from list
!
  do i = 1,nvaca+ninte
    ii = ndptr(i)
    xi = xperf(ii) - xdc
    yi = yperf(ii) - ydc
    zi = zperf(ii) - zdc
    rdc = xi*xi + yi*yi + zi*zi
    rdcmax = max(rdcmax,rdc)
  enddo
  rdcmax = sqrt(rdcmax)
!
  return
  end
