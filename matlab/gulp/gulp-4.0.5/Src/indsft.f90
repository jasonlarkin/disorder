  subroutine indsft(ind,ixd,iyd,izd,isign)
!
!  Correct cell indices for the shifts ixd, iyd, izd multiplied by isign
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
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout) :: ind
  integer(i4), intent(in)    :: isign
  integer(i4), intent(in)    :: ixd
  integer(i4), intent(in)    :: iyd
  integer(i4), intent(in)    :: izd
!
!  Local variables
!
  integer(i4)                :: ix
  integer(i4)                :: iy
  integer(i4)                :: iz
!
!  Convert current cell index to components
!
  call mindtoijk(ind,ix,iy,iz)
!
!  Correct components
!
  ix = ix + isign*ixd
  iy = iy + isign*iyd
  iz = iz + isign*izd
!
!  Convert components back to cell index
!
  ind = (ix + 5) + 10*(iy + 5) + 100*(iz + 5)
!
  return
  end
