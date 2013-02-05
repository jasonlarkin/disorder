  subroutine lintoijk(ii,jj,kk,indin,imax,jmax,kmax)
!
!  Converts linear index to explicit cell looping indices
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
  integer(i4), intent(in)  :: indin
  integer(i4), intent(in)  :: imax
  integer(i4), intent(in)  :: jmax
  integer(i4), intent(in)  :: kmax
  integer(i4), intent(out) :: ii
  integer(i4), intent(out) :: jj
  integer(i4), intent(out) :: kk
!
!  Local variables
!
  integer(i4)              :: im2
  integer(i4)              :: ind
  integer(i4)              :: jm2
!
  ind = indin
  jm2 = 2*kmax + 1
  im2 = (2*jmax + 1)*jm2
  ii = (ind - 1)/im2
  ind = ind - ii*im2
  jj = (ind - 1)/jm2
  kk = ind - 1 - jj*jm2 - kmax
  jj = jj - jmax
  ii = ii - imax
!
  return
  end
