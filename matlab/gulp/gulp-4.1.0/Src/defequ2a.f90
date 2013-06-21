  subroutine defequ2a
!
!  Generate the equivalent dsplacements from the asymmetric
!  unit for a defect calculation in region 2a.
!
!  dsymop   = array of 48 possible 3 x 3 symmetry operators
!  ndrel2a  = no. of ion to which ion is related by symmetry
!  ndrelop2a= symmetry operator which relates ion to asymmetric
!             unit ion
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
  use defects
  use region2a
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: is
  real(dp)           :: xi
  real(dp)           :: yi
  real(dp)           :: zi
  real(dp)           :: xis
  real(dp)           :: yis
  real(dp)           :: zis
!
!  Generate full displacements
!
  do i = 1,nreg2
    is = ndrelop2a(i)
    if (is.ne.1) then
      ii = ndsptr2a(ndrel2a(i))
      xi = xdis(ii)
      yi = ydis(ii)
      zi = zdis(ii)
      xis = xi*dsymop(1,1,is) + yi*dsymop(1,2,is) + zi*dsymop(1,3,is)
      yis = xi*dsymop(2,1,is) + yi*dsymop(2,2,is) + zi*dsymop(2,3,is)
      zis = xi*dsymop(3,1,is) + yi*dsymop(3,2,is) + zi*dsymop(3,3,is)
      xdis(i) = xis
      ydis(i) = yis
      zdis(i) = zis
    endif
  enddo
!
  return
  end
