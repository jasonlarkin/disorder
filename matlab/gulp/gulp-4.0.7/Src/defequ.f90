  subroutine defequ
!
!  Generate the equivalent positions from the asymmetric
!  unit for a defect calculation.
!
!  dsymop   = array of 48 possible 3 x 3 symmetry operators
!  ndrel    = no. of ion to which ion is related by symmetry
!  ndrelop  = symmetry operator which relates ion to asymmetric
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
  use current
  use defects
  implicit none
!
!  Local variables
!
  integer(i4)     :: i
  integer(i4)     :: ii
  integer(i4)     :: is
  real(dp)        :: xi
  real(dp)        :: yi
  real(dp)        :: zi
  real(dp)        :: xis
  real(dp)        :: yis
  real(dp)        :: zis
!*******************************************
!  Generate full region 1 using operators  *
!*******************************************
  do i = 1,nreg1
    is = ndrelop(i)
    if (is.ne.1) then
      ii = ndsptr(ndrel(i))
      xi = xdefe(ii) - xdc
      yi = ydefe(ii) - ydc
      zi = zdefe(ii) - zdc
      xis = xi*dsymop(1,1,is) + yi*dsymop(1,2,is) + zi*dsymop(1,3,is)
      yis = xi*dsymop(2,1,is) + yi*dsymop(2,2,is) + zi*dsymop(2,3,is)
      zis = xi*dsymop(3,1,is) + yi*dsymop(3,2,is) + zi*dsymop(3,3,is)
      xdefe(i) = xis + xdc
      ydefe(i) = yis + ydc
      zdefe(i) = zis + zdc
      radefe(i) = radefe(ii)
    endif
  enddo
!
  return
  end
