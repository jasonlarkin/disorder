  subroutine pressure(epv,lgrad1,lgrad2)
!
!  Calculates pressure*volume contribution to enthalpy
!  for externally applied pressures.
!
!   7/97 Error in second derivatives (1,1)/(2,2)/(3,3) corrected.
!  11/02 Style updated
!  12/10 Anisotropic pressure added
!   1/11 Anisotropic pressure modified so that only strain 
!        derivatives are added
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, January 2011
!
  use current
  use derivatives
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out) :: epv
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
!
!  Local variables
!
  integer(i4)              :: i
  real(dp),           save :: cfactor = 6.241460893d-3
  real(dp)                 :: epv2
  real(dp)                 :: vol
  real(dp)                 :: volume
!
!  Isotropic pressure
!
  vol = volume(rv)
  epv = press*vol*cfactor
!
!  Anisotropic pressure if present
!
  if (lanisotropicpress.and.lgrad1) then
    do i = 1,6
      strderv(i) = strderv(i) + anisotropicpress(i)*vol*cfactor
    enddo
  endif
!
!  Derivatives
!
  if (lgrad1) then
    strderv(1) = strderv(1) + epv
    strderv(2) = strderv(2) + epv
    strderv(3) = strderv(3) + epv
    if (lgrad2) then
      sderv2(1,1) = sderv2(1,1) + epv
      sderv2(2,2) = sderv2(2,2) + epv
      sderv2(3,3) = sderv2(3,3) + epv
      sderv2(2,1) = sderv2(2,1) + epv
      sderv2(3,1) = sderv2(3,1) + epv
      sderv2(3,2) = sderv2(3,2) + epv
      epv2 = 0.5_dp*epv
      sderv2(4,4) = sderv2(4,4) + epv2
      sderv2(5,5) = sderv2(5,5) + epv2
      sderv2(6,6) = sderv2(6,6) + epv2
    endif
  endif
!
  return
  end
