  subroutine initmaxeamfnspecdefaults(i)
!
!  Initialises the arrays associated with maxeamfnspec
!
!   9/10 Created from changemax routine
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use eam
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Initialise defaults for new part of array
!
  if (i.ge.1.and.i.le.maxeamfnspec) then
    symboleamfnspec(i) = ' '
    eamfnfile(i) = ' '
    eamfnpar(1,i) = 1.0_dp
    eamfnpar(2:33,i) = 0.0_dp
    neamfnmeamorder(i) = 1
    neamfnmeamcombotype(i) = 2
    neamfnmeamtype(i) = 2
    eamfnmeamcoeff(1:maxmeamorder,i) = 0.0_dp
    eamfnmeamcoeff(1,i) = 1.0_dp
  endif
!
  return
  end
