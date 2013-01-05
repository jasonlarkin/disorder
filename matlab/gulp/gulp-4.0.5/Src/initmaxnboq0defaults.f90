  subroutine initmaxnboQ0defaults(i)
!
!  Initialises the arrays associated with maxnboQ0
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
  use bondorderdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Initialise defaults for new part of array
!
  if (i.ge.1.and.i.le.maxnboQ0) then
    BOq0pot(i) = 0.0_dp
    BOq0ref(i) = 0.0_dp
    BOq0rho(i) = 0.0_dp
    nBOtypQ0(i) = 0
    nBOtypeQ0(i) = 1
  endif
!
  return
  end
