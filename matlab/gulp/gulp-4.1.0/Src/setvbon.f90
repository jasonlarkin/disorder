  subroutine setvbon(np)
!
!  Sets up the normalisation constant for the VBO_twobody potential
!
!  On entry:
!
!  np            = potential number
!  twopot(2,)    = gamma
!  twopot(3,)    = R0
!  twopot(4,)    = delta
!
!  On exit:
!
!  twopot(5,)    = N
!
!   1/09 Created
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use control
  use iochannels
  use parallel
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: np
!
!  Local variables
!
  real(dp)                :: delta
  real(dp)                :: gamma
  real(dp)                :: R0
!
  gamma = twopot(2,np)
  R0    = twopot(3,np)
  delta = twopot(4,np)
!
  twopot(5,np) = 2.0_dp*exp(gamma/(1.0_dp - sqrt(R0/delta)))
!
  return
  end
