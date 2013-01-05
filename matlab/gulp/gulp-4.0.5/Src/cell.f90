!****************
!  3-D systems  *
!****************
  subroutine cell3D(rv,a,b,c,alpha,beta,gamma)
!
!  Convert cell parameters into cartesian frame
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use constants
  implicit none
!
!  Passed variables
!  
  real(dp), intent(in)  :: a
  real(dp), intent(in)  :: b
  real(dp), intent(in)  :: c
  real(dp), intent(in)  :: alpha
  real(dp), intent(in)  :: beta
  real(dp), intent(in)  :: gamma
  real(dp), intent(out) :: rv(3,3)
!
!  Local variables
!
  real(dp)              :: alp
  real(dp)              :: bet
  real(dp)              :: cosa
  real(dp)              :: cosb
  real(dp)              :: cosg
  real(dp)              :: gam
  real(dp)              :: sing
  real(dp)              :: trm1
!
  if (alpha.eq.90.0) then
    cosa = 0.0_dp
  else
    alp = alpha*degtorad
    cosa = cos(alp)
  endif
  if (beta.eq.90.0) then
    cosb = 0.0_dp
  else
    bet = beta*degtorad
    cosb = cos(bet)
  endif
  if (gamma.eq.90.0) then
    sing = 1.0_dp
    cosg = 0.0_dp
  else
    gam = gamma*degtorad
    sing = sin(gam)
    cosg = cos(gam)
  endif
  rv(2,1) = 0.0_dp
  rv(3,1) = 0.0_dp
  rv(3,2) = 0.0_dp
  rv(1,1) = a
  rv(1,2) = b*cosg
  rv(2,2) = b*sing
  rv(1,3) = c*cosb
  rv(2,3) = c*(cosa - cosg*cosb)/sing
  trm1 = rv(2,3)/c
  rv(3,3) = c*sqrt(1.0_dp - cosb**2 - trm1**2)
!
  return
  end
!****************
!  2-D systems  *
!****************
  subroutine cell2D(rv,a,b,alpha)
!
!  Convert surface cell parameters into cartesian frame 
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use constants
  implicit none
!
!  Passed variables
! 
  real(dp), intent(in)  :: a
  real(dp), intent(in)  :: b
  real(dp), intent(in)  :: alpha
  real(dp), intent(out) :: rv(3,3)
!
!  Local variables     
!
  real(dp)              :: alp
  real(dp)              :: cosa
  real(dp)              :: sina
!
  if (alpha.eq.90.0) then
    cosa = 0.0_dp
    sina = 1.0_dp
  else
    alp = alpha*degtorad
    cosa = cos(alp)
    sina = sin(alp)
  endif
  rv(1,1) = a
  rv(2,1) = 0.0_dp
  rv(3,1) = 0.0_dp
  rv(1,2) = b*cosa
  rv(2,2) = b*sina
  rv(3,2) = 0.0_dp
  rv(1,3) = 0.0_dp
  rv(2,3) = 0.0_dp
  rv(3,3) = 0.0_dp
!
  return
  end
!****************
!  1-D systems  *
!****************
  subroutine cell1D(rv,a)
!
!  Convert surface cell parameters into cartesian frame 
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use constants
  implicit none
!
!  Passed variables
! 
  real(dp), intent(in)  :: a
  real(dp), intent(out) :: rv(3,3)
!
  rv(1,1) = a
  rv(2,1) = 0.0_dp
  rv(3,1) = 0.0_dp
  rv(1,2) = 0.0_dp
  rv(2,2) = 0.0_dp
  rv(3,2) = 0.0_dp
  rv(1,3) = 0.0_dp
  rv(2,3) = 0.0_dp
  rv(3,3) = 0.0_dp
!
  return
  end
