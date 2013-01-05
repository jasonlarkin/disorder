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
  implicit none
!
!  Passed variables
!  
  real*8           :: a
  real*8           :: b
  real*8           :: c
  real*8           :: alpha
  real*8           :: beta
  real*8           :: gamma
  real*8,     save :: degtorad = 1.0d0/57.29577951d0
  real*8           :: rv(3,3)
!
!  Local variables
!
  real*8           :: alp
  real*8           :: bet
  real*8           :: cosa
  real*8           :: cosb
  real*8           :: cosg
  real*8           :: gam
  real*8           :: sing
  real*8           :: trm1
!
  if (alpha.eq.90.0) then
    cosa = 0.0d0
  else
    alp = alpha*degtorad
    cosa = cos(alp)
  endif
  if (beta.eq.90.0) then
    cosb = 0.0d0
  else
    bet = beta*degtorad
    cosb = cos(bet)
  endif
  if (gamma.eq.90.0) then
    sing = 1.0d0
    cosg = 0.0d0
  else
    gam = gamma*degtorad
    sing = sin(gam)
    cosg = cos(gam)
  endif
  rv(2,1) = 0.0d0
  rv(3,1) = 0.0d0
  rv(3,2) = 0.0d0
  rv(1,1) = a
  rv(1,2) = b*cosg
  rv(2,2) = b*sing
  rv(1,3) = c*cosb
  rv(2,3) = c*(cosa - cosg*cosb)/sing
  trm1 = rv(2,3)/c
  rv(3,3) = c*sqrt(1.0d0 - cosb**2 - trm1**2)
!
  return
  end
