  subroutine mddistconpost
!
!  Applies a distance constraint during MD
!
!   7/08 Created from mdvvcorrect
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, July 2008.
!
  use current
  use derivatives
  use moldyn
  use optimisation
  use velocities
  implicit none
!
!  Local variables
!
  integer(i4) :: n1
  integer(i4) :: n2
  real(dp)    :: m1
  real(dp)    :: m2
  real(dp)    :: rdotr
  real(dp)    :: rdotv
  real(dp)    :: r(3)
  real(dp)    :: v(3)
!
  if (lmdconstrain(ncf)) then 
!             
!  Initialise variables for constrained dynamics
!
    n1 = nmdconstrainatom(1,ncf)
    n2 = nmdconstrainatom(2,ncf)
    m1 = mass(n1)
    m2 = mass(n2)
!           
!  Find relative vectors and velocities
!           
    r(1) = xalat(n1) - xalat(n2)
    r(2) = yalat(n1) - yalat(n2)
    r(3) = zalat(n1) - zalat(n2)
    v(1) = velx(n1) - velx(n2)
    v(2) = vely(n1) - vely(n2)
    v(3) = velz(n1) - velz(n2)
!
!  Calculate dot products
!
    rdotr = r(1)*r(1) + r(2)*r(2) + r(3)*r(3)
    rdotv = r(1)*v(1) + r(2)*v(2) + r(3)*v(3)
!
!  Calculate constrain force x timestep
!
    lambdaV = - rdotv*m1*m2/(rdotr*(m1 + m2))
!
!  Calculate modified velocities allowing for constraint force
!
    velx(n1) = velx(n1) + lambdaV*r(1)/m1
    vely(n1) = vely(n1) + lambdaV*r(2)/m1
    velz(n1) = velz(n1) + lambdaV*r(3)/m1
    velx(n2) = velx(n2) - lambdaV*r(1)/m2
    vely(n2) = vely(n2) - lambdaV*r(2)/m2
    velz(n2) = velz(n2) - lambdaV*r(3)/m2
  endif
  return
  end
