  subroutine hfunc(u,x,signx,y,z,h,d1h,d2h,d3h,lgrad1,lgrad2,lgrad3)
!
!  Calculates the H function required by the 1-D Coulomb sum.
!
!   9/01 Created
!  10/01 Derivatives added
!  12/01 Second derivatives added
!   5/02 Third derivatives added
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
!  Copyright Curtin University 2004
!
!  Julian Gale, Curtin University, November 2004
!
  use datatypes
  implicit none
!
!  Passed variables
!
  logical,  intent(in)   :: lgrad1
  logical,  intent(in)   :: lgrad2
  logical,  intent(in)   :: lgrad3
  real(dp), intent(out)  :: d1h(3)
  real(dp), intent(out)  :: d2h(6)
  real(dp), intent(out)  :: d3h(10)
  real(dp), intent(out)  :: h
  real(dp), intent(in)   :: u
  real(dp), intent(in)   :: signx
  real(dp), intent(in)   :: x
  real(dp), intent(in)   :: y
  real(dp), intent(in)   :: z
!
!  Local variables
!
  real(dp)   :: alpha
  real(dp)   :: d1
  real(dp)   :: d1a
  real(dp)   :: d1a2
  real(dp)   :: d1f(3)
  real(dp)   :: d1g(3)
  real(dp)   :: d2f(6)
  real(dp)   :: d2g(6)
  real(dp)   :: d3f(10)
  real(dp)   :: f
  real(dp)   :: g
  real(dp)   :: ux
  real(dp)   :: uxa
!
!  Function
!
  alpha = y*y + z*z
  ux = u + x
  g = (ux*ux + alpha)
  uxa = sqrt(g)
  f = (uxa + ux)
  h = log(f)
!
!  First derivatives
!
  if (lgrad1) then
    d1 = 1.0_dp/f
    d1a = 1.0_dp/uxa
    d1g(1) = 2.0_dp*ux*signx
    d1g(2) = 2.0_dp*y
    d1g(3) = 2.0_dp*z
    d1f(1) = 0.5_dp*d1a*d1g(1) + signx
    d1f(2) = 0.5_dp*d1a*d1g(2)
    d1f(3) = 0.5_dp*d1a*d1g(3)
    d1h(1) = d1*d1f(1)
    d1h(2) = d1*d1f(2)
    d1h(3) = d1*d1f(3)
    if (lgrad2) then
      d1a2 = d1a*d1a
!
      d2g(1) = 2.0_dp
      d2g(2) = 2.0_dp
      d2g(3) = 2.0_dp
      d2g(4) = 0.0_dp
      d2g(5) = 0.0_dp
      d2g(6) = 0.0_dp
!
      d2f(1) = 0.5_dp*d1a*(d2g(1) - 0.5_dp*d1a2*d1g(1)*d1g(1))
      d2f(2) = 0.5_dp*d1a*(d2g(2) - 0.5_dp*d1a2*d1g(2)*d1g(2))
      d2f(3) = 0.5_dp*d1a*(d2g(3) - 0.5_dp*d1a2*d1g(3)*d1g(3))
      d2f(4) = 0.5_dp*d1a*(d2g(4) - 0.5_dp*d1a2*d1g(2)*d1g(3))
      d2f(5) = 0.5_dp*d1a*(d2g(5) - 0.5_dp*d1a2*d1g(1)*d1g(3))
      d2f(6) = 0.5_dp*d1a*(d2g(6) - 0.5_dp*d1a2*d1g(1)*d1g(2))
!
      d2h(1) = d1*d2f(1) - d1*d1*d1f(1)*d1f(1)
      d2h(2) = d1*d2f(2) - d1*d1*d1f(2)*d1f(2)
      d2h(3) = d1*d2f(3) - d1*d1*d1f(3)*d1f(3)
      d2h(4) = d1*d2f(4) - d1*d1*d1f(2)*d1f(3)
      d2h(5) = d1*d2f(5) - d1*d1*d1f(1)*d1f(3)
      d2h(6) = d1*d2f(6) - d1*d1*d1f(1)*d1f(2)
      if (lgrad3) then
        d3f(1) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(1)*d1g(1)*d1g(1) - 3.0_dp*d2g(1)*d1g(1))
        d3f(2) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(2)*d1g(1)*d1g(1) - 2.0_dp*d2g(6)*d1g(1) - d2g(1)*d1g(2))
        d3f(3) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(1)*d1g(1) - 2.0_dp*d2g(5)*d1g(1) - d2g(1)*d1g(3))
        d3f(4) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(2)*d1g(2)*d1g(1) - 2.0_dp*d2g(6)*d1g(2) - d2g(2)*d1g(1))
        d3f(5) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(2)*d1g(1) - d2g(4)*d1g(1) - d2g(5)*d1g(2) - d2g(6)*d1g(3))
        d3f(6) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(3)*d1g(1) - 2.0_dp*d2g(5)*d1g(3) - d2g(3)*d1g(1))
        d3f(7) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(2)*d1g(2)*d1g(2) - 3.0_dp*d2g(2)*d1g(2))
        d3f(8) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(2)*d1g(2) - 2.0_dp*d2g(4)*d1g(2) - d2g(2)*d1g(3))
        d3f(9) = 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(3)*d1g(2) - 2.0_dp*d2g(4)*d1g(3) - d2g(3)*d1g(2))
        d3f(10)= 0.25_dp*d1a*d1a2*(1.5_dp*d1a2*d1g(3)*d1g(3)*d1g(3) - 3.0_dp*d2g(3)*d1g(3))
!
        d3h(1) = d1*(d3f(1) + 2.0_dp*d1*d1*d1f(1)*d1f(1)*d1f(1) - d1*(3.0_dp*d2f(1)*d1f(1)))
        d3h(2) = d1*(d3f(2) + 2.0_dp*d1*d1*d1f(2)*d1f(1)*d1f(1) - d1*(2.0_dp*d2f(6)*d1f(1) + d2f(1)*d1f(2)))
        d3h(3) = d1*(d3f(3) + 2.0_dp*d1*d1*d1f(3)*d1f(1)*d1f(1) - d1*(2.0_dp*d2f(5)*d1f(1) + d2f(1)*d1f(3)))
        d3h(4) = d1*(d3f(4) + 2.0_dp*d1*d1*d1f(2)*d1f(2)*d1f(1) - d1*(2.0_dp*d2f(6)*d1f(2) + d2f(2)*d1f(1)))
        d3h(5) = d1*(d3f(5) + 2.0_dp*d1*d1*d1f(3)*d1f(2)*d1f(1) - d1*(d2f(4)*d1f(1) + d2f(5)*d1f(2) + d2f(6)*d1f(3)))
        d3h(6) = d1*(d3f(6) + 2.0_dp*d1*d1*d1f(3)*d1f(3)*d1f(1) - d1*(2.0_dp*d2f(5)*d1f(3) + d2f(3)*d1f(1)))
        d3h(7) = d1*(d3f(7) + 2.0_dp*d1*d1*d1f(2)*d1f(2)*d1f(2) - d1*(3.0_dp*d2f(2)*d1f(2)))
        d3h(8) = d1*(d3f(8) + 2.0_dp*d1*d1*d1f(3)*d1f(2)*d1f(2) - d1*(2.0_dp*d2f(4)*d1f(2) + d2f(2)*d1f(3)))
        d3h(9) = d1*(d3f(9) + 2.0_dp*d1*d1*d1f(3)*d1f(3)*d1f(2) - d1*(2.0_dp*d2f(4)*d1f(3) + d2f(3)*d1f(2)))
        d3h(10)= d1*(d3f(10)+ 2.0_dp*d1*d1*d1f(3)*d1f(3)*d1f(3) - d1*(3.0_dp*d2f(3)*d1f(3)))
      endif
    endif
  endif
!
  return
  end
