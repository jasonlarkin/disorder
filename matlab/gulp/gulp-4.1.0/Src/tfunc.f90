!***************
!  Tanh taper  *
!***************
  subroutine tfunc(r,Rw,w,t,dtdr,d2tdr2,d3tdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the taper-like function and derivatives 
!  using tanh interpolation.
!
!  On entry : 
!
!  r               = current distance for which the taper function
!                    is to be calculated
!  Rw              = location of switching function
!  w               = width of switching function
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  t               = the value of the taper function at r
!  dtdr            = first derivative of taper function
!  d2twdr2         = second derivative of taper function
!  d3twdr3         = third derivative of taper function
!
!   2/10 Created
!   8/11 If w < 0 then return taper function = 1
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
!  Julian Gale, NRI, Curtin University, August 2011
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: r
  real(dp),    intent(in)             :: Rw
  real(dp),    intent(in)             :: w
  real(dp),    intent(out)            :: t
  real(dp),    intent(out)            :: dtdr
  real(dp),    intent(out)            :: d2tdr2
  real(dp),    intent(out)            :: d3tdr3
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: denom
  real(dp)                            :: e2x
  real(dp)                            :: dtanhdx
  real(dp)                            :: d2tanhdx2
  real(dp)                            :: d3tanhdx3
  real(dp)                            :: dxdr
  real(dp)                            :: tanh
  real(dp)                            :: x
!
  if (w.gt.0.0_dp) then
    x = (r - Rw)/w
    e2x = exp(2.0_dp*x)
    denom = 1.0_dp/(e2x + 1.0_dp)
    tanh = (e2x - 1.0_dp)*denom
!
! Function
!
    t = 0.5_dp*(1.0_dp + tanh)
    if (lgrad1) then
      dxdr = 1.0_dp/w
      dtanhdx = 1.0_dp - tanh*tanh
      dtdr = 0.5_dp*dtanhdx*dxdr
      if (lgrad2) then
        d2tanhdx2 = - 2.0_dp*tanh*dtanhdx
        d2tdr2 = 0.5_dp*d2tanhdx2*dxdr**2
        if (lgrad3) then
          d3tanhdx3 = - 2.0_dp*(tanh*d2tanhdx2 + dtanhdx*dtanhdx)
          d3tdr3 = 0.5_dp*d3tanhdx3*dxdr**3
        endif
      endif
    endif
  else
    t = 1.0_dp
    if (lgrad1) then
      dtdr = 0.0_dp
      if (lgrad2) then
        d2tdr2 = 0.0_dp
        if (lgrad3) then
          d3tdr3 = 0.0_dp
        endif
      endif
    endif
  endif
!
  return
  end
