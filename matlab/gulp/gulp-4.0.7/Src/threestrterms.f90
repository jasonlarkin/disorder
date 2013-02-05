  subroutine threestrterms(ndim,rprod,x21,y21,z21,x31,y31,z31,x32,y32,z32)
!
!  Subroutine for calculating three-body strain terms
!
!  On entry :
!
!   ndim              = number of dimensions
!   x21, y21, z21 etc = Cartesian components of interatomic vectors
!
!  On exit :
!
!   rprod             = strain terms
!  
!   2/01 Created from fourstrterms and threemd
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
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4) :: ndim
  real(dp)    :: rprod(6,3)
  real(dp)    :: x21
  real(dp)    :: y21
  real(dp)    :: z21
  real(dp)    :: x31
  real(dp)    :: y31
  real(dp)    :: z31
  real(dp)    :: x32
  real(dp)    :: y32
  real(dp)    :: z32
!
  if (ndim.eq.3) then
!
!  Set up strain products for 3-D case
!
    rprod(1,1) = x21*x21
    rprod(2,1) = y21*y21
    rprod(3,1) = z21*z21
    rprod(4,1) = y21*z21
    rprod(5,1) = x21*z21
    rprod(6,1) = x21*y21
    rprod(1,2) = x31*x31
    rprod(2,2) = y31*y31
    rprod(3,2) = z31*z31
    rprod(4,2) = y31*z31
    rprod(5,2) = x31*z31
    rprod(6,2) = x31*y31
    rprod(1,3) = x32*x32
    rprod(2,3) = y32*y32
    rprod(3,3) = z32*z32
    rprod(4,3) = y32*z32
    rprod(5,3) = x32*z32
    rprod(6,3) = x32*y32
  elseif (ndim.eq.2) then
!
!  Set up strain products for 2-D case
!
    rprod(1,1) = x21*x21
    rprod(2,1) = y21*y21
    rprod(3,1) = x21*y21
    rprod(1,2) = x31*x31
    rprod(2,2) = y31*y31
    rprod(3,2) = x31*y31
    rprod(1,3) = x32*x32
    rprod(2,3) = y32*y32
    rprod(3,3) = x32*y32
  elseif (ndim.eq.1) then
!
!  Set up strain products for 1-D case
!
    rprod(1,1) = x21*x21
    rprod(1,2) = x31*x31
    rprod(1,3) = x32*x32
  endif
!
  return
  end
