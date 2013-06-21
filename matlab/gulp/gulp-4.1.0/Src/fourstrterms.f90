  subroutine fourstrterms(ndim,rprod,x21,y21,z21,x31,y31,z31, &
    x41,y41,z41,x32,y32,z32,x42,y42,z42,x43,y43,z43)
!
!  Subroutine for calculating four-body strain terms
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
!   2/01 Created from fournos
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
  integer(i4) :: ndim
  real(dp)    :: rprod(6,6)
  real(dp)    :: x21
  real(dp)    :: y21
  real(dp)    :: z21
  real(dp)    :: x31
  real(dp)    :: y31
  real(dp)    :: z31
  real(dp)    :: x41
  real(dp)    :: y41
  real(dp)    :: z41
  real(dp)    :: x32
  real(dp)    :: y32
  real(dp)    :: z32
  real(dp)    :: x42
  real(dp)    :: y42
  real(dp)    :: z42
  real(dp)    :: x43
  real(dp)    :: y43
  real(dp)    :: z43
!
  if (ndim.eq.3) then
!
!  Set up strain products for 3-D case
!
    rprod(1,1) = x21*x21
    rprod(1,2) = x31*x31
    rprod(1,3) = x41*x41
    rprod(1,4) = x32*x32
    rprod(1,5) = x42*x42
    rprod(1,6) = x43*x43
    rprod(2,1) = y21*y21
    rprod(2,2) = y31*y31
    rprod(2,3) = y41*y41
    rprod(2,4) = y32*y32
    rprod(2,5) = y42*y42
    rprod(2,6) = y43*y43
    rprod(3,1) = z21*z21
    rprod(3,2) = z31*z31
    rprod(3,3) = z41*z41
    rprod(3,4) = z32*z32
    rprod(3,5) = z42*z42
    rprod(3,6) = z43*z43
    rprod(4,1) = y21*z21
    rprod(4,2) = y31*z31
    rprod(4,3) = y41*z41
    rprod(4,4) = y32*z32
    rprod(4,5) = y42*z42
    rprod(4,6) = y43*z43
    rprod(5,1) = x21*z21
    rprod(5,2) = x31*z31
    rprod(5,3) = x41*z41
    rprod(5,4) = x32*z32
    rprod(5,5) = x42*z42
    rprod(5,6) = x43*z43
    rprod(6,1) = x21*y21
    rprod(6,2) = x31*y31
    rprod(6,3) = x41*y41
    rprod(6,4) = x32*y32
    rprod(6,5) = x42*y42
    rprod(6,6) = x43*y43
  elseif (ndim.eq.2) then
!
!  Set up strain products for 2-D case
!
    rprod(1,1) = x21*x21
    rprod(1,2) = x31*x31
    rprod(1,3) = x41*x41
    rprod(1,4) = x32*x32
    rprod(1,5) = x42*x42
    rprod(1,6) = x43*x43
    rprod(2,1) = y21*y21
    rprod(2,2) = y31*y31
    rprod(2,3) = y41*y41
    rprod(2,4) = y32*y32
    rprod(2,5) = y42*y42
    rprod(2,6) = y43*y43
    rprod(3,1) = x21*y21
    rprod(3,2) = x31*y31
    rprod(3,3) = x41*y41
    rprod(3,4) = x32*y32
    rprod(3,5) = x42*y42
    rprod(3,6) = x43*y43
  elseif (ndim.eq.1) then
!
!  Set up strain products for 1-D case
!
    rprod(1,1) = x21*x21
    rprod(1,2) = x31*x31
    rprod(1,3) = x41*x41
    rprod(1,4) = x32*x32
    rprod(1,5) = x42*x42
    rprod(1,6) = x43*x43
  endif
!
  return
  end
