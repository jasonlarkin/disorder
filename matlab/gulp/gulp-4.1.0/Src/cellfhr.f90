  subroutine cellfhr(icfhr,rv)
!
!  Works out type of cell for hexagonal/trigonal systems
!
!  11/07 Unused variables cleaned up
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
!  Copyright Curtin University 2007
!
!  Julian Gale
!
  use datatypes
  implicit none
!
!  Passed variables
! 
  integer(i4), intent(out) :: icfhr
  real(dp),    intent(in)  :: rv(3,3)
!
!  Local variables     
!
  logical                  :: la90
  logical                  :: lb90
  real(dp)                 :: a
  real(dp)                 :: b
  real(dp)                 :: c
  real(dp)                 :: alpha
  real(dp)                 :: beta
  real(dp)                 :: gamma
!
  call uncell3D(rv,a,b,c,alpha,beta,gamma)
  la90 = (abs(alpha-90.0d0).lt.1.0d-3)
  lb90 = (abs(beta -90.0d0).lt.1.0d-3)
  if (la90.and.lb90.and.abs(gamma-120.0d0).lt.1.0d-3) then
!
!  Hexagonal
!
    icfhr = 0
  else
    icfhr = 1
  endif
!
  return
  end
