  function volume(rv)
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
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp)       :: rv(3,3)
  real(dp)       :: volume
!
!  Local variables
!
  real(dp)       :: r1x
  real(dp)       :: r1y
  real(dp)       :: r1z
  real(dp)       :: r2x
  real(dp)       :: r2y
  real(dp)       :: r2z
  real(dp)       :: r3x
  real(dp)       :: r3y
  real(dp)       :: r3z
  real(dp)       :: vol
!
  r1x = rv(1,1)
  r1y = rv(2,1)
  r1z = rv(3,1)
  r2x = rv(1,2)
  r2y = rv(2,2)
  r2z = rv(3,2)
  r3x = rv(1,3)
  r3y = rv(2,3)
  r3z = rv(3,3)
  vol = r1x*(r2y*r3z - r2z*r3y) + r1y*(r3x*r2z - r3z*r2x) + r1z*(r2x*r3y - r2y*r3x)
  volume = abs(vol)
!
  return
  end
!
  function area(rv)
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
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp)       :: rv(3,3)
  real(dp)       :: area
!
!  Local variables
!
  real(dp)       :: ara
  real(dp)       :: r1x
  real(dp)       :: r1y
  real(dp)       :: r2x
  real(dp)       :: r2y
!
  r1x = rv(1,1)
  r1y = rv(2,1)
  r2x = rv(1,2)
  r2y = rv(2,2)
  ara = r1x*r2y - r1y*r2x
  area = abs(ara)
!
  return
  end
