!********
!  3-D  *
!********
  subroutine uncell3D(rv,a,b,c,alpha,beta,gamma)
!
!  Convert cell vectors to parameters
!
!   1/02 Cell parameters now abs'd to avoid problems in algorithms
!  11/04 Intent added
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
  use constants
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)  :: a
  real(dp),    intent(out)  :: b
  real(dp),    intent(out)  :: c
  real(dp),    intent(out)  :: alpha
  real(dp),    intent(out)  :: beta
  real(dp),    intent(out)  :: gamma
  real(dp),    intent(in)   :: rv(3,3)
!
!  Local variables
!
  integer(i4)               :: i
  integer(i4)               :: j
  real(dp)                  :: temp(6)
!
  do i = 1,3
    temp(i) = 0.0_dp
    do j = 1,3
      temp(i) = temp(i) + rv(j,i)**2
    enddo
    temp(i) = sqrt(temp(i))
  enddo
  a = abs(temp(1))
  b = abs(temp(2))
  c = abs(temp(3))
  do i = 1,3
    temp(3+i) = 0.0_dp
  enddo
  do j = 1,3
    temp(4) = temp(4) + rv(j,2)*rv(j,3)
    temp(5) = temp(5) + rv(j,1)*rv(j,3)
    temp(6) = temp(6) + rv(j,1)*rv(j,2)
  enddo
  temp(4) = temp(4)/(temp(2)*temp(3))
  temp(5) = temp(5)/(temp(1)*temp(3))
  temp(6) = temp(6)/(temp(1)*temp(2))
  alpha = radtodeg*acos(temp(4))
  beta  = radtodeg*acos(temp(5))
  gamma = radtodeg*acos(temp(6))
!
!  Avoid round off errors for 90.0 and 120.0 degrees
!
  if (abs(alpha-90.0).lt.0.00001) alpha = 90.0_dp
  if (abs(alpha-120.0).lt.0.00001) alpha = 120.0_dp
  if (abs(beta-90.0).lt.0.00001) beta = 90.0_dp
  if (abs(beta-120.0).lt.0.00001) beta = 120.0_dp
  if (abs(gamma-90.0).lt.0.00001) gamma = 90.0_dp
  if (abs(gamma-120.0).lt.0.00001) gamma = 120.0_dp
!
  return
  end
!********
!  2-D  *
!********
  subroutine uncell2D(rv,a,b,alpha)
!
!  Convert cell vectors to parameters
!
!   1/02 Cell parameters now abs'd to avoid problems in algorithms
!  11/04 Intent added
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
  use constants
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)  :: a
  real(dp),    intent(out)  :: b
  real(dp),    intent(out)  :: alpha
  real(dp),    intent(in)   :: rv(3,3)
!
!  Local variables
!
  integer(i4)               :: i
  integer(i4)               :: j
  real(dp)                  :: temp(3)
!
  do i = 1,2
    temp(i) = 0.0_dp
    do j = 1,2
      temp(i) = temp(i) + rv(j,i)**2
    enddo
    temp(i) = sqrt(temp(i))
  enddo
  a = abs(temp(1))
  b = abs(temp(2))
  temp(3) = 0.0_dp
  do j = 1,2
    temp(3) = temp(3) + rv(j,1)*rv(j,2)
  enddo
  temp(3) = temp(3)/(temp(1)*temp(2))
  alpha = radtodeg*acos(temp(3))
!
!  Avoid round off errors for 90.0 and 120.0 degrees
!
  if (abs(alpha-90.0).lt.0.00001) alpha = 90.0_dp
  if (abs(alpha-120.0).lt.0.00001) alpha = 120.0_dp
!
  return
  end
!********
!  1-D  *
!********
  subroutine uncell1D(rv,a)
!
!  Convert cell vectors to parameters
!
!   1/02 Cell parameters now abs'd to avoid problems in algorithms
!  11/04 Intent added
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
  use constants
  implicit none
!
!  Passed variables
!
  real(dp),  intent(out)  :: a
  real(dp),  intent(in)   :: rv(3,3)
!
  a = abs(rv(1,1))
!
  return
  end
