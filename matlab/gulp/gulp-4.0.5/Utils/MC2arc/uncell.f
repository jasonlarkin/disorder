      subroutine uncell(rv,a,b,c,alpha,beta,gamma)
C
C  Convert cell vectors to parameters
C
C   1/02 Cell parameters now abs'd to avoid problems in algorithms
C  11/04 Intent added
C
C  Conditions of use:
C
C  GULP is available free of charge to academic institutions
C  and non-commerical establishments only. Copies should be
C  obtained from the author only and should not be distributed
C  in any form by the user to a third party without the express
C  permission of the author. This notice applies to all parts
C  of the program, except any library routines which are
C  distributed with the code for completeness. All rights for
C  such routines remain with the original distributor.
C
C  No claim is made that this program is free from errors and
C  no liability will be accepted for any loss or damage that
C  may result. The user is responsible for checking the validity
C  of their results.
C
C  Copyright Curtin University 2004
C
C  Julian Gale, Curtin University, November 2004
C
      use datatypes
      implicit none
C
C  Passed variables
C
      real(dp),    intent(out)  :: a
      real(dp),    intent(out)  :: b
      real(dp),    intent(out)  :: c
      real(dp),    intent(out)  :: alpha
      real(dp),    intent(out)  :: beta
      real(dp),    intent(out)  :: gamma
      real(dp),    intent(in)   :: rv(3,3)
C
C  Local variables
C
      integer(i4)               :: i
      integer(i4)               :: j
      real(dp)                  :: temp(6)
      real(dp),            save :: radtodeg = 57.29577951_dp
C
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
C
C  Avoid round off errors for 90.0 and 120.0 degrees
C
      if (abs(alpha-90.0).lt.0.00001) alpha = 90.0_dp
      if (abs(alpha-120.0).lt.0.00001) alpha = 120.0_dp
      if (abs(beta-90.0).lt.0.00001) beta = 90.0_dp
      if (abs(beta-120.0).lt.0.00001) beta = 120.0_dp
      if (abs(gamma-90.0).lt.0.00001) gamma = 90.0_dp
      if (abs(gamma-120.0).lt.0.00001) gamma = 120.0_dp
C
      return
      end
