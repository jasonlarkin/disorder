  subroutine setfeshtmat(mcv,msv,derv2,dervi,eigr,eigi,maxd2)
!
!  Sets up the shell transformation matrices used in a free energy minimisation.
!
!   8/97 Created 
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
  integer(i4)        :: maxd2
  integer(i4)        :: mcv
  integer(i4)        :: msv
  real(dp)           :: derv2(maxd2,*)
  real(dp)           :: dervi(maxd2,*)
  real(dp)           :: eigi(maxd2,*)
  real(dp)           :: eigr(maxd2,*)
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: j
  integer(i4)        :: k
!
!  Generate Pns from Msc and eigenvectors
!
  do i = 1,mcv
    do j = 1,msv
      derv2(i,mcv+j) = 0.0_dp
      dervi(i,mcv+j) = 0.0_dp
      do k = 1,mcv
        derv2(i,mcv+j) = derv2(i,mcv+j) + eigr(k,i)*derv2(mcv+j,k)
        derv2(i,mcv+j) = derv2(i,mcv+j) - eigi(k,i)*dervi(mcv+j,k)
        dervi(i,mcv+j) = dervi(i,mcv+j) + eigi(k,i)*derv2(mcv+j,k)
        dervi(i,mcv+j) = dervi(i,mcv+j) + eigr(k,i)*dervi(mcv+j,k)
      enddo
    enddo
  enddo
!
  return
  end
