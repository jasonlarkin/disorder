  subroutine scaleevec(mcv,eigr,maxd2,scale)
!
!  Scales the eigenvectors by a mode dependant constant
!
!   9/97 Created 
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
  real(dp)           :: eigr(maxd2,*)
  real(dp)           :: scale(*)
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: j
  real(dp)           :: rscale
!
!  Scale by mode
!
  do i = 1,mcv
    rscale = scale(i)
    do j = 1,mcv
      eigr(j,i) = rscale*eigr(j,i)
    enddo
  enddo
!
  return
  end
