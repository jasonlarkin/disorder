!****************
!  3-D systems  *
!****************
  subroutine strain3D(xstr,rv)
!
!  Applies strain to original unit cell to create new unit cell
!
!  11/04 Intent added
!  11/06 Order of looping improved
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),   intent(inout) :: rv(3,3)
  real(dp),   intent(in)    :: xstr(*)
!
!  Local variables
!
  integer(i4)               :: i
  integer(i4)               :: istr1
  integer(i4)               :: istr2
  integer(i4)               :: j
  integer(i4)               :: k
  integer(i4)               :: m
  integer(i4)               :: nstrind(6)
  real(dp)                  :: rv2(3,3)
  real(dp)                  :: strmat(3,3)
!
  data (nstrind(i),i=1,6)/2,3,1,3,1,2/
  do j = 1,3
    rv2(1,j) = rv(1,j)
    rv2(2,j) = rv(2,j)
    rv2(3,j) = rv(3,j)
    rv(1,j) = 0.0_dp
    rv(2,j) = 0.0_dp
    rv(3,j) = 0.0_dp
  enddo
!
!  Build strain matrix
!
  strmat(1,1) = 1.0_dp + xstr(1)
  strmat(2,2) = 1.0_dp + xstr(2)
  strmat(3,3) = 1.0_dp + xstr(3)
  do i = 4,6
    m = (i-4)*2 + 1
    istr1 = nstrind(m)
    istr2 = nstrind(m+1)
    strmat(istr1,istr2) = 0.5_dp*xstr(i)
    strmat(istr2,istr1) = 0.5_dp*xstr(i)
  enddo
!
!  Apply strain
!
  do j = 1,3
    do k = 1,3
      rv(k,j) = rv(k,j) + strmat(k,1)*rv2(1,j) + strmat(k,2)*rv2(2,j) + strmat(k,3)*rv2(3,j)
    enddo
  enddo
!
  return
  end
!****************
!  2-D systems  *
!****************
  subroutine strain2D(xstr,rv)
!
!  Applies strain to original unit cell to create new unit cell
!
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
  use datatypes
  implicit none
!
!  Passed variables
!  
  real(dp),   intent(inout) :: rv(3,3)
  real(dp),   intent(in)    :: xstr(*)
!
!  Local variables
!
  integer(i4)               :: j
  integer(i4)               :: k
  real(dp)                  :: rv2(3,3)
  real(dp)                  :: strmat(2,2)
!
  do j = 1,2
    rv2(1,j) = rv(1,j)
    rv2(2,j) = rv(2,j)
    rv(1,j) = 0.0_dp
    rv(2,j) = 0.0_dp
  enddo
!
!  Build strain matrix
!
  strmat(1,1) = 1.0_dp + xstr(1)
  strmat(2,2) = 1.0_dp + xstr(2)
  strmat(1,2) = 0.5_dp*xstr(3)
  strmat(2,1) = 0.5_dp*xstr(3)
!
!  Apply strain
!
  do j = 1,2
    do k = 1,2
      rv(k,j) = rv(k,j) + rv2(1,j)*strmat(k,1) + rv2(2,j)*strmat(k,2)
    enddo
  enddo
!
  return
  end
!****************
!  1-D systems  *
!****************
  subroutine strain1D(xstr,rv)
!
!  Applies strain to original unit cell to create new unit cell
!
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
  use datatypes
  implicit none
!
!  Passed variables
! 
  real(dp),   intent(inout) :: rv(3,3)
  real(dp),   intent(in)    :: xstr(*)
!
!  Local variables
!
  real(dp)                  :: strmat
!
!  Apply strain
!
  strmat = 1.0_dp + xstr(1)
  rv(1,1) = rv(1,1)*strmat
!
  return
  end
