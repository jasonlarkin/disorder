  subroutine symdervrot(k1,k2,deriv)
!
!  Rotates the derivative acting on one atom into the symmetry related derivative acting on the other.
!
!   4/09 Created
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
!  Copyright Curtin University 2009
!
!  Julian Gale, Curtin University, April 2009
!
  use current,     only : nrotop
  use eam,         only : maxmeamcomponent
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: k1
  integer(i4), intent(in)    :: k2
  real(dp),    intent(inout) :: deriv(3)
!
!  Local variables
!
  integer(i4)                :: ifail
  integer(i4)                :: j
  integer(i4)                :: k
  integer(i4)                :: mv1
  integer(i4)                :: mv2
  real(dp)                   :: wrk(6)
  real(dp)                   :: x(3)
  real(dp)                   :: xf(3)
  real(dp)                   :: x2(3,3)
!
!  If k1 = k2 there is nothing to do so return
!
  if (k1.eq.k2) return
!
!  Find rotational symmetry operators for atoms k1 and k2
!
  mv1 = nrotop(k1)
  mv2 = nrotop(k2)
!
!  Find inverse of first symmetry operator
!
  x2(1:3,1:3) = rop(1:3,1:3,mv1) 
  call matinv(x2,3_i4,3_i4,wrk,ifail)
!
!  Check there wasn't a problem
!
  if (ifail.ne.0) then
    call outerror('inversion of rotational operator failed',0_i4)
    call stopnow('symdervrot')
  endif
!
!  Multiply by rotational inverse
!
  x(1) = deriv(1)
  x(2) = deriv(2)
  x(3) = deriv(3)
  do j = 1,3
    xf(j) = 0.0_dp
    do k = 1,3
      xf(j) = xf(j) + x2(j,k)*x(k)
    enddo
  enddo
!
!  Multiply by forward rotation of second atom
!
  do j = 1,3
    deriv(j) = 0.0_dp
    do k = 1,3
      deriv(j) = deriv(j) + rop(j,k,mv2)*xf(k)
    enddo
  enddo
!
  return
  end
