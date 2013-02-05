  subroutine ctfunct(iflag,n,xc,fc,gc,maxj,jmat,fij,chivec)
!
!  Computes energy and first derivatives of charge transfer function
!
!   4/10 Created 
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
!  Julian Gale, NRI, Curtin University, April 2010
!
  use iochannels
  use parallel

  implicit none
!
!  Passed variables
!
  integer(i4),          intent(in)            :: iflag
  integer(i4),          intent(in)            :: n
  integer(i4),          intent(in)            :: maxj
  real(dp),             intent(out)           :: fc
  real(dp),             intent(out)           :: gc(n)
  real(dp),             intent(in)            :: xc(n)
  real(dp),             intent(in)            :: chivec(n)
  real(dp),             intent(in)            :: jmat(maxj,*)
  real(dp),             intent(in)            :: fij(maxj,*)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: j
  real(dp)                                    :: Jkl
  real(dp)                                    :: rn
  real(dp)                                    :: sumgc
  real(dp)                                    :: term
!
  rn = 1.0_dp/dble(n)
!
!  Initialise function
!
  fc = 0.0_dp
  gc(1:n) = 0.0_dp
!
!  Compute energy and charge derivatives
!
  do i = 1,n
    do j = 1,n
      term = rn*(chivec(i) - chivec(j))*fij(j,i)
      fc = fc + term*xc(i)
      gc(i) = gc(i) + term
    enddo
  enddo
  do i = 1,n
    Jkl = 0.0_dp
    do j = 1,n
      Jkl = Jkl + xc(j)*jmat(j,i)
    enddo
    fc = fc + 0.5_dp*xc(i)*Jkl
    gc(i) = gc(i) + Jkl
  enddo
!
!  Ensure there that the of the derivatives with respect to charge is zero
!
  sumgc = 0.0_dp
  do i = 1,n
    sumgc = sumgc + gc(i) 
  enddo
  sumgc = - sumgc/dble(n)
  do i = 1,n
    gc(i) = gc(i) + sumgc
  enddo
!
  return
  end
