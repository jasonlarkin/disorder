  subroutine wolfselftrial(ewolfself,ntrialatom,nptrtrialatom)
!
!  Subroutine for calculating the energy due to the self term in the Wolf sum for
!  a subset of the atoms
!
!   1/08 Created from wolfself
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use constants,      only : angstoev
  use control,        only : lwolf
  use current
  use general,        only : cutw, selfwolf
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: ntrialatom
  integer(i4), intent(in)  :: nptrtrialatom(ntrialatom)
  real(dp), intent(out)    :: ewolfself
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: nt
  real(dp)                 :: q2sum
  real(dp)                 :: qi
!
!  Initialise energy
!
  ewolfself = 0.0_dp
!
!  If this is not a Wolf sum return
!
  if (.not.lwolf.or.cutw.lt.1.0d-8) return
!
!  Loop over asymmetric unit summing the square of the charges
!
  q2sum = 0.0_dp
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    qi = qf(i)*occuf(i)
    q2sum = q2sum + qi*qi
  enddo
!
!  Calculate energy
!
  ewolfself = - 0.5_dp*angstoev*q2sum*selfwolf
!
  return
  end
