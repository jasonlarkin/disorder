  subroutine initmaxthbdefaults(i)
!
!  Initialises the arrays associated with maxthb
!
!   9/10 Created from changemax routine
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use m_three
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Initialise defaults for new part of array
!
  if (i.ge.1.and.i.le.maxthb) then
    lgenerated3(i) = .false.
    lthetataper(i) = .false.
    ltdreiding(i) = .false.
    mmtexc(i) = 0
    n3botype(1,1:2,i) = 0
    n3botype(2,1:2,i) = 1
    n3bondno(1:maxn3bondnono,1:2,i) = 0
    n3bondnono(1:2,i) = 0
    thrho1(i) = 0.0_dp
    thrho2(i) = 0.0_dp
    thrho3(i) = 0.0_dp
    thr1min(i) = 0.0_dp
    thr2min(i) = 0.0_dp
    thr3min(i) = 0.0_dp
  endif
!
  return
  end
