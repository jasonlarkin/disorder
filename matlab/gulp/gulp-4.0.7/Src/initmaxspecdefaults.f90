  subroutine initmaxspecdefaults(i)
!
!  Initialises the arrays associated with maxspec
!
!   9/10 Created from changemax routine
!   9/12 NMR properties added
!  12/12 NMR f value added
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, December 2012
!
  use library
  use polarise
  use montecarlo
  use shell
  use species
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Initialise defaults for new part of array
!
  if (i.ge.1.and.i.le.maxspec) then
    libspec(i) = ' '
    lbrspec(i) = .false.
    ldefshspec(i) = .false.
    linspec(i) = .false.
    lmassinspec(i) = .false.
    lnmrinspec(i) = .false.
    lqinspec(i) = .false.
    lmask(i) = .false.
    massspec(i) = 0.0_dp
    nmrspec(1,i) = 0.0_dp
    nmrspec(2,i) = 0.0_dp
    nmrspec(3,i) = 1.0_dp
    radspec(i) = 0.0_dp
    natpolspec(i) = 0
    natratiomspec(i) = 0
    dpolspec(i) = 0.0_dp
    qpolspec(i) = 0.0_dp
    ratiomspec(i) = 0.0_dp
  endif
!
  return
  end
