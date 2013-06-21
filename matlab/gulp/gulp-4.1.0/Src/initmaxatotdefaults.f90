  subroutine initmaxatotdefaults(i)
!
!  Initialises the arrays associated with maxatot
!
!   9/10 Created from changemaxatot
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
  use configurations
  use moldyn,        only : lfix
  use scan,          only : ltranat
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Initialise new parts of data arrays
!
  if (i.ge.1.and.i.le.maxatot) then
    lbsmat(i) = .false.
    leinsteinat(i) = .false.
    lfix(i) = .false.
    lopfi(3*i-2:3*i) = .false.
    ltdforcecfg(1:3,i) = .false.
    lqmatom(i) = .false.
    lsliceatom(i) = .false.
    ltranat(i) = .false.
    nregionno(i) = 1
    forcecfg(1:3,i) = 0.0_dp
    tdforcecfg(1:3,1:3,i) = 0.0_dp
    keinsteinat(i) = 0.0_dp
    xeinsteinat(i) = 0.0_dp
    yeinsteinat(i) = 0.0_dp
    zeinsteinat(i) = 0.0_dp
  endif
!
  return
  end
