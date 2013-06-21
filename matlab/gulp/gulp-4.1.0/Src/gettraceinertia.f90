  subroutine gettraceinertia(trace)
!
!  Calculate the trace of the moment of inertia tensor
!
!   8/06 Created from property0
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, July 2007
!
  use current
  use element
  use iochannels
  use parallel
  use shell,     only : ncore
  use species,   only : massspec
  implicit none
!
!  Passed variables
!
  real(dp),            intent(out) :: trace
!
!  Local variables
!
  integer(i4)                      :: i
  real(dp)                         :: dx
  real(dp)                         :: dy
  real(dp)                         :: dz
  real(dp)                         :: mi
  real(dp)                         :: totalmass
  real(dp)                         :: xcom
  real(dp)                         :: ycom
  real(dp)                         :: zcom
!*****************************
!  Moment of inertia tensor  *
!*****************************
!
!  Initialise trace
!
  trace = 0.0_dp
!
!  Find centre of mass
!
  xcom = 0.0_dp
  ycom = 0.0_dp
  zcom = 0.0_dp
  totalmass = 0.0_dp
  do i = 1,ncore
    mi = occuf(i)*massspec(nspecptr(i))
    totalmass = totalmass + mi
    xcom = xcom + xclat(i)*mi
    ycom = ycom + yclat(i)*mi
    zcom = zcom + zclat(i)*mi
  enddo
  xcom = xcom/totalmass
  ycom = ycom/totalmass
  zcom = zcom/totalmass
!
!  Calculate tensor
!
  do i = 1,ncore
    mi = occuf(i)*massspec(nspecptr(i))
    dx = xclat(i) - xcom
    dy = yclat(i) - ycom
    dz = zclat(i) - zcom
    trace = trace + mi*(dx*dx + dy*dy + dz*dz)
  enddo
!
  return
  end
