  subroutine setratiom
!
!  Set up shell mass ratio for MD
!
!   7/05 created
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use current
  use element, only : maxele
  use mdlogic, only : ladiabatic
  use parallel
  use shell
  implicit none
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: j
  integer(i4)      :: natp
  integer(i4)      :: ntypp
!
!  Initialise mass ratios to zero 
!
  do i = 1,numat
    if (nat(i).le.maxele) then
      ratiom(i) = 1.0_dp
    else
      ratiom(i) = 0.0_dp
    endif
  enddo
!
!  Loop over mass ratio species assigning to atoms
!
  do i = 1,nratiomspec
    natp = natratiomspec(i)
    ntypp = ntypratiomspec(i)
    do j = 1,numat
      if (nat(j).gt.maxele) then
!
!  Shells only
!
        if (nat(j)-maxele.eq.natp) then
          if (ntypp.eq.0.or.ntypp.eq.nftype(j)) then
            ratiom(j) = ratiomspec(i)
            ratiom(ncsptr(j)) = 1.0_dp - ratiom(j)
          endif
        endif
      endif
    enddo
  enddo
!
!  Check for a particle with a zero mass ratio
!
  if (.not.ladiabatic) then
    do i = 1,numat
      if (ratiom(i).lt.1.0d-12) then
        call outerror('particle has a zero mass in molecular dynamics',0_i4)
        call stopnow('setratiom')
      endif
    enddo
  endif
!
  return
  end
